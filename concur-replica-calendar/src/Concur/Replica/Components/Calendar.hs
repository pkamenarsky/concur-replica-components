{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Concur.Replica.Components.Calendar where

import           Concur.Core hiding (display)
import           Concur.Core.Types (liftUnsafeBlockingIO, liftSafeBlockingIO)
import           Concur.Replica hiding (col)

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad (forever)
import           Control.Monad.Trans.Cont
import           Control.ShiftMap

import           Data.IORef
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time

import           Replica.VDOM
import           Replica.VDOM.Types

import qualified Concur.Replica.Components.Grid as Grid
import           Concur.Replica.Components.Grid hiding (cell, cell_, Cell, Grid, Style)

import           Prelude hiding (div)

import           Debug.Trace

--------------------------------------------------------------------------------

data Δ a = Value (TVar a) (TChan a) deriving Eq

data Lens s t

mapValue :: Lens s t -> Δ t -> Δ s
mapValue = undefined

pairValues :: Lens a t -> Lens b t -> Δ a -> Δ b -> Δ t
pairValues = undefined

localIO :: a -> IO (Δ a)
localIO a = atomically $ liftA2 Value (newTVar a) newBroadcastTChan

local :: a -> (Δ a -> Widget HTML b) -> Widget HTML b
local a f = do
  v <- liftUnsafeBlockingIO
    $ atomically
    $ liftA2 Value (newTVar a) newBroadcastTChan
  f v

put :: Δ a -> a -> Widget HTML ()
put (Value ref bcast) a = liftUnsafeBlockingIO $ atomically $ do
  writeTVar ref a
  writeTChan bcast a

observe :: Δ a -> (a -> Widget HTML r) -> Widget HTML r
observe (Value ref bcast) w = do
  (a, read) <- liftUnsafeBlockingIO
    $ atomically
    $ liftA2 (,) (readTVar ref) (dupTChan bcast)
  go read a
  where
    go read a = do
      r <- fmap Left (w a) <|> fmap Right (get read)
      case r of
        Right a' -> go read a'
        Left r   -> pure r

    get read = liftSafeBlockingIO $ atomically $ readTChan read

with :: Δ a -> ((a -> Widget HTML r) -> a -> Widget HTML r) -> Widget HTML r
with (Value ref bcast) w = do
  (a, read) <- liftUnsafeBlockingIO
    $ atomically
    $ liftA2 (,) (readTVar ref) (dupTChan bcast)
  go read a
  where
    recur read a = w (\a' -> write read a' >>= recur read) a

    go read a = do
      r <- fmap Left (recur read a) <|> fmap Right (get read)
      case r of
        Right a' -> go read a'
        Left r'  -> pure r'

    get read = liftSafeBlockingIO $ atomically $ readTChan read

    write read a = liftUnsafeBlockingIO $ atomically $ do
      writeTVar ref a
      writeTChan bcast a
      readTChan read

data Step a b = Recur a | Done b

recur :: a -> Step a b
recur  = Recur

done :: b -> Step a b
done  = Done

withE :: Δ a -> (a -> Widget HTML (Step a r)) -> Widget HTML r
withE (Value ref bcast) w = do
  (a, read) <- liftUnsafeBlockingIO
    $ atomically
    $ liftA2 (,) (readTVar ref) (dupTChan bcast)
  go read a
  where
    go read a = do
      r <- fmap Left (w a) <|> fmap Right (get read)
      case r of
        Right a' -> go read a'
        Left (Recur a') -> do
          write read a'
          go read a'
        Left (Done b) -> pure b

    get read = liftSafeBlockingIO $ atomically $ readTChan read

    write read a = liftUnsafeBlockingIO $ atomically $ do
      writeTVar ref a
      writeTChan bcast a
      readTChan read

withE2 :: Δ a -> Δ b -> (a -> b -> Widget HTML (Step (a, b) r)) -> Widget HTML r
withE2 (Value refa bcasta) (Value refb bcastb) w = do
  (a, reada, b, readb) <- liftUnsafeBlockingIO
    $ atomically
    $ (,,,) <$> readTVar refa <*> dupTChan bcasta <*> readTVar refb <*> dupTChan bcastb
  go reada a readb b
  where
    go reada a readb b = do
      r <- fmap Left (w a b) <|> fmap Right (get reada a readb b)
      case r of
        Right (a', b') -> go reada a' readb b'
        Left (Recur (a', b')) -> do
          liftUnsafeBlockingIO $ atomically $ do
            write refa bcasta reada a'
            write refb bcastb readb b'
          go reada a' readb b'
        Left (Done r) -> pure r

    get reada a readb b = liftSafeBlockingIO $ atomically $ do
      r <- fmap Left (readTChan reada) <|> fmap Right (readTChan readb)
      case r of
        Left a'  -> (a',) . fromMaybe b <$> tryReadTChan readb
        Right b' -> (,b') . fromMaybe a <$> tryReadTChan reada

    write ref bcast read a = do
      writeTVar ref a
      writeTChan bcast a
      readTChan read

--------------------------------------------------------------------------------

type Style = [(T.Text, T.Text)]

(.=) = (,)

color :: T.Text -> Props a
color color = style [("color", color)]

backgroundColor :: T.Text -> Props a
backgroundColor color = style [("backgroundColor", color)]

textAlignX :: T.Text -> Props a
textAlignX align = style [("text-align", align)]

textAlignY :: T.Text -> Props a
textAlignY align = style [("vertical-align", align)]

--------------------------------------------------------------------------------

type Year  = Integer
type Month = Int

data CalendarAction = SelectDay (Maybe Time.Day) | NextMonth | PrevMonth

data CalendarState = CalendarState
  { csYear :: Year
  , csMonth :: Month
  , csSelectedDay :: Maybe Time.Day
  } deriving Show

calendarSt2 :: Δ Month -> Δ (Maybe Time.Day) -> Widget HTML r
calendarSt2 m d = withE2 m d go
  where
    csYear = 2019
    go csMonth csSelectedDay = do
      r <- calendarGrid

      case r of
        PrevMonth     -> pure $ recur (csMonth - 1, csSelectedDay)
        NextMonth     -> pure $ recur (csMonth + 1, csSelectedDay)
        SelectDay day -> do
          -- div [ onClick ] [ text $ "Clicked: " <> T.pack (show day) ]
          pure $ recur (csMonth, day)
      where
        date = Time.fromGregorian csYear csMonth 1
        dayCount = Time.gregorianMonthLength csYear csMonth
        (_, _, firstDay) = Time.toWeekDate date

        calendarGrid = grid [ rowGap (Px 20) ] (Only $ Px 500) (MaxContent, Px 300)
          [ ( Only $ headerGrid
            , Only $ monthGrid
            )
          ]

        headerGrid = row [] (MaxContent, Fr 1, MaxContent, Px 20, MaxContent)
          ( div [] [ text $ T.pack (show csMonth) ]
          , one div [] (div [ centeredX ] [ text "Calendar" ])
          , div [ PrevMonth <$ onClick ] [ text "<" ]
          , div [] []
          , div [ NextMonth <$ onClick ] [ text ">" ]
          )

        monthGrid = unsafeGrid
          [ gaps (Px 1) (Px 1) ]
          [ Fr 1 | _ <- [0..6] ]
          [ Fr 1 | _ <- [0..5] ]
          [ [ [ one div
                  [ mconcat $ if Just dayDate == csSelectedDay
                      then [ backgroundColor "#333", color "#fff" ]
                      else [ backgroundColor "#777" ]
                  , SelectDay (Just dayDate) <$ onClick
                  ]
                  (div [ centered ] [ text (T.pack $ if dayNr >= 1 && dayNr <= dayCount then show dayNr else "") ])
              | day <- [0..6]
              , let dayNr = week * 7 + day + 2 - firstDay
                    dayDate = fromIntegral (dayNr - 1) `Time.addDays` date
              ]
            | week <- [0..5]
            ]
          ]

calendarSt :: Δ CalendarState -> Widget HTML r
calendarSt = flip withE go
  where
    go st@(CalendarState {..}) = do
      r <- calendarGrid

      case r of
        PrevMonth     -> pure $ recur $ st { csMonth = csMonth - 1 }
        NextMonth     -> pure $ recur $ st { csMonth = csMonth + 1 }
        SelectDay day -> do
          -- div [ onClick ] [ text $ "Clicked: " <> T.pack (show day) ]
          pure $ recur $ st { csSelectedDay = day }
      where
        date = Time.fromGregorian csYear csMonth 1
        dayCount = Time.gregorianMonthLength csYear csMonth
        (_, _, firstDay) = Time.toWeekDate date

        calendarGrid = grid [ rowGap (Px 20) ] (Only $ Px 500) (MaxContent, Px 300)
          [ ( Only $ headerGrid
            , Only $ monthGrid
            )
          ]

        headerGrid = row [] (MaxContent, Fr 1, MaxContent, Px 20, MaxContent)
          ( div [] [ text $ T.pack (show csMonth) ]
          , one div [] (div [ centeredX ] [ text "Calendar" ])
          , div [ PrevMonth <$ onClick ] [ text "<" ]
          , div [] []
          , div [ NextMonth <$ onClick ] [ text ">" ]
          )

        monthGrid = unsafeGrid
          [ gaps (Px 1) (Px 1) ]
          [ Fr 1 | _ <- [0..6] ]
          [ Fr 1 | _ <- [0..5] ]
          [ [ [ one div
                  [ mconcat $ if Just dayDate == csSelectedDay
                      then [ backgroundColor "#333", color "#fff" ]
                      else [ backgroundColor "#777" ]
                  , SelectDay (Just dayDate) <$ onClick
                  ]
                  (div [ centered ] [ text (T.pack $ if dayNr >= 1 && dayNr <= dayCount then show dayNr else "") ])
              | day <- [0..6]
              , let dayNr = week * 7 + day + 2 - firstDay
                    dayDate = fromIntegral (dayNr - 1) `Time.addDays` date
              ]
            | week <- [0..5]
            ]
          ]

calendar :: Year -> Month -> Maybe Time.Day -> Widget HTML a
calendar year month selectedDay = do
  (day, week) <- calendarGrid

  div [ onClick ] [ text $ "Clicked: " <> T.pack (show day) <> ", " <> T.pack (show week) ]

  calendar year month selectedDay
  where
    day = Time.fromGregorian year month 1
    dayCount = Time.gregorianMonthLength year month
    (_, _, firstDay) = Time.toWeekDate day

    calendarGrid = grid [ rowGap (Px 20) ] (Only $ Px 500) (MaxContent, Px 300)
      [ ( Only headerGrid
        , Only monthGrid
        )
      ]

    headerGrid = row [] (MaxContent, Fr 1, MaxContent)
      ( div [] [ text "July" ]
      , one div [] (div [ centeredX ] [ text "Calendar" ])
      , div [] [ text "2019" ]
      )

    monthGrid = unsafeGrid
      [ gaps (Px 1) (Px 1) ]
      [ Fr 1 | _ <- [1..7] ]
      [ Fr 1 | _ <- [1..5] ]
      [ [ [ one div
              [ backgroundColor "#777"
              , (day, week) <$ onClick
              ]
              (div [ centered ] [ text (T.pack (show day) <> ", " <> T.pack (show week)) ])
          | day <- [1..7]
          ]
        | week <- [1..5]
        ]
      ]

testCal1 :: Widget HTML a
testCal1 = grid [] (Fr 1, Fr 1) (Px 60, Px 120)
  [ ( ( div [ backgroundColor "#333" ] [], div [ backgroundColor "#c00" ] [] )
    , ( div [ backgroundColor "#0c0" ] [], div [ backgroundColor "#00c" ] [] )
    )
  ]

testCal2 :: Widget HTML a
testCal2 = dynRows [] (Px 50) (Fr 1, Fr 1)
  [ ( div [ backgroundColor "#333" ] [], div [ backgroundColor "#c00" ] [] )
  , ( div [ backgroundColor "#0c0" ] [], div [ backgroundColor "#00c" ] [] )
  , ( div [ backgroundColor "#333" ] [], div [ backgroundColor "#c00" ] [] )
  , ( div [ backgroundColor "#0c0" ] [], div [ backgroundColor "#00c" ] [] )
  ]

testCalendar = runDefault 3456 "Calendar" (calendar 2019 7 Nothing)

testCalendar2 = do
  st  <- localIO (CalendarState 2019 9 Nothing)
  st2 <- localIO (CalendarState 2019 9 Nothing)
  runDefault 3456 "Calendar" $ div []
    [ calendarSt st
    , observe st $ \st -> observe st2 $ \st2 -> div []
        [ text (T.pack $ show st)
        , text (T.pack $ show st2)
        ]
    ]

testCalendar3 = do
  m1 <- localIO 9
  m2 <- localIO 9
  sd <- localIO Nothing
  runDefault 3456 "Calendar" $ div []
    [ calendarSt2 m1 sd
    , calendarSt2 m2 sd
    , observe m1 $ \m1 -> observe m2 $ \m2 -> observe sd $ \sd -> div []
        [ text (T.pack $ show m1)
        , text (T.pack $ show m2)
        , text (T.pack $ show sd)
        ]
    ]

--------------------------------------------------------------------------------

counter :: (Int -> Widget HTML r) -> Int -> Widget HTML r
counter recur x = do
  div [ onClick ] [ text (T.pack $ show x) ]
  recur (x + 1)

counterE ::Int -> Widget HTML (Step Int r)
counterE x = do
  div [ onClick ] [ text (T.pack $ show x) ]
  pure $ recur (x + 1)

counters = local 0 $ \x -> div []
  [ withE x counterE
  , withE x counterE
  ]

input' :: (T.Text -> Widget HTML r) -> T.Text -> Widget HTML r
input' recur str = do
  e <- div [] [ input [ value str, onInput ] ]
  recur (targetValue $ target e)

inputOnEnter :: (T.Text -> Widget HTML T.Text) -> T.Text -> Widget HTML T.Text
inputOnEnter recur str = do
  e <- div [] [ input [ value str, onKeyDown ] ]
  if kbdKey e == "Enter"
    then pure  (targetValue $ target $ kbdBaseEvent e)
    else recur (targetValue $ target $ kbdBaseEvent e)

testLocalW x y = div []
  -- [ with x counter, with x counter, with x counter
  -- , with y counter, with y counter, with y counter, with y counter
  -- ]
  [ with x input', with x input'
  , with y input', with y input'
  ]
  
testLocalW2 x = do
  s <- div [] [ with x inputOnEnter ]
  div [] [ text s ]

testLocal = do
  x <- localIO ""
  y <- localIO ""
  runDefault 3456 "Local" (testLocalW2 x)

-- Generics --------------------------------------------------------------------

data U
data R

type family Ref a b where
  Ref U a = Δ a
  Ref R a = a

data CalendarStateRef ref = CalendarStateRef
  { csrYear        :: Ref ref Year
  , csrMonth       :: Ref ref Month
  , csrSelectedDay :: Ref ref (Maybe Time.Day)
  }

toR :: a U -> Δ (a R)
toR = undefined
