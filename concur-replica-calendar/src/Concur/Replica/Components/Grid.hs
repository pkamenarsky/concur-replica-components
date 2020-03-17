{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Concur.Replica.Components.Grid where

import           Concur.Core.Types
import           Concur.Replica hiding (col)

import Control.Monad hiding ((>>=))

import Data.List
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits

import Prelude hiding (div, (>>=))

import Debug.Trace

--------------------------------------------------------------------------------

appendAttrs :: Attrs -> Widget HTML a -> Widget HTML a
appendAttrs attrs = mapView (fmap f)
  where
    f :: VDOM -> VDOM
    f (VLeaf e nattrs)     = VLeaf e (nattrs <> attrs)
    f (VNode e nattrs chs) = VNode e (nattrs <> attrs) chs
    f e                    = e

data AutoFlow = Col | Row | Dense
data DynGrow  = DynRows Length | DynCols Length | Static

data Length = Px Int | Pct Int | Pt Double | Fr Double | Auto | MinContent | MaxContent

type Width  = Length
type Height = Length

data Cell a b = Cell a b deriving (Show, Functor)

type Style = [(Text, Text)]

lengthToCss :: Length -> Text
lengthToCss (Px px)    = T.pack (show px) <> "px"
lengthToCss (Pct px)   = T.pack (show px) <> "%"
lengthToCss (Pt px)    = T.pack (show px) <> "pt"
lengthToCss (Fr px)    = T.pack (show px) <> "fr"
lengthToCss Auto       = "auto"
lengthToCss MinContent = "min-content"
lengthToCss MaxContent = "max-content"

unsafeTaggedGrid'
  :: DynGrow
  -> [Props a]
  -> [Width]
  -> [Height]
  -> [[[Cell (Maybe Text) (Widget HTML a)]]]
  -> Widget HTML a
unsafeTaggedGrid' dynGrow props coldef rowdef cells = div
  (props <> [gridCss])
  [ appendAttrs (style cellCss) e | Cell cellCss e <- cellsCss ]
  where
    gridCss = style $
      [ ("display",             "grid")
      , ("gridTemplateColumns", T.intercalate " " $ map lengthToCss coldef)
      , ("gridTemplateRows",    T.intercalate " " $ map lengthToCss rowdef)
      ]
      <> case dynGrow of
           DynRows l -> [("gridAutoRows",    lengthToCss l)]
           DynCols l -> [("gridAutoColumns", lengthToCss l)]
           Static    -> []

    cellsCss = fmap rectToCell rects

    rects = concat $ fmap (fmap toRel . rectsBy eqCell) cells
      
    eqCell :: Eq t => Cell (Maybe t) e -> Cell (Maybe t) e -> Bool
    Cell Nothing _  `eqCell` Cell _ _        = False
    Cell _ _        `eqCell` Cell Nothing _  = False
    Cell (Just a) _ `eqCell` Cell (Just b) _ = a == b

    rectToCss :: Rect e -> Style
    rectToCss (e, P x, P y, L w, L h) = mconcat
      [ case dynGrow of
          DynCols _ -> [ ("gridRow",     T.pack (show $ y + 1) <> "/span " <> T.pack (show h)) ]
          DynRows _ -> [ ("gridColumn",  T.pack (show $ x + 1) <> "/span " <> T.pack (show w)) ]
          Static    -> [ ("gridRow",     T.pack (show $ y + 1) <> "/span " <> T.pack (show h))
                       , ("gridColumn",  T.pack (show $ x + 1) <> "/span " <> T.pack (show w))
                       ]
      ]

    rectToCell :: Rect (Cell (Maybe t) e) -> Cell Style e
    rectToCell r@(Cell _ e, _, _, _, _) = Cell (rectToCss r) e

unsafeTaggedGrid
  :: [Props a]
  -> [Width]
  -> [Height]
  -> [[[Cell (Maybe Text) (Widget HTML a)]]]
  -> Widget HTML a
unsafeTaggedGrid = unsafeTaggedGrid' Static

unsafeGrid
  :: [Props a]
  -> [Width]
  -> [Height]
  -> [[[Widget HTML a]]]
  -> Widget HTML a
unsafeGrid props coldef rowdef
  = unsafeTaggedGrid' Static props coldef rowdef . fmap (fmap (fmap untagged))

taggedRow
  :: ToVec rowdef (Vec x Length)
  => ToVec row (Vec x (Cell (Maybe Text) (Widget HTML a)))
  => [Props a]
  -> rowdef
  -> row
  -> Widget HTML a
taggedRow props rowdef row = unsafeTaggedGrid
  props
  (toList $ toVec rowdef)
  []
  [[toList $ toVec row]]

row
  :: ToVec rowdef (Vec x Length)
  => ToVec row (Vec x (Widget HTML a))
  => [Props a]
  -> rowdef
  -> row
  -> Widget HTML a
row props rowdef row = unsafeTaggedGrid
  props
  (toList $ toVec rowdef)
  []
  [[fmap untagged $ toList $ toVec row]]

taggedDynRows
  :: ToVec rowdef (Vec x Length)
  => ToVec row (Vec x (Cell (Maybe Text) (Widget HTML a)))
  => [Props a]
  -> Height
  -> rowdef
  -> [row]
  -> Widget HTML a
taggedDynRows props height rowdef row = unsafeTaggedGrid'
  (DynRows height)
  props
  (toList $ toVec rowdef)
  []
  [fmap (toList . toVec) row]

dynRows
  :: ToVec rowdef (Vec x Length)
  => ToVec row (Vec x (Widget HTML a))
  => [Props a]
  -> Height
  -> rowdef
  -> [row]
  -> Widget HTML a
dynRows props height rowdef row = unsafeTaggedGrid'
  (DynRows height)
  props
  (toList $ toVec rowdef)
  []
  [fmap (fmap untagged . toList . toVec) row]


taggedCol
  :: ToVec coldef (Vec y Length)
  => ToVec col (Vec y (Cell (Maybe Text) (Widget HTML a)))
  => [Props a]
  -> coldef
  -> col
  -> Widget HTML a
taggedCol props coldef col = unsafeTaggedGrid
  props
  []
  (toList $ toVec coldef)
  [[toList $ toVec col]]

col
  :: ToVec coldef (Vec y Length)
  => ToVec col (Vec y (Widget HTML a))
  => [Props a]
  -> coldef
  -> col
  -> Widget HTML a
col props coldef col = unsafeTaggedGrid
  props
  []
  (toList $ toVec coldef)
  [[fmap untagged $ toList $ toVec col]]

taggedDynCols
  :: ToVec coldef (Vec y Length)
  => ToVec col (Vec y (Cell (Maybe Text) (Widget HTML a)))
  => [Props a]
  -> Width
  -> coldef
  -> [col]
  -> Widget HTML a
taggedDynCols props width coldef col = unsafeTaggedGrid'
  (DynCols width)
  props
  []
  (toList $ toVec coldef)
  [fmap (toList . toVec) col]

dynCols
  :: ToVec coldef (Vec y Length)
  => ToVec col (Vec y (Widget HTML a))
  => [Props a]
  -> Width
  -> coldef
  -> [col]
  -> Widget HTML a
dynCols props width coldef col = unsafeTaggedGrid'
  (DynCols width)
  props
  []
  (toList $ toVec coldef)
  [fmap (fmap untagged . toList . toVec) col]

taggedGrid
  :: ToVec coldef (Vec x Length)
  => ToVec rowdef (Vec y Length)
  => ToVec cells (Vec y row)
  => ToVec row (Vec x (Cell (Maybe Text) (Widget HTML a)))
  => [Props a]
  -> coldef
  -> rowdef
  -> [cells]
  -> Widget HTML a
taggedGrid props coldef rowdef cells = unsafeTaggedGrid
  props
  (toList $ toVec coldef)
  (toList $ toVec rowdef)
  (fmap (fmap (toList . toVec) . toList . toVec) cells)

grid
  :: ToVec coldef (Vec x Length)
  => ToVec rowdef (Vec y Length)
  => ToVec cells (Vec y row)
  => ToVec row (Vec x (Widget HTML a))
  => [Props a]
  -> coldef
  -> rowdef
  -> [cells]
  -> Widget HTML a
grid props coldef rowdef cells = unsafeTaggedGrid
  props
  (toList $ toVec coldef)
  (toList $ toVec rowdef)
  (fmap (fmap (fmap untagged . toList . toVec) . toList . toVec) cells)

one :: ([Props a] -> [Widget HTML a] -> Widget HTML a) -> [Props a] -> Widget HTML a -> Widget HTML a
one e props chs = e (props <> [style [("display", "grid")]]) [chs]

tagged :: t -> e -> Cell (Maybe t) e
tagged t e = Cell (Just t) e

untagged :: e -> Cell (Maybe t) e
untagged e = Cell Nothing e

-- Props -----------------------------------------------------------------------

centered :: Props a
centered = style [("justify-self", "center"), ("align-self", "center")]

left :: Props a
left = style [("justify-self", "start")]

centeredX :: Props a
centeredX = style [("justify-self", "center")]

right :: Props a
right = style [("justify-self", "end")]

stretchX :: Props a
stretchX = style [("justify-self", "stretch")]

top :: Props a
top  = style [("align-self", "start")]

centeredY :: Props a
centeredY = style [("align-self", "center")]

bottom :: Props a
bottom = style [("align-self", "end")]

stretchV :: Props a
stretchV = style [("align-self", "stretch")]

spaceAroundX :: Props a
spaceAroundX = style [("justify-content", "space-around")]

spaceBetweenX :: Props a
spaceBetweenX = style [("justify-content", "space-between")]

spaceEvenlyX :: Props a
spaceEvenlyX = style [("justify-content", "space-evenly")]

spaceAroundY :: Props a
spaceAroundY = style [("align-content", "space-around")]

spaceBetweenY :: Props a
spaceBetweenY = style [("align-content", "space-between")]

spaceEvenlyY :: Props a
spaceEvenlyY = style [("align-content", "space-evenly")]

colGap :: Width -> Props a
colGap w = style [("gridColumnGap", lengthToCss w)]

rowGap :: Height -> Props a
rowGap h = style [("gridRowGap", lengthToCss h)]

gaps :: Width -> Height -> Props a
gaps w h = style
  [ ("gridColumnGap", lengthToCss w)
  , ("gridRowGap", lengthToCss h)
  ]

--------------------------------------------------------------------------------

newtype Only a = Only a

newtype Vec (n :: Nat) a = Vec [a]

toList :: Vec n a -> [a]
toList (Vec as) = as

class ToVec a b | a -> b, b -> a where
  toVec :: a -> b

instance ToVec (Only a) (Vec 1 a) where
  toVec (Only a) = Vec [a]

instance ToVec (a, a) (Vec 2 a) where
  toVec (a, b) = Vec [a, b]

instance ToVec (a, a, a) (Vec 3 a) where
  toVec (a, b, c) = Vec [a, b, c]

instance ToVec (a, a, a, a) (Vec 4 a) where
  toVec (a, b, c, d) = Vec [a, b, c, d]

instance ToVec (a, a, a, a, a) (Vec 5 a) where
  toVec (a, b, c, d, e) = Vec [a, b, c, d, e]

instance ToVec (a, a, a, a, a, a) (Vec 6 a) where
  toVec (a, b, c, d, e, f) = Vec [a, b, c, d, e, f]

data Repeat (a :: Nat) b = Repeat b

-- instance (ToVec x (Vec n a), m * n ~ o) => ToVec (Repeat m x) (Vec o a) where

--------------------------------------------------------------------------------

data PP = X | Y | X2 | Y2

-- | Position
data P (t :: PP) a = P a deriving (Eq, Ord, Show)

data LP = W | H

-- | Length
data L (t :: LP) a = L a deriving (Eq, Ord, Show)

type Span a  = (a, P X Int, P X2 Int)
type RectA a = (a, P X Int, P Y Int, P X2 Int, P Y2 Int)
type Rect a  = (a, P X Int, P Y Int, L W Int, L H Int)

rectsBy :: forall a. (a -> a -> Bool) -> [[a]] -> [RectA a]
rectsBy cmp = go 0 []
  where
    rows :: [a] -> [Span a]
    rows = go 0 . groupBy cmp
      where
        go _ [] = []
        go p (a@(s:_):ss) = (s, P p, P (p + length a)):go (p + length a) ss

    go :: Int -> [Span (a, P Y Int)] -> [[a]] -> [RectA a]
    go y c [] = [ (ca, cx1, cy, cx2, P y) | ((ca, cy), cx1, cx2) <- c ]
    go y c (s:ss) = let (c', rs) = sprects y (rows s) c in rs <> go (y + 1) c' ss

    sprects :: Int -> [Span a] -> [Span (a, P Y Int)] -> ([Span (a, P Y Int)], [RectA a])
    sprects y oss [] = ([ ((sa, P y), sx1, sx2) | (sa, sx1, sx2) <- oss ], [])
    sprects y [] ocs = ([], [ (ca, cx1, cy, cx2, P y) | ((ca, cy), cx1, cx2) <- ocs ])
    sprects y oss@((sa, sx1, sx2):ss) ocs@(c@((ca, cy), cx1, cx2):cs)
      | sa `cmp` ca && sx1 == cx1 && sx2 == cx2 = let (rss, rrs) = sprects y ss cs in
          (c:rss, rrs)
      | cx1 <= sx1 = let (rss, rrs) = sprects y oss cs in
          (rss, (ca, cx1, cy, cx2, P y):rrs)
      | sx1 < cx1 = let (rss, rrs) = sprects y ss ocs in
          (((sa, P y), sx1, sx2):rss, rrs)

toRel :: RectA a -> Rect a
toRel (a, P x1, P y1, P x2, P y2) = (a, P x1, P y1, L (x2 - x1), L (y2 - y1))
