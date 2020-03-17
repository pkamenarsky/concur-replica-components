{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Concur.Core hiding (display)
import           Concur.Core.Types (liftUnsafeBlockingIO, liftSafeBlockingIO)
import           Concur.Replica hiding (col, id)
import           Concur.Replica.DOM (el)

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad (forever)
import           Control.Monad.Trans.Cont
import           Control.ShiftMap

import qualified Data.ByteString            as B
import qualified Data.FileEmbed             as FE
import           Data.IORef
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time

import           Replica.VDOM
import           Replica.VDOM.Types

import           Prelude hiding (div)

import           Network.WebSockets.Connection   (ConnectionOptions, defaultConnectionOptions)

import           Debug.Trace

import Lib

component :: B.ByteString
component = $(FE.embedFile "./js/dist/component.js")

leaflet :: [Attrs' a] -> [Widget HTML a] -> Widget HTML a
leaflet = el "leaflet"

app = do
  _ <- div [ onClick ] [ text "CLICK" ]
  leaflet [] [ div [] [ text "bla" ] ]

main :: IO ()
main = run 3456 (defaultIndex "BLA" header) defaultConnectionOptions app
  where
    fl = Attrs . M.fromList

    header =
      [ VNode "script" (fl [ ("src", AText "https://unpkg.com/leaflet@1.5.1/dist/leaflet.js"), ("crossorigin", AText "")]) []
      , VNode "script" (fl [ ("language", AText "javascript")])
          [ VRawText $ T.decodeUtf8 component ]
      , VNode "link" (fl [ ("rel", AText "stylesheet"), ("href", AText "https://unpkg.com/leaflet@1.5.1/dist/leaflet.css")]) []
      ]
