{-# LANGUAGE GADTs #-}

module Concur.Replica.Components.JS where

import Data.List
import Data.Proxy
import GHC.Records

data JSValue a where
  JSString :: String -> JSValue String
  JSInt :: Int -> JSValue Int
  JSDouble :: Double -> JSValue Double

data JS a where
  Pure :: JSValue a -> JS a
  Var  :: String -> JS a
  Get :: HasField x r a => Proxy x -> r -> JS a
  If :: JS Bool -> JS a -> JS a -> JS a

  NumCmp :: Num a => String -> JS a -> JS a -> JS Bool
  NumUn  :: Num a => String -> JS a -> JS a
  NumBin :: Num a => String -> JS a -> JS a -> JS a

  Abs :: (JS a -> JS b) -> JS (a -> b)
  App :: JS (a -> b) -> JS a -> JS b

  Pair :: JS a -> JS b -> JS (a, b)
  Fst :: JS (a, b) -> JS a
  Snd :: JS (a, b) -> JS b
  List :: [JS a] -> JS [a]

parens :: String -> String
parens s = "(" <> s <> ")"

toJS :: JS a -> String
toJS (Pure (JSString v)) = v
toJS (Pure (JSInt v)) = show v
toJS (Pure (JSDouble v)) = show v
toJS (Var a) = a
toJS (If c a b) = parens (toJS c <> " ? " <> toJS a <> " : " <> toJS b)
toJS (NumCmp op a b) = toJS a <> " " <> op <> " " <> toJS b
toJS (NumUn op a) = op <> " " <> toJS a
toJS (NumBin op a b) = toJS a <> " " <> op <> " " <> toJS b
toJS (Abs f) = parens ("function(a) { return " <> toJS (f $ Var "a") <> "; }")
toJS (App f e) = toJS f <> "(" <> toJS e <> ")"
toJS (Pair a b) = "[" <> toJS a <> "," <> toJS b <> "]"
toJS (Fst p) = toJS p <> "[0]"
toJS (Snd p) = toJS p <> "[1]"
toJS (List as) = "[" <> intercalate "," (fmap toJS as) <> "]"

testJS = Abs (\a -> If (NumCmp ">" a (Pure $ JSInt 5)) (Pair a a) (Pair a a)) `App` (Pure $ JSInt 7)
