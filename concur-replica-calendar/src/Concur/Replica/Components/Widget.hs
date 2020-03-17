{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concur.Replica.Components.Widget where

import Prelude hiding (div)

data V a

instance Functor V

vpure :: a -> V a
vpure = undefined

data Widget a

instance Functor Widget
instance Applicative Widget
instance Monad Widget

data Props a
data Event = Event

onClick :: Props Event
onClick = undefined

vif :: V Bool -> V a -> V a -> V a
vif = undefined

text :: V String -> Widget (V a)
text = undefined

div :: [Props a] -> [Widget (V a)] -> Widget (V a)
div = undefined

test = do
  e <- div [ onClick ] []

  let t = flip fmap e $ \e -> case e of
        Event -> text (vpure "asD")

  text (vpure "asd")
  test
