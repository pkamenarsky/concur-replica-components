{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Concur.Replica.Components.Indexed where

import Data.Proxy
import GHC.TypeLits

type family Member (a :: Symbol) (as :: [Symbol]) where
  Member a '[]       = False
  Member a (a ': as) = True
  Member a (b ': as) = Member a as

isMember :: Member a as ~ True => Proxy a -> Proxy as -> ()
isMember = undefined

test = isMember (Proxy :: Proxy "asd") (Proxy :: Proxy '["asd", "iii"])
