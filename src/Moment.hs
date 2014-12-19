{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PackageImports #-}

module Moment (
  Moment, MomentObject ,
  requireMoment, browserMoment ,
  now ,
  Moment ) where

import FFI
import HaskellReact
import "fay-base" Prelude

-- | opaque data type representing moment library instance
data MomentObject

-- | opaque data type representing concrete moment in time
data Moment

-- | use CommonJS module
requireMoment :: Moment
requireMoment = ffi " require('moment') "

-- | conjure up the moment from the thin air
browserMoment :: Moment
browserMoment = ffi " moment "

now :: Moment -> MomentObject
now = ffi " %1() "
