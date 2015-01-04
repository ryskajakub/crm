{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.DatePicker (
  DatePicker ) where

import "fay-base" Prelude as P

import qualified Crm.Shared.YearMonthDay as YMD

type DatePicker = (YMD.YearMonthDay, Bool)
