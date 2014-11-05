module Main where

import Control.Monad.IO.Class (liftIO)

import Data.Text (pack)

import Snap.Http.Server (quickHttpServe)

import Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)
import Rest.Driver.Snap (apiToHandler')
import Rest.Resource (Resource, mkResourceId, Void, name, schema, list)
import Rest.Schema (Schema, named, withListing)
import Rest.Dictionary.Combinators (jsonO, someO)
import Rest.Handler (ListHandler, mkListing)

listing :: ListHandler IO
listing = mkListing (jsonO . someO) (\_ -> return $ return [pack "ahoj", pack "pse"])

dogSchema :: Schema Void () Void
dogSchema = withListing () (named [])

dog :: Resource IO IO Void () Void
dog = mkResourceId {
    list = \_ -> listing
    , name = "dogs"
    , schema = dogSchema
  }

router :: Router IO IO
router = root `compose` route dog

api :: Api IO
api = [(mkVersion 1 0 0, Some1 router)]

main :: IO ()
main = quickHttpServe $ apiToHandler' liftIO api
