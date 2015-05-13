{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.MachineKindResource where

import Opaleye.RunQuery (runQuery)
import Opaleye.PGTypes (pgInt4, pgString, pgDay)

import Data.List (zip)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (liftA3)
import Control.Monad (forM_)

import Rest.Resource (Resource, Void, schema, list, name, mkResourceId, get, update, remove)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, jsonI)
import Rest.Handler (ListHandler, mkListing, Handler, mkConstHandler, mkInputHandler)

import qualified Crm.Shared.MachineKind as MK

import Crm.Server.Helpers (prepareReaderTuple, readMay', dayToYmd, today, deleteRows',
  withConnId, ymdToDay, maybeToNullable, createDeletion, prepareUpdate)
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB
import Crm.Server.Core (nextServiceDate)

import qualified Crm.Shared.ExtraField as EF
import qualified Crm.Shared.Api as A

import TupleTH

resource :: Resource Dependencies Dependencies () Void Void
resource = mkResourceId {
  schema = S.noListing (S.unnamedSingle $ const ()) ,
  name = A.machineKind ,
  update = Just updation }

updation :: Handler Dependencies
updation = mkInputHandler jsonI $ \allSettings -> let 
  s = allSettings :: [(MK.MachineKindEnum, [(EF.ExtraFieldIdentification, MK.MachineKindSpecific)])]
  insertSetting (machineKindEnum, extraFields) = let
    insertField (index, (fieldId, field)) =
      case fieldId of
        EF.ToBeAssigned -> return ()
        EF.Assigned assignedId -> return ()
    in forM_ ([0..] `zip` extraFields) insertField
  in forM_ allSettings insertSetting
