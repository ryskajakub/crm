{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Server.Base where

import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, defaultConnectInfo, connect, close)

import Opaleye.QueryArr (Query, QueryArr)
import Opaleye.Table (Table(Table), required, queryTable, optional)
import Opaleye.Column (Column, toNullable, Nullable)
import qualified Opaleye.Column as COL
import Opaleye.Order (orderBy, asc, limit, desc)
import Opaleye.RunQuery (runQuery)
import Opaleye.Operators ((.==), (.&&), (.||), restrict, lower, (.<))
import qualified Opaleye.Operators as OO
import Opaleye.PGTypes (pgInt4, PGDate, pgDay, PGBool, PGInt4, PGInt8, PGText, pgString, pgBool)
import Opaleye.Manipulation (runInsert, runUpdate, runInsertReturning, runDelete)
import qualified Opaleye.Aggregate as AGG

import Control.Monad.Reader (ReaderT, ask, runReaderT, mapReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Error (ErrorT)
import Control.Monad.Trans.Class (lift)
import Control.Arrow (returnA)
import Control.Monad (forM_, forM)

import Data.Profunctor.Product (p1, p2, p3, p4, p5, p6)
import Data.Maybe (maybeToList)
import Data.Functor.Identity (runIdentity)
import Data.Time.Calendar (Day, addDays)
import Data.Int (Int64)
import Data.List (intersperse, sortBy)
import Data.Tuple.All (Sel1, sel1, sel2, sel3, sel4, upd3, uncurryN)

import Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)
import Rest.Resource (Resource, mkResourceId, Void, schema, list, name, create, mkResourceReaderWith, get ,
  update )
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkListing, mkInputHandler, Handler, mkConstHandler)
import Rest.Types.Error (DataError(ParseError), Reason(InputError, IdentError, 
  NotFound, NotAllowed, UnsupportedRoute))

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.YearMonthDay as D
import qualified Crm.Shared.Employee as E
import qualified Crm.Shared.UpkeepSequence as US

import Crm.Server.Helpers (ymdToDay, dayToYmd, maybeId, readMay', mapUpkeeps, 
  prepareReaderIdentity, prepareReaderTuple, mappedUpkeepSequences)
import Crm.Server.Boilerplate ()
import Crm.Server.DB
import Crm.Server.Types
import Crm.Server.Api.CompanyResource (companyResource)
import Crm.Server.Api.MachineResource (machineResource)
import qualified Crm.Server.Api.UpkeepResource as UR
import Crm.Server.Api.MachineTypeResource (machineTypeResource)
import Crm.Server.Api.EmployeeResource (employeeResource)
import qualified Crm.Server.Api.Company.MachineResource as CMR
import qualified Crm.Server.Api.Company.UpkeepResource as UMR

import Safe (readMay, minimumMay)

import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

router' :: Router Dependencies Dependencies
router' = root `compose` ((route companyResource `compose` route CMR.machineResource)
                                                 `compose` route UMR.upkeepResource)
               `compose` route machineResource
               `compose` route UR.upkeepResource
               `compose` route machineTypeResource
               `compose` route employeeResource

api :: Api Dependencies
api = [(mkVersion 1 0 0, Some1 $ router')]
