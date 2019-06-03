{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.Machine.ReassignResource (
  resource ) where

import           Crm.Server.Boilerplate             ()

import           Control.Monad                      (forM_)

import           Control.Lens                       (view, _1)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (ask)
import           Control.Monad.Trans.Except         (ExceptT)
import           Data.Pool                          (withResource)
import           Data.Text                          (Text)
import           Data.Tuple.All                     (sel2)
import           Database.PostgreSQL.Simple         (Connection)
import           Opaleye                            (runQuery, (.===))
import           Opaleye.Manipulation               (runInsert,
                                                     runInsertReturning, runUpdate)
import           Opaleye.PGTypes                    (pgBool, pgDay, pgInt4,
                                                     pgStrictText)
import           Rest.Dictionary.Combinators        (jsonI, jsonO)
import           Rest.Handler                       (Handler, ListHandler)
import           Rest.Resource                      (Resource, Void, create,
                                                     list, mkResourceId, name,
                                                     schema, statics)
import qualified Rest.Schema                        as S
import           Rest.Types.Error                   (Reason)

import qualified Crm.Shared.Api                     as A
import qualified Crm.Shared.Company                 as C
import qualified Crm.Shared.ContactPerson           as CP
import qualified Crm.Shared.ExtraField              as EF
import qualified Crm.Shared.Machine                 as M
import qualified Crm.Shared.MachineKind             as MK
import qualified Crm.Shared.MachineType             as MT
import           Crm.Shared.MyMaybe                 (toMaybe)
import qualified Crm.Shared.UpkeepSequence          as US
import qualified Crm.Shared.YearMonthDay            as YMD

import           Crm.Server.Boilerplate             
import           Crm.Server.CachedCore              (recomputeSingle)
import           Crm.Server.DB
import           Crm.Server.Handler                 (mkInputHandler',
                                                     mkListing')
import           Crm.Server.Helpers                 (maybeToNullable)
import           Crm.Server.Types

import           Crm.Server.Database.MachineType
import qualified Crm.Server.Database.UpkeepSequence as USD

import           TupleTH                            (proj)

changeCompany ::
  Connection ->
  M.MachineId ->
  C.CompanyId ->
  IO () 
changeCompany connection machineId newCompanyId = let
  update columnsR = columnsR {
      _companyFK = fmap pgInt4 newCompanyId ,
      _machinePK = fmap (Just . pgInt4) machineId 
    }
  in do
    runUpdate connection machinesTable update 
      (\columnsR -> _machinePK columnsR .=== fmap pgInt4 machineId)
    putStrLn $ show machineId
    putStrLn $ show newCompanyId
    return ()

changeCompanyHandler :: Handler (IdDependencies' M.MachineId)
changeCompanyHandler = mkInputHandler' (jsonI) $ \(ReassignPayload newCompanyId) -> do
  ((cache, pool), machineId) <- ask

  machineRows <- withResource pool $ \connection -> liftIO $ runQuery connection (machineQuery machineId)
  machineRow :: MachineRecord <- singleRowOrColumn machineRows

  liftIO $ withResource pool $ \connection ->
    changeCompany connection machineId (C.CompanyId newCompanyId)

  let oldCompanyId = _companyFK machineRow

  liftIO $ putStrLn "XXXXXXX"
  liftIO $ putStrLn $ show oldCompanyId

  withResource pool $ \connection -> recomputeSingle (oldCompanyId) connection cache
  withResource pool $ \connection -> recomputeSingle (C.CompanyId newCompanyId) connection cache

resource :: Resource (IdDependencies' M.MachineId) (IdDependencies' M.MachineId) Void Void ()
resource = mkResourceId {
  name = "reassign" ,
  schema = S.noListing $ S.named [("do", S.static ())] ,
  statics = \_ -> changeCompanyHandler }
