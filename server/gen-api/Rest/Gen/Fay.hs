{-# OPTIONS -fno-warn-incomplete-patterns #-} 

{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}

module Rest.Gen.Fay (
  mkFayApi
  ) where

import Control.Arrow (second)
import Control.Category
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import Prelude hiding (id, (.))
import Safe
import System.Directory
import System.FilePath
import qualified Data.Label.Total               as L
import qualified Data.List.NonEmpty             as NList
import qualified Language.Haskell.Exts.Pretty   as H
import qualified Language.Haskell.Exts.Syntax   as H
import Rest.Gen.Haskell (HaskellContext(..))

import Rest.Api (Router)

import Rest.Gen.Base
import Rest.Gen.Types
import qualified Rest.Gen.Base.ActionInfo.Ident as Ident
import qualified Data.Generics.Uniplate.Data    as U


rewriteModuleNames :: [(H.ModuleName, H.ModuleName)] -> H.Module -> H.Module
rewriteModuleNames rews = U.transformBi $ \m -> lookupJustDef m m rews


mkFayApi :: HaskellContext -> Router m s -> IO ()
mkFayApi ctx r =
  do let tree = sortTree . (if includePrivate ctx then id else noPrivate) . apiSubtrees $ r
     mapM_ (writeRes ctx) $ allSubTrees tree


writeRes :: HaskellContext -> ApiResource -> IO ()
writeRes ctx node =
  do createDirectoryIfMissing True (targetPath ctx </> "src" </> modPath (namespace ctx ++ resParents node))
     writeFile (targetPath ctx </> "src" </> modPath (namespace ctx ++ resId node) ++ ".hs") (mkRes ctx node)


mkRes :: HaskellContext -> ApiResource -> String
mkRes ctx node = H.prettyPrint $ buildHaskellModule ctx node pragmas Nothing
  where
    pragmas = [H.OptionsPragma noLoc (Just H.GHC) "-fno-warn-unused-imports"]
    _warningText = "Warning!! This is automatically generated code, do not modify!"


buildHaskellModule :: 
  HaskellContext -> 
  ApiResource -> 
  [H.ModulePragma] -> 
  Maybe H.WarningText -> 
  H.Module
buildHaskellModule ctx node pragmas warningText =
  rewriteModuleNames (rewrites ctx) $
    H.Module noLoc name pragmas warningText exportSpecs importDecls decls
  where
    name = H.ModuleName $ qualModName $ namespace ctx ++ resId node
    exportSpecs = Nothing
    dataText = H.ImportDecl noLoc (H.ModuleName "Data.Text") False False False Nothing Nothing 
      (Just (False, [H.IVar (H.Ident "pack")]))
    runtimeImports = dataText : []
    importDecls = nub $ runtimeImports 
                     ++ parentImports
                     ++ dataImports
                     ++ idImports
    decls = concat funcs ++ idData node

    parentImports = map mkImport . tail . inits . resParents $ node
    dataImports = map (qualImport . unModuleName) datImp
    idImports = concat . mapMaybe (return . map (qualImport . unModuleName) . Ident.haskellModules <=< snd) . resAccessors $ node

    (funcs, datImp) = second (map textInternal . filter rest . nub . concat) . unzip . map (mkFunction . resName $ node) 
      . filter onlySingle . filter (not . isFile) $ resItems node where
      textInternal (H.ModuleName str) = H.ModuleName $ if isInfixOf "Data.Text.Internal" str
        then "Data.Text"
        else str
      rest (H.ModuleName str) = take 4 str /= "Rest"
      onlySingle (ApiAction _ _ actionInfo) = not $ elem actionType' blacklistActions where
        actionType' = actionType actionInfo
        blacklistActions = [DeleteMany, UpdateMany]
      isFile (ApiAction _ _ actionInfo) = any isFile' (outputs actionInfo) || any isFile' (inputs actionInfo) where
        isFile' (DataDescription (DataDesc File _ _) _) = True
        isFile' _                                       = False
    mkImport p = (namedImport importName) { H.importQualified = True,
                                            H.importAs = importAs' }
      where importName = qualModName $ namespace ctx ++ p
            importAs' = fmap (H.ModuleName . modName) . lastMay $ p

noBinds :: Maybe a
noBinds = Nothing


use :: H.Name -> H.Exp
use = H.Var . H.UnQual


idData :: ApiResource -> [H.Decl]
idData node =
  go $ resAccessors node where
  go ((pathName, Just i) : xs) =
    case Ident.description i of
      "string" -> go xs
      _ -> go xs ++ [
        H.TypeDecl noLoc tyIdent [] (Ident.haskellType i) ,
        H.TypeSig noLoc [H.Ident "getInt'"] newtypeToString ,
        H.FunBind [H.Match noLoc (H.Ident "getInt'") pat Nothing rhs' noBinds] ,
        H.TypeSig noLoc [H.Ident "getInt"] newtypeToString ,
        H.FunBind [H.Match noLoc (H.Ident "getInt") pat Nothing rhs noBinds]] where
          pat = [H.PApp qName ((:[]) . H.PVar .  H.Ident $ "int")]
          rhs' = H.UnGuardedRhs $ H.InfixApp
            (H.Lit . H.String $ pathName ++ "/")
            (H.QVarOp . H.UnQual . H.Symbol $ "++")
            (var "show" `H.App` var "int")
          rhs = H.UnGuardedRhs $ var "show" `H.App` var "int"
          qName = getQName . Ident.haskellType $ i
          getQName type' = case type' of
            H.TyCon q -> q
            H.TyApp (H.TyCon (H.Qual module' (H.Ident name'))) _ -> H.Qual module' (H.Ident . init $ name')
          newtypeToString = newtype' `H.TyFun` string
          newtype' = Ident.haskellType i
          string = H.TyCon $ H.UnQual $ H.Ident "String"
  go _ = []


mkFunction :: String -> ApiAction -> ([H.Decl], [H.ModuleName])
mkFunction res (ApiAction _ lnk ai) = ([typeSignature, functionBinding], usedImports) where
  typeSignature = H.TypeSig noLoc [funName] fType
  functionBinding = H.FunBind [H.Match noLoc funName fParams Nothing rhs noBinds]
  usedImports = responseModules errorI ++ responseModules output ++ maybe [] inputModules mInp ++ [runtime, router]

  runtime = H.ModuleName "Crm.Runtime"
  router = H.ModuleName "Crm.Router"
  callbackIdent = H.Ident "callback"
  routerIdent = H.Ident "router"
  funName = mkHsName ai
  pFirst f s = H.PTuple H.Boxed [H.PVar . H.Ident $ f, H.PVar $ H.Ident s]
  pRest = H.Ident "px"
  fParams = 
    (if null (params ai) then [] else [H.PInfixApp (pFirst "pName" "pValue") (H.UnQual . H.Symbol $ ":") (H.PVar pRest)]) ++
    (map H.PVar lPars) ++ 
    (maybe [] ((:[]) . H.PVar . hsName . cleanName . description) (ident ai)) ++ 
    (map H.PVar $ maybe [] (const [input]) mInp) ++ 
    (map H.PVar [callbackIdent]) ++
    [H.PVar routerIdent]
  (lUrl, lPars) = linkToURL (ident ai) res lnk
  mInp :: Maybe InputInfo
  mInp = fmap (inputInfo . L.get desc . chooseType) . NList.nonEmpty . inputs $ ai
  (isList, fType) = (isList', fTypify tyParts) where 
    fayUnit = H.TyApp (H.TyCon . H.UnQual . H.Ident $ "Fay") (H.TyCon . H.UnQual . H.Ident $ "()")
    (callbackParams, isList') = unList $ responseHaskellType output
    unList ( H.TyApp (H.TyCon (H.Qual (H.ModuleName "Rest.Types.Container") _)) elements) = (H.TyList elements, True)
    unList x = (x, False)
    callback = H.TyFun callbackParams fayUnit
    crmRouter = H.TyCon . H.Qual router . H.Ident $ "CrmRouter"

    fTypify :: [H.Type] -> H.Type
    fTypify [] = error "Rest.Gen.Haskell.mkFunction.fTypify - expects at least one type"
    fTypify [ty1] = ty1
    fTypify [ty1, ty2] = H.TyFun ty1 ty2
    fTypify (ty1 : tys) = H.TyFun ty1 (fTypify tys)
    text = H.TyCon . H.UnQual . H.Ident $ "String"
    tyParts = 
      (if null (params ai) then [] else [H.TyList . H.TyTuple H.Boxed $ [text, text]]) ++
      map qualIdent lPars ++ 
      maybe [] (return . Ident.haskellType) (ident ai) ++ 
      inp ++ 
      [callback, crmRouter, fayUnit]

    qualIdent (H.Ident s)
      | s == cleanHsName res = H.TyCon $ H.UnQual tyIdent
      | otherwise = H.TyCon $ H.Qual (H.ModuleName $ modName s) tyIdent
    qualIdent H.Symbol{} = error "Rest.Gen.Haskell.mkFunction.qualIdent - not expecting a Symbol"
    inp | Just i  <- mInp
        , i' <- inputHaskellType i = [i']
        | otherwise = []
  input = H.Ident "input"
  ajax = H.Var . H.Qual runtime . H.Ident $ "passwordAjax"
  nothing = H.Con . H.UnQual . H.Ident $ "Nothing"
  callbackVar = H.Var . H.UnQual $ callbackIdent
  runtimeVar = H.Var . H.Qual runtime . H.Ident
  itemsIdent = runtimeVar "items"
  compose = H.QVarOp $ H.UnQual $ H.Symbol "."
  rhs = H.UnGuardedRhs exp'
  items 
    | isList = \cbackIdent -> H.InfixApp cbackIdent compose itemsIdent
    | otherwise = id
  exp' = ajax `H.App` (addParams . mkPack . concat' $ lUrl) `H.App` (items callbackVar) `H.App` 
      input' `H.App` method' `H.App` nothing `H.App` nothing `H.App` (H.Var . H.UnQual $ routerIdent) where
    plusPlus = (H.QVarOp $ H.UnQual $ H.Symbol "++")
    addParams url = if null . params $ ai 
      then url
      else url `pp` mkParam "?" "pName" "pValue" `pp` restParams
      where
      exp1 `pp` exp2 = H.InfixApp exp1 plusPlus exp2
      mkParam symbol f s = (H.Lit . H.String $ symbol) `pp` (H.Var . H.UnQual . H.Ident $ f) `pp` (H.Lit . H.String $ "=") `pp` 
        (H.Var . H.UnQual . H.Ident $ s)
      mkOtherParams = H.Lambda noLoc [pFirst "pName'" "pValue'"] (mkParam "&" "pName'" "pValue'")
      restParams = 
        (H.Var . H.UnQual . H.Ident $ "concat") `H.App`  
          ((H.Var . H.UnQual . H.Ident $ "map") `H.App` 
          mkOtherParams `H.App` 
          (H.Var . H.UnQual . H.Ident $ "px"))
    method' = mkMethod ai
    concat' (exp1:exp2:exps) = H.InfixApp (addEndSlash exp1) plusPlus $ concat' (exp2:exps) where
      addEndSlash e = H.InfixApp e plusPlus (H.Lit . H.String $ "/")
    concat' (exp1:[]) = exp1
    mkPack = H.InfixApp (var "pack") (H.QVarOp . H.UnQual . H.Symbol $ "$")
    input' = maybe nothing (const $ (H.Con . H.UnQual . H.Ident $ "Just") `H.App` use input) mInp

  errorI :: ResponseInfo
  errorI = errorInfo responseType
  output :: ResponseInfo
  output = outputInfo responseType
  responseType = chooseResponseType ai

  mkMethod :: ActionInfo -> H.Exp
  mkMethod ai' = runtimeVar $ case actionType ai' of
    Retrieve -> "get"
    Create -> "post"
    Delete -> "delete"
    List -> "get"
    Update -> "put"


linkToURL :: Maybe Ident -> String -> Link -> ([H.Exp], [H.Name])
linkToURL mId' res lnk = urlParts mId' res lnk ([], [])


var :: String -> H.Exp
var = H.Var . H.UnQual . H.Ident


urlParts :: Maybe Ident -> String -> Link -> ([H.Exp], [H.Name]) -> ([H.Exp], [H.Name])
urlParts mId' res lnk ac@(rlnk, pars) =
  case lnk of
    [] -> ac
    (LResource r : a@(LAccess _) : xs)
      | not (hasParam a) -> urlParts mId' res xs (rlnk ++ [H.Lit $ H.String r], pars)
      | otherwise -> urlParts mId' res xs (rlnk', pars ++ [H.Ident . cleanHsName $ r])
           where rlnk' = rlnk ++ ((H.Lit $ H.String $ r) : tailed)
                 tailed = [funName `H.App` (use $ hsName (cleanName r))]
                 h = modName . cleanHsName $ r
                 q = if r == res
                   then H.UnQual 
                   else H.Qual (H.ModuleName h)
                 funName = H.Var $ q (H.Ident "getInt'")
    (LParam p : xs) -> 
      urlParts mId' res xs (rlnk ++ [(mkToString . use $ hsName (cleanName p))], pars) 
        where
        mkToString = case Ident.haskellType `fmap` mId' of
          Just (H.TyCon (H.UnQual (H.Ident "String"))) -> id
          _ -> \x -> var "getInt" `H.App` x
    (i : xs) -> urlParts mId' res xs (rlnk ++ [H.Lit $ H.String $ itemString i], pars)


tyIdent :: H.Name
tyIdent = H.Ident "Identifier"


mkHsName :: ActionInfo -> H.Name
mkHsName ai = hsName $ concatMap cleanName parts
  where
      parts = case actionType ai of
                Retrieve   -> let nm = get ++ by ++ target
                              in if null nm then ["access"] else nm
                Create     -> ["create"] ++ by ++ target
                -- Should be delete, but delete is a JS keyword and causes problems in collect.
                Delete     -> ["remove"] ++ by ++ target
                DeleteMany -> ["removeMany"] ++ by ++ target
                List       -> ["list"]   ++ by ++ target
                Update     -> ["save"]   ++ by ++ target
                UpdateMany -> ["saveMany"] ++ by ++ target
                Modify   -> if resDir ai == "" then ["do"] else [resDir ai]

      target = if resDir ai == "" then maybe [] ((:[]) . description) (ident ai) else [resDir ai]
      by     = if target /= [] && (isJust (ident ai) || actionType ai == UpdateMany) then ["by"] else []
      get    = if isAccessor ai then [] else ["get"]

hsName :: [String] -> H.Name
hsName []       = H.Ident ""
hsName (x : xs) = H.Ident $ cleanHsName $ downFirst x ++ concatMap upFirst xs


cleanHsName :: String -> String
cleanHsName s =
  if s `elem` reservedNames
    then s ++ "_"
    else intercalate "" . cleanName $ s
  where
    reservedNames =
      ["as","case","class","data","instance","default","deriving","do"
      ,"foreign","if","then","else","import","infix","infixl","infixr","let"
      ,"in","module","newtype","of","qualified","type","where"]


qualModName :: ResourceId -> String
qualModName = intercalate "." . map modName


modPath :: ResourceId -> String
modPath = intercalate "/" . map modName


modName :: String -> String
modName = concatMap upFirst . cleanName


data InputInfo = InputInfo
  { inputModules     :: [H.ModuleName]
  , inputHaskellType :: H.Type
  } deriving (Eq, Show)

inputInfo :: DataDesc -> InputInfo
inputInfo dsc =
  case L.get dataType dsc of
    String -> InputInfo [] (haskellStringType)
    -- TODO fromJusts
    XML    -> InputInfo (L.get haskellModules dsc) (L.get haskellType dsc) 
    JSON   -> InputInfo (L.get haskellModules dsc) (L.get haskellType dsc)
    File   -> InputInfo [] haskellByteStringType 
    Other  -> InputInfo [] haskellByteStringType 


data ResponseInfo = ResponseInfo
  { responseModules     :: [H.ModuleName]
  , responseHaskellType :: H.Type
  } deriving (Eq, Show)


outputInfo :: ResponseType -> ResponseInfo
outputInfo r =
  case outputType r of
    Nothing -> ResponseInfo [] haskellUnitType 
    Just t -> case L.get dataType t of
      String -> ResponseInfo [] haskellStringType 
      XML    -> ResponseInfo (L.get haskellModules t) (L.get haskellType t) 
      JSON   -> ResponseInfo (L.get haskellModules t) (L.get haskellType t) 
      File   -> ResponseInfo [] haskellByteStringType 
      Other  -> ResponseInfo [] haskellByteStringType 


errorInfo :: ResponseType -> ResponseInfo
errorInfo r =
  case errorType r of
    -- Rest only has XML and JSON instances for errors, so we need to
    -- include at least one of these in the accept header. We don't
    -- want to make assumptions about the response type if there is no
    -- accept header so in that case we force it to be JSON.
    Nothing -> fromJustNote ("rest-gen bug: toResponseInfo' was called with a data type other than XML or JSON, responseType: " ++ show r)
             . toResponseInfo' . defaultErrorDataDesc . maybe JSON (\x -> case x of { XML -> XML; _ -> JSON })
             . fmap (L.get dataType) . outputType
             $ r
    Just t -> toResponseInfo [t]
  where
    toResponseInfo :: [DataDesc] -> ResponseInfo
    toResponseInfo xs
      = fromMaybe (error $ "Unsupported error formats: " ++ show xs ++ ", this is a bug in rest-gen.")
      . headMay
      . mapMaybe toResponseInfo'
      $ xs
    toResponseInfo' :: DataDesc -> Maybe ResponseInfo
    toResponseInfo' t = case L.get dataType t of
      XML  -> Just $ ResponseInfo (L.get haskellModules t) (L.get haskellType t) 
      JSON -> Just $ ResponseInfo (L.get haskellModules t) (L.get haskellType t) 
      _    -> Nothing


defaultErrorDataDesc :: DataType -> DataDesc
defaultErrorDataDesc dt =
  DataDesc
    { _dataType       = dt
    , _haskellType    = haskellVoidType
    , _haskellModules = [ModuleName "Rest.Types.Void"]
    }


-- taken from rest gen private module

upFirst :: String -> String
upFirst = mapHead toUpper

downFirst :: String -> String
downFirst = mapHead toLower

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x : xs) = f x : xs
