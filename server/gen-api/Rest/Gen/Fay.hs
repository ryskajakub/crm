{-# LANGUAGE
    CPP
  , DoAndIfThenElse
  , LambdaCase
  , PatternGuards
  , TemplateHaskell
  , ViewPatterns
  #-}
module Rest.Gen.Fay (
  mkFayApi
  ) where

import Control.Arrow (first, second)
import Control.Category
import Control.Monad
import Data.List
import Data.Maybe
import Prelude hiding (id, (.))
import Safe
import System.Directory
import System.FilePath
import qualified Data.Label.Total                            as L
import qualified Data.List.NonEmpty                          as NList
import qualified Language.Haskell.Exts.Pretty                as H
import qualified Language.Haskell.Exts.Syntax                as H
import Rest.Gen.Haskell (HaskellContext(..))

import Rest.Api (Router, Version)

import Rest.Gen.Base
import Rest.Gen.Types
import Rest.Gen.Utils
import qualified Rest.Gen.Base.ActionInfo.Ident as Ident

mkFayApi :: HaskellContext -> Router m s -> IO ()
mkFayApi ctx r =
  do let tree = sortTree . (if includePrivate ctx then id else noPrivate) . apiSubtrees $ r
     mapM_ (writeRes ctx) . filter (("companies" ==) . resName) $ allSubTrees tree

writeRes :: HaskellContext -> ApiResource -> IO ()
writeRes ctx node =
  do createDirectoryIfMissing True (targetPath ctx </> "src" </> modPath (namespace ctx ++ resParents node))
     writeFile (targetPath ctx </> "src" </> modPath (namespace ctx ++ resId node) ++ ".hs") (mkRes ctx node)

mkRes :: HaskellContext -> ApiResource -> String
mkRes ctx node = H.prettyPrint $ buildHaskellModule ctx node pragmas Nothing
  where
    pragmas = [H.OptionsPragma noLoc (Just H.GHC) "-fno-warn-unused-imports"]
    _warningText = "Warning!! This is automatically generated code, do not modify!"

buildHaskellModule :: HaskellContext -> ApiResource ->
                      [H.ModulePragma] -> Maybe H.WarningText ->
                      H.Module
buildHaskellModule ctx node pragmas warningText =
    H.Module noLoc name pragmas warningText exportSpecs importDecls decls
  where
    name = H.ModuleName $ qualModName $ namespace ctx ++ resId node
    exportSpecs = Nothing
    dataText = H.ImportDecl noLoc (H.ModuleName "Data.Text") False False False Nothing Nothing 
      (Just (False, [H.IVar H.NoNamespace (H.Ident "pack")]))
    runtimeImports = dataText : []
    importDecls = nub $ runtimeImports 
                     ++ parentImports
                     ++ dataImports
                     ++ idImports
    decls = concat funcs

    parentImports = map mkImport . tail . inits . resParents $ node
    dataImports = map (qualImport . unModuleName) datImp
    idImports = concat . mapMaybe (return . map (qualImport . unModuleName) . Ident.haskellModules <=< snd) . resAccessors $ node

    (funcs, datImp) = second (filter rest . nub . concat) . unzip . map (mkFunction (apiVersion ctx) . resName $ node) 
      . filter onlySingle $ resItems node where
      rest (H.ModuleName str) = take 4 str /= "Rest"
      onlySingle (ApiAction _ _ actionInfo) = elem actionType' blacklistActions where
        actionType' = actionType actionInfo
        blacklistActions = [Delete]
    mkImport p = (namedImport importName) { H.importQualified = True,
                                            H.importAs = importAs' }
      where importName = qualModName $ namespace ctx ++ p
            importAs' = fmap (H.ModuleName . modName) . lastMay $ p

noBinds :: H.Binds
noBinds = H.BDecls []

use :: H.Name -> H.Exp
use = H.Var . H.UnQual

useMQual :: (Maybe H.ModuleName) -> H.Name -> H.Exp
useMQual Nothing = use
useMQual (Just qual) = H.Var . (H.Qual $ qual)

mkFunction :: Version -> String -> ApiAction -> ([H.Decl], [H.ModuleName])
mkFunction ver res (is @ ( ApiAction _ lnk ai)) =
  ([H.TypeSig noLoc [funName] fType,
    H.FunBind [H.Match noLoc funName fParams Nothing rhs noBinds]],
    responseModules errorI ++ responseModules output ++ maybe [] inputModules mInp ++ [runtime])
     where
       runtime = H.ModuleName "Crm.Server"
       callbackIdent = H.Ident "callback"
       funName = mkHsName ai
       fParams = map H.PVar $ lPars
                           ++ maybe [] ((:[]) . hsName . cleanName . description) (ident ai)
                           ++ maybe [] (const [input]) mInp
                           ++ [callbackIdent]
       (lUrl, lPars) = linkToURL res lnk
       mInp :: Maybe InputInfo
       mInp = fmap (inputInfo . L.get desc . chooseType) . NList.nonEmpty . inputs $ ai
       (isList, fType)   = (isList', H.TyFun fParams' fayUnit)
         where 
               fParams' = fTypify tyParts
               fayUnit = H.TyApp (H.TyCon $ H.UnQual $ H.Ident "Fay") (H.TyCon $ H.UnQual $ H.Ident "()")
               (callbackParams, isList') = unList $ responseHaskellType output
               unList ( H.TyApp (H.TyCon (H.Qual (H.ModuleName "Rest.Types.Container") _)) elements) = (H.TyList elements, True)
               unList x = (x, False)
               callback = H.TyParen $ H.TyFun callbackParams fayUnit
               fTypify :: [H.Type] -> H.Type
               fTypify [] = error "Rest.Gen.Haskell.mkFunction.fTypify - expects at least one type"
               fTypify [ty1] = ty1
               fTypify [ty1, ty2] = H.TyFun ty1 ty2
               fTypify (ty1 : tys) = H.TyFun ty1 (fTypify tys)
               tyParts = map qualIdent lPars
                         ++ maybe [] (return . Ident.haskellType) (ident ai)
                         ++ inp
                         ++ [callback]

               qualIdent (H.Ident s)
                 | s == cleanHsName res = H.TyCon $ H.UnQual tyIdent
                 | otherwise = H.TyCon $ H.Qual (H.ModuleName $ modName s) tyIdent
               qualIdent H.Symbol{} = error "Rest.Gen.Haskell.mkFunction.qualIdent - not expecting a Symbol"
               inp | Just i  <- mInp
                   , i' <- inputHaskellType i = [i']
                   | otherwise = []
       input = H.Ident "input"
       ajax = H.Var $ H.Qual runtime $ H.Ident "passwordAjax"
       nothing = H.Con $ H.UnQual $ H.Ident "Nothing"
       callbackVar = H.Var $ H.UnQual callbackIdent
       itemsIdent = H.Var $ H.Qual runtime $ H.Ident "items"
       compose = H.QVarOp $ H.UnQual $ H.Symbol "."
       rhs = H.UnGuardedRhs exp'
       items 
         | isList = \cbackIdent -> H.InfixApp cbackIdent compose itemsIdent
         | otherwise = id
       exp' = ajax `H.App` (mkPack . concat' $ url) `H.App` (items callbackVar) `H.App` input' `H.App` nothing `H.App` nothing where
         concat' (exp1:exp2:exps) = H.InfixApp (addEndSlash exp1) plusPlus $ concat' (exp2:exps) where
           addEndSlash e = H.InfixApp e plusPlus (H.Lit . H.String $ "/")
           plusPlus = (H.QVarOp $ H.UnQual $ H.Symbol "++")
         concat' (exp1:[]) = exp1
         mkPack = H.InfixApp (var "pack") (H.QVarOp $ H.UnQual $ H.Symbol "$")
         input' = maybe nothing (const $ use input) mInp

       (ve, url) = ("v" ++ show ver, lUrl)
       errorI :: ResponseInfo
       errorI = errorInfo responseType
       output :: ResponseInfo
       output = outputInfo responseType
       responseType = chooseResponseType ai

linkToURL :: String -> Link -> ([H.Exp], [H.Name])
linkToURL res lnk = urlParts res lnk ([], [])

var :: String -> H.Exp
var = H.Var . H.UnQual . H.Ident

urlParts :: String -> Link -> ([H.Exp], [H.Name]) -> ([H.Exp], [H.Name])
urlParts res lnk ac@(rlnk, pars) =
  case lnk of
    [] -> ac
    (LResource r : a@(LAccess _) : xs)
      | not (hasParam a) -> urlParts res xs (rlnk ++ [H.Lit $ H.String r], pars)
      | otherwise -> urlParts res xs (rlnk', pars ++ [H.Ident . cleanHsName $ r])
           where rlnk' = rlnk ++ (H.List [H.Lit $ H.String $ r] : tailed)
                 tailed = [var "show" `H.App` (use $ hsName (cleanName r))]
    (LParam p : xs) -> urlParts res xs (rlnk ++ [var "show" `H.App` (use $ hsName (cleanName p))], pars)
    (i : xs) -> urlParts res xs (rlnk ++ [H.Lit $ H.String $ itemString i], pars)

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

dataName :: String -> String
dataName = modName

modName :: String -> String
modName = concatMap upFirst . cleanName

data InputInfo = InputInfo
  { inputModules     :: [H.ModuleName]
  , inputHaskellType :: H.Type
  , inputContentType :: String
  , inputFunc        :: String
  } deriving (Eq, Show)

inputInfo :: DataDesc -> InputInfo
inputInfo dsc =
  case L.get dataType dsc of
    String -> InputInfo [] (haskellStringType) "text/plain" "fromString"
    -- TODO fromJusts
    XML    -> InputInfo (L.get haskellModules dsc) (L.get haskellType dsc) "text/xml" "toXML"
    JSON   -> InputInfo (L.get haskellModules dsc) (L.get haskellType dsc) "text/json" "toJSON"
    File   -> InputInfo [] haskellByteStringType "application/octet-stream" "id"
    Other  -> InputInfo [] haskellByteStringType "text/plain" "id"

data ResponseInfo = ResponseInfo
  { responseModules     :: [H.ModuleName]
  , responseHaskellType :: H.Type
  , responseFunc        :: String
  } deriving (Eq, Show)

outputInfo :: ResponseType -> ResponseInfo
outputInfo r =
  case outputType r of
    Nothing -> ResponseInfo [] haskellUnitType "(const ())"
    Just t -> case L.get dataType t of
      String -> ResponseInfo [] haskellStringType "toString"
      XML    -> ResponseInfo (L.get haskellModules t) (L.get haskellType t) "fromXML"
      JSON   -> ResponseInfo (L.get haskellModules t) (L.get haskellType t) "fromJSON"
      File   -> ResponseInfo [] haskellByteStringType "id"
      Other  -> ResponseInfo [] haskellByteStringType "id"

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
      XML  -> Just $ ResponseInfo (L.get haskellModules t) (L.get haskellType t) "fromXML"
      JSON -> Just $ ResponseInfo (L.get haskellModules t) (L.get haskellType t) "fromJSON"
      _    -> Nothing

defaultErrorDataDesc :: DataType -> DataDesc
defaultErrorDataDesc dt =
  DataDesc
    { _dataType       = dt
    , _haskellType    = haskellVoidType
    , _haskellModules = [ModuleName "Rest.Types.Void"]
    }
