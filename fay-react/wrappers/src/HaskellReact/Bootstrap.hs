{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap where

import "fay-base" FFI (ffi, Automatic, Defined(Defined, Undefined), Nullable(Null))
import "fay-base" Data.Text (fromString, Text, showInt, (<>))
import "fay-base" Prelude

import HaskellReact hiding (row, col', col)

data ReactBootstrap
instance CommonJSModule ReactBootstrap

requireReactBootstrap :: ReactBootstrap
requireReactBootstrap = ffi " require('react-bootstrap') "

-- | Creates an instance of a React Bootstrap class
reactBootstrap :: (Renderable b)
               => Text -- ^ The name of the Bootstrap class
               -> a -- ^ The props passed to the instance
               -> b -- ^ The children passed to the instance
               -> DOMElement
reactBootstrap = foreignReact requireReactBootstrap

data Technical1 = Technical1 {
  role :: Text }

data TooltipData = TooltipData {
  data_toggle :: Text ,
  data_placement :: Defined Text ,
  title :: Defined Text ,
  href :: Defined Text }

pillData :: TooltipData
pillData = TooltipData "pill" Undefined Undefined Undefined

technical1 :: Technical1
technical1 = Technical1 "navigation"

pill ::
  Renderable a
  => Attributes
  -> Text
  -> a
  -> DOMElement
pill attrs aria child = constructDOMElement "a" attrs (pillData { href = Defined $ "#" <> aria }) child

tooltip :: Renderable a
        => TooltipData
        -> a
        -> DOMElement
tooltip tooltipData child = constructDOMElement "span" mkAttrs tooltipData child

navBar' :: Renderable a
        => (Attributes -> Attributes)
        -> a
        -> DOMElement
navBar' modifyAttrs children = 
  constructDOMElement "nav" (modifyAttrs $ class'' ["navbar", "navbar-default"]) technical1 $
    div' (class' "container") children

navBar :: Renderable a
       => a
       -> DOMElement
navBar children = navBar' Prelude.id children

nav :: Renderable a
    => a 
    -> DOMElement
nav children = ul' (class'' ["nav", "navbar-nav"]) children

table :: Renderable a
      => Automatic a
      -> DOMElement
table = reactBootstrap "Table" Null

row' :: Renderable a
     => (Attributes -> Attributes)
     -> a -- ^ children
     -> DOMElement
row' modifyAttributes children = div' (modifyAttributes $ class' "row") children

row'' ::
  Renderable a =>
  [Text] ->
  a -> -- ^ children
  DOMElement
row'' classes children = div' (class'' $ "row" : classes) children

row :: Renderable a
    => a
    -> DOMElement
row = row' Prelude.id

jumbotron :: Renderable a
          => Automatic a
          -> DOMElement
jumbotron = reactBootstrap "Jumbotron" Null

grid :: Renderable a
     => Automatic a
     -> DOMElement
grid = reactBootstrap "Grid" Null

data ColProps = ColProps {
  md :: Int , 
  mdOffset :: Defined Int }

mkColProps :: Int -> ColProps
mkColProps int = ColProps int Undefined

col' :: Renderable a
     => ColProps
     -> Defined Text
     -> a
     -> DOMElement
col' colProps key' = let
  md' = ["col-md-" <> (showInt $ md colProps)]
  mdOffset' = case mdOffset colProps of
    Defined int -> ["col-md-offset-" <> showInt int]
    _ -> []
  bothClasses = mdOffset' ++ md'
  in div' ((class'' bothClasses) {key = key' })

col :: Renderable a
    => ColProps
    -> a
    -> DOMElement
col props children = col' props Undefined children

colSize ::
  Renderable a =>
  Int ->
  a ->
  DOMElement
colSize size = col (mkColProps size) 

fullCol :: Renderable a => a -> DOMElement
fullCol = col (mkColProps 12)

fullRow :: Renderable a => a -> DOMElement
fullRow = row . fullCol

panel' ::
  Renderable a => 
  [Text] ->
  a ->
  DOMElement
panel' = panel'' []

panel'' ::
  Renderable a =>
  [Text] ->
  [Text] ->
  a ->
  DOMElement
panel'' bodyStyles style' children = div' (class'' $ ("panel" : style')) (div' (class'' $ "panel-body" : bodyStyles) children)

panel ::
  Renderable a =>
  a ->
  DOMElement
panel = panel' ["panel-default"]
