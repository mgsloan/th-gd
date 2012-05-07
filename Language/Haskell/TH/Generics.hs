{-# LANGUAGE TemplateHaskell, TypeFamilies, EmptyDataDecls #-}

module Language.Haskell.TH.Generics
  (module D) where

import qualified Generics.Deriving as D

import Generics.Deriving.TH (deriveMeta)
import Control.Applicative  ((<$>))

import Language.Haskell.TH.Syntax

$(concat <$> mapM deriveMeta
  [ ''(,)
  , ''(,,)
  , ''Body
  , ''Callconv
  , ''Clause
  , ''Con
  , ''Dec
  , ''Exp
  , ''FamFlavour
  , ''Fixity
  , ''FixityDirection
  , ''Foreign
  , ''FunDep
  , ''Guard
  , ''Info
  , ''InlineSpec
  , ''Kind
  , ''Lit
  , ''Match
  , ''Pat
  , ''Pragma
  , ''Pred
  , ''Range
  , ''Safety
  , ''Stmt
  , ''Strict
  , ''Type
  , ''TyVarBndr
  ])