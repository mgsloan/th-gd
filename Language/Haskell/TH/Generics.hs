{-# LANGUAGE
    DeriveGeneric
  , EmptyDataDecls
  , StandaloneDeriving
  , TemplateHaskell
  , TypeFamilies
  #-}

module Language.Haskell.TH.Generics
  (module Generics.Deriving) where

import Generics.Deriving

import Generics.Deriving.TH (deriveMeta)
import Control.Applicative  ((<$>))

import qualified Language.Haskell.TH.Syntax as TH

deriving instance (Generic TH.Body)
deriving instance (Generic TH.Callconv)
deriving instance (Generic TH.Clause)
deriving instance (Generic TH.Con)
deriving instance (Generic TH.Dec)
deriving instance (Generic TH.Exp)
deriving instance (Generic TH.FamFlavour)
deriving instance (Generic TH.Fixity)
deriving instance (Generic TH.FixityDirection)
deriving instance (Generic TH.Foreign)
deriving instance (Generic TH.FunDep)
deriving instance (Generic TH.Guard)
deriving instance (Generic TH.Info)
deriving instance (Generic TH.InlineSpec)
deriving instance (Generic TH.Kind)
deriving instance (Generic TH.Lit)
deriving instance (Generic TH.Match)
deriving instance (Generic TH.Pat)
deriving instance (Generic TH.Pragma)
deriving instance (Generic TH.Pred)
deriving instance (Generic TH.Range)
deriving instance (Generic TH.Safety)
deriving instance (Generic TH.Stmt)
deriving instance (Generic TH.Strict)
deriving instance (Generic TH.Type)
deriving instance (Generic TH.TyVarBndr)

$(concat <$> mapM deriveMeta
  [ ''(,)
  , ''(,,)
  , ''TH.Body
  , ''TH.Callconv
  , ''TH.Clause
  , ''TH.Con
  , ''TH.Dec
  , ''TH.Exp
  , ''TH.FamFlavour
  , ''TH.Fixity
  , ''TH.FixityDirection
  , ''TH.Foreign
  , ''TH.FunDep
  , ''TH.Guard
  , ''TH.Info
  , ''TH.InlineSpec
  , ''TH.Kind
  , ''TH.Lit
  , ''TH.Match
  , ''TH.Pat
  , ''TH.Pragma
  , ''TH.Pred
  , ''TH.Range
  , ''TH.Safety
  , ''TH.Stmt
  , ''TH.Strict
  , ''TH.Type
  , ''TH.TyVarBndr
  ])