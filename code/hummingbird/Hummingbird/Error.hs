{-# Language OverloadedStrings #-}

module Hummingbird.Error where

import Num
import Text qualified

import Control.Exception (Exception)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty))
import Prettyprinter qualified as Pretty

import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Prelude

data Error
  = Parse FilePath Text
  | DuplicateName !Name.Qualified Int
  | ImportNotFound !Name.Module Text
  | ModuleNotFound !Name.Module
  | DuplicateModules !Name.Module [FilePath]
  deriving (Generic, Eq, Show)

instance Pretty Error where
  pretty = \case
    Parse path msg ->
      Pretty.hang 2 $ pretty msg
    _ -> ""

instance Exception Error

newtype Errors = Errors [Error]
  deriving (Show)

instance Exception Errors
