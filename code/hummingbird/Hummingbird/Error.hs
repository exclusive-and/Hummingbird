{-# Language OverloadedStrings #-}

module Hummingbird.Error where

import Control.Exception (Exception)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty))
import Prettyprinter qualified as Pretty

import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Prelude
import Hummingbird.Rename (RenameMessage)

data Error
  = Parse FilePath Text
  | Rename !Name.Module RenameMessage
  | DuplicateName !Name.Qualified Int
  | ImportNotFound !Name.Module Text
  | ModuleNotFound !Name.Module
  | DuplicateModules !Name.Module [FilePath]
  deriving (Generic, Eq, Show)

instance Exception Error

instance Pretty Error where
  pretty = \case
    Parse path msg ->
      Pretty.hang 2 $ pretty msg
    Rename modName msg ->
      Pretty.hang 2 $ Pretty.vcat [pretty modName, pretty msg]
    _ -> ""

newtype Errors = Errors [Error]
  deriving (Show)

instance Exception Errors
