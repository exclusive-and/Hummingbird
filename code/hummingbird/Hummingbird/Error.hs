module Hummingbird.Error where

import Control.Exception (Exception)
import Data.Hashable
import Prelude
import Prettyprinter

import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var (Var)
import Hummingbird.Var qualified as Var

-- |
data Error
  = CannotParse !FilePath !Text
  | Elaboration !Elaboration
  deriving stock (Eq, Generic, Show)

instance Exception Error
instance Exception [Error]

-- | Report all errors directly to the standard output.
reportAll :: [Error] -> IO ()
reportAll = print . vcat . map pretty

-- |
data Elaboration
  = NotInScope !Name
  | AmbiguousNames !Name !Var
  deriving stock (Eq, Generic, Show)

instance Hashable Error
instance Hashable Elaboration

instance Pretty Error where
  pretty = \case
    CannotParse path msg ->
      hang 2 $ pretty msg
    Elaboration elab -> pretty elab

instance Pretty Elaboration where
  pretty = \case
    NotInScope name ->
      hang 2 $ "Not in scope:" <+> pretty name
    AmbiguousNames name other ->
      hang 2 $ "The name '" <> pretty name <> "' conflicts with an existing definition"
