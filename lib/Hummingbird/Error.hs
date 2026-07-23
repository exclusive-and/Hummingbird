module Hummingbird.Error where

import Control.Exception (Exception)
import Data.Binary
import Data.ContentAddress
import Data.Hashable
import Data.Text qualified as Text
import Prelude
import Prettyprinter

import Hummingbird.Codebase.Hash
import Hummingbird.Codebase.Id
import Hummingbird.Elaboration.Var (Var)
import Hummingbird.Name as Name
import Hummingbird.Surface qualified as Surface

-- |
data Error
  = RawMessage !Text
  | CannotParse !FilePath !Text
  | Elaboration !Elaboration
  deriving
    ( Eq
    , Generic
    , Show
    )
  deriving anyclass (Binary, Hashable)

instance Exception Error
instance Exception [Error]

-- | Report all errors directly to the standard output.
reportAll :: [Error] -> IO ()
reportAll = print . vcat . map pretty

-- |
data Elaboration
  = NotInScope !Name
  | AmbiguousNames !Name !Var
  deriving
    ( Eq
    , Generic
    , Show
    )
  deriving anyclass (Binary, Hashable)

instance Pretty Error where
  pretty = \case
    RawMessage msg -> hang 2 $ vcat $ pretty <$> Text.lines msg
    CannotParse path msg ->
      hang 2 $ pretty msg
    Elaboration elab -> pretty elab

instance Pretty Elaboration where
  pretty = \case
    NotInScope name ->
      hang 2 $ "Not in scope:" <+> pretty name
    AmbiguousNames name other ->
      hang 2 $ "The name '" <> pretty name <> "' conflicts with an existing definition"
