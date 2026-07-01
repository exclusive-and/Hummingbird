{-# Language TemplateHaskell #-}

module Hummingbird.Version
(
  Version (..),
  version,
  VersionCodename,
  GitRef,
  CommitDate,
  gitDescribeTH,
) where

import Data.Binary
import Data.Hashable
import Data.Text qualified as Text
import Prelude
import Prettyprinter

import Shellmet

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

version :: Version
version = Version "hummingbird" gitDescribeTH

data Version
  = Version !VersionCodename !(GitRef, CommitDate)
  deriving stock
    ( Eq
    , Generic
    , Ord
    , Show
    )
  deriving anyclass (Binary, Hashable)

type VersionCodename = Text

type GitRef = Text

type CommitDate = Text

gitDescribeTH :: (GitRef, CommitDate)
gitDescribeTH =
  $( runIO $ do
      date <- "git" $| ["show", "-s", "--format=%cs"] $? pure ""
      tag <- "git" $| ["describe", "--tags", "--always", "--dirty='"] $? pure "unknown"
      pure $ TupE [
          Just . LitE . StringL $ Text.unpack tag
        , Just . LitE . StringL $ Text.unpack date
        ]
    )

