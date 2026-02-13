{-# Language DataKinds #-}
{-# Language GADTs #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}

module Hummingbird.Surface.Parsec
(
  -- * Main Parsec interface
  P,
  Parsec.parse,
  Parsec,
  Parsec.runParser,
  Parsec.runParserT,
  ParsecT,

  -- * Combinators
  Parsec.anyChar,
  Parsec.anyToken,
  Parsec.between,
  Parsec.char,
  Parsec.choice,
  Parsec.digit,
  Parsec.endOfLine,
  Parsec.eof,
  expect,
  Parsec.letter,
  Parsec.notFollowedBy,
  Parsec.oneOf,
  satisfy,
  Parsec.space,
  Parsec.spaces,
  tokenPrim,
  Parsec.try,
  Parsec.unexpected,

  -- * Location
  located,
  point,
  pos,
  range,
  beginningOf,
  endingOf,
  -- ** Internal to Parsec
  Column,
  Line,
  SourceName,
  SourcePos,
  Parsec.getPosition,
  Parsec.setPosition,
  Parsec.setSourceColumn,
  Parsec.setSourceLine,
  Parsec.setSourceName,
  sourceColumn,
  sourceLine,
  sourceName,

  -- * State
  Parsec.getInput,
  Parsec.getState,
  Parsec.modifyState,
  Parsec.putState,

  -- * Errors
  Parsec.ParseError,
  Parsec.errorPos,

  -- * Parsec input stream
  Parsec.Stream,
)
where

import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec (Parsec, ParsecT)
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos (
    Column,
    Line,
    SourceName,
    SourcePos,
    newPos,
    sourceColumn,
    sourceLine,
    sourceName,
  )
import Text.Parsec.Pos qualified as Parsec

import Hummingbird.Name (Name)
import Hummingbird.Prelude
import Hummingbird.Surface.Located (
    Located (At),
    unLoc,
    Span (Span),
  )
import Hummingbird.Surface.Located qualified as Located
import Hummingbird.Surface.Token as Token

type P = Parsec [Located (Token 'NonLayout)] ()

located ::
  (Monad m)
  => ParsecT s u m a
  -> ParsecT s u m (Located a)
located p = do
  begin <- Parsec.getPosition
  a <- p
  end <- Parsec.getPosition
  pure $ At (range begin end) a

point :: SourceName -> Line -> Column -> Span
point name line column =
  let
    beginLine = line
    beginColumn = column
    endLine = line
    endColumn = column
  in
    Span {name = Text.pack name, ..}

pos :: SourcePos -> Span
pos = point <$> sourceName <*> sourceLine <*> sourceColumn

range :: SourcePos -> SourcePos -> Span
range begin end =
  let
    beginLine = sourceLine begin
    beginColumn = sourceColumn begin
    endLine = sourceLine end
    endColumn = sourceColumn end
  in
    Span{name = Text.pack (sourceName begin), ..}

fromLocated :: Located a -> (a, SourcePos, SourcePos)
fromLocated (At Span{..} a) =
  let
    begin = newPos (Text.unpack name) beginLine beginColumn
    end = newPos (Text.unpack name) endLine endColumn
  in
    (a, begin, end)

beginningOf :: Span -> SourcePos
beginningOf Span{..} =
  newPos (Text.unpack name) beginLine beginColumn

endingOf :: Span -> SourcePos
endingOf Span{..} =
  newPos (Text.unpack name) endLine endColumn

expect :: Token 'NonLayout -> P (Located (Token 'NonLayout))
expect token = satisfy ((== token) . unLoc)

satisfy :: (Located (Token 'NonLayout) -> Bool) -> P (Located (Token 'NonLayout))
satisfy predicate = tokenPrim
  (\token -> if predicate token then Just token else Nothing)

tokenPrim :: (Located (Token 'NonLayout) -> Maybe a) -> P a
tokenPrim = Parsec.tokenPrim show advance
  where
    advance sourcePos _ tokens =
      case tokens of
        [] -> sourcePos
        At origin _ : _ -> beginningOf origin
