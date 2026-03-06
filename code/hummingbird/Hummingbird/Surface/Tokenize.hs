module Hummingbird.Surface.Tokenize
(
  -- * Main interface
  MonadToken (..),
  tokenize,
  Tokenize,

  -- * Internal tokenizers
  alphanumeric,
  character,
  colon,
  doubleQuote,
  equals,
  newline,
  operator,
  singleQuote,
  special,
  symbol,
  underscore,
  unqualified,
  visible,
  word,
)
where

import Birds.Prelude

import Control.Applicative
import Control.Monad.Chronicle
import Data.Char
import Data.Functor
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec qualified

import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Surface.Located (Located (..))
import Hummingbird.Surface.Located qualified as Located
import Hummingbird.Surface.Parsec (
    P,
    parse,
    Parsec,
    ParseError,
    anyChar,
    between,
    char,
    choice,
    digit,
    endOfLine,
    getInput,
    getPosition,
    letter,
    located,
    notFollowedBy,
    oneOf,
    satisfy,
    sourceColumn,
    space,
    spaces,
    unexpected,
  )
import Hummingbird.Surface.Parsec qualified as Parsec
import Hummingbird.Surface.Token

-- | Monad capable of lexical analysis on a hidden input string.
class (Monad m) => MonadToken token m where
  {-# MINIMAL (tokenRun | tokenLex), tokensLex #-}

  -- | Run a continuation on the next token in the input.
  tokenRun :: (Located token -> m a) -> m a
  tokenRun cont =
    tokenLex >>= cont

  {-# INLINE tokenRun #-}

  -- | Get the next token from input.
  tokenLex :: m (Located token)
  tokenLex =
    tokenRun pure

  -- | Convert the whole input into its lexical tokens all at once.
  tokensLex :: m [Located token]

instance MonadToken (Token 'NonLayout) P where
  tokenLex = Parsec.anyToken
  tokensLex = Parsec.getInput

-- | A simple tokenizer monad backed by 'Parsec'.
type Tokenize = Parsec Text ()

instance MonadToken (Token 'Layout) Tokenize where
  tokenLex = choice [
      located visible,
      newline,
      space *> tokenLex
    ]

  tokensLex = reverse <$> go []
    where
      go xs = do
        input <- getInput
        if Text.null input then
          pure xs
        else do
          token <- tokenRun pure
          go (token : xs)

-- | Convert a raw text input into a stream of tokens.
tokenize ::
  (MonadChronicle ParseError m)
  => Int
  -> FilePath
  -> Text
  -> m [Located (Token 'Layout)]

tokenize line path text =
  case parse tokensLex path text of
    Left errs -> confess errs
    Right result -> pure result

{-# INLINE tokenize #-}

-- | Newlines and indentation.
newline :: Tokenize (Located (Token 'Layout))
newline = do
  void endOfLine
  void spaces
  located (Indent . sourceColumn <$> getPosition)

-- | Visible tokens.
visible :: Tokenize (Token 'Layout)
visible = choice [
    operator
  , number
  , word
  , character
  , BracketL <$ char '['
  , BracketR <$ char ']'
  , Comma <$ char ','
  , Lambda <$ char '\\'
  , ParenL <$ char '('
  , ParenR <$ char ')'
  ]

character :: Tokenize (Token 'Layout)
character = between singleQuote singleQuote $ do
  Character <$> choice [
      char '\n' *> unexpected ""
    , singleQuote *> unexpected ""
    , char '\\' *> escape
    , anyChar
    ]
  where
    escape = choice [
        oneOf "\\\"\'"
      , '\a' <$ char 'a'
      , '\b' <$ char 'b'
      , '\f' <$ char 'f'
      , '\n' <$ char 'n'
      , '\r' <$ char 'r'
      , '\t' <$ char 't'
      , '\v' <$ char 'v'
      , space <* spaces
      ]

colon :: Tokenize (Token 'Layout)
colon = Colon <$ char ':' <* notFollowedBy symbol

equals :: Tokenize (Token 'Layout)
equals = Equals <$ char '=' <* notFollowedBy symbol

number :: Tokenize (Token 'Layout)
number = do
  sign <- Text.Parsec.optionMaybe $ Parsec.oneOf "+-"
  value <- some Parsec.digit
  pure $ case sign of
    Nothing  -> Integer $ read value
    Just '+' -> Integer $ read value
    Just '-' -> Integer $ negate $ read value

operator :: Tokenize (Token 'Layout)
operator = do
  str <- Text.pack <$> some symbol
  pure $ case str of
    "<-" -> ArrowL
    "->" -> ArrowR
    ":" -> Colon
    "=" -> Equals
    _ -> Operator (Name.Name str)

word :: Tokenize (Token 'Layout)
word = do
  str <- alphanumeric
  pure $ case str of
    "case" -> SpecialWord Case
    "class" -> SpecialWord Class
    "data" -> SpecialWord Data
    "do" -> SpecialWord Do
    "in" -> SpecialWord In
    "instance" -> SpecialWord Instance
    "let" -> SpecialWord Let
    "module" -> SpecialWord Module
    "of" -> SpecialWord Of
    "record" -> SpecialWord Record
    "where" -> SpecialWord Where
    _ -> Word str

alphanumeric :: Tokenize Text
alphanumeric = do
  x <- letter <|> underscore
  xs <- many (letter <|> underscore <|> digit)
  pure $ Text.pack $ x : xs

doubleQuote :: Tokenize Char
doubleQuote = char '\"'

singleQuote :: Tokenize Char
singleQuote = char '\''

special :: Tokenize Char
special = oneOf "\"'(),[\\]_{}"

symbol :: Tokenize Char
symbol = do
  notFollowedBy special
  choice [
      Text.Parsec.satisfy isSymbol
    , Text.Parsec.satisfy isPunctuation
    ]

underscore :: Tokenize Char
underscore = char '_'

unqualified :: P Name
unqualified = Parsec.tokenPrim go
  where
    go token =
      case Located.unLoc token of
        Word name -> Just $ Name.Name name
        _         -> Nothing
