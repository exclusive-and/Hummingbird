module Hummingbird.Surface.Parse where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Coerce (coerce)
import Data.Text qualified as Text
import Prelude
import Text.Parsec qualified

import Hummingbird.Error (Error)
import Hummingbird.Error qualified as Error
import Hummingbird.Literal (Literal)
import Hummingbird.Literal qualified as Literal
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Surface qualified as Surface
import Hummingbird.Surface.Located (Located)
import Hummingbird.Surface.Located qualified as Located
import Hummingbird.Surface.Parsec (
    P,
    -- parse,
    between,
    choice,
    expect,
    try,
  )
import Hummingbird.Surface.Parsec qualified as Parsec
import Hummingbird.Surface.Token
  ( Token
  , Layoutness (..)
  )
import Hummingbird.Surface.Token qualified as Token
import Hummingbird.Surface.Tokenize

parse ::
  (MonadError [Error] m)
  => FilePath
  -> [Located (Token 'NonLayout)]
  -> m (Surface.Module Name)
  
parse path tokens =
  case Parsec.parse hummingbirdP path tokens of
    Right parsed -> pure parsed
    Left errs -> throwError
      [Error.CannotParse path (Text.pack $ show errs)]

hummingbirdP :: P (Surface.Module Name)
hummingbirdP = do
  modName <- moduleNameP
  expect $ Token.Keyword Token.Module
  expect Token.Begin
  Surface.Module modName <$> many (featherP <* expect Token.Newline)

featherP :: P (Surface.Declaration Name)
featherP = do
  name <- nameP
  choice [
      Surface.Fun name <$ expect Token.Equals <*> someTermsP
    , Surface.Sig name <$ expect Token.Colon <*> funTyP
    ]
{-
bindP :: Name -> P (Name)
bindP name =
  Bind name <$ expect Token.Equals <*> someTermsP
-}
someTermsP :: P (Surface.Term Name)
someTermsP = do
  xs <- some termP
  case xs of
    [x] -> pure x
    _   -> pure $ Surface.Concat xs

termsP :: P (Surface.Term Name)
termsP = do
  xs <- many termP
  case xs of
    [x] -> pure x
    _   -> pure $ Surface.Concat xs

termP :: P (Surface.Term Name)
termP = choice [
    Surface.Lit <$> litP
  , Surface.Word <$> nameP
  , lambdaP
  , Surface.Quoted <$> bracketP termsP
  , parenP someTermsP
  ]

litP :: P Literal
litP = choice [
    Literal.Int <$> integerP
  ]

integerP :: P Integer
integerP = Parsec.tokenPrim go
  where
    go token =
      case Located.unLoc token of
        Token.Integer value -> Just value
        _                   -> Nothing

lambdaP :: P (Surface.Term Name)
lambdaP = do
  expect Token.Lambda
  bndr <- nameP
  expect Token.ArrowR
  Surface.Lambda bndr <$> someTermsP

funTyP :: P (Surface.Type Name)
funTyP = funTyP
{-
funTyP =
  FunTy <$> stackTyP <* expect Token.ArrowR <*> stackTyP

sigP :: Name -> P (Name)
sigP name =
  Sig name <$ expect Token.Colon <*> funTyP

typeP :: P (Surface.Type Name)
typeP = choice [
    VarTy <$> nameP
  , try $ parenP funTyP
  , try $ parenP $ ConcatTy <$> some typeP
  ]

stackTyP :: P (Surface.Type Name)
stackTyP = do
  name <- nameP
  expect (Token.Operator (Name.Name ".."))
  StackTy name <$> many typeP
-}

moduleNameP :: P Name.Module
moduleNameP = Parsec.tokenPrim go
  where
    go token =
      case Located.unLoc token of
        Token.Word name -> Just $ Name.Module name
        _               -> Nothing

nameP :: P Name
nameP = unqualified

bracketP :: P a -> P a
bracketP = between (expect Token.BracketL) (expect Token.BracketR)

parenP :: P a -> P a
parenP = between (expect Token.ParenL) (expect Token.ParenR)
