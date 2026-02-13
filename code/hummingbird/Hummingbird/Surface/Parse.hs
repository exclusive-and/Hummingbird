{-# Language DataKinds #-}
{-# Language GADTs #-}
{-# Language OverloadedStrings #-}

module Hummingbird.Surface.Parse where

import Control.Applicative
import Control.Monad
import Data.Coerce (coerce)

import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Prelude
import Hummingbird.Surface
import Hummingbird.Surface.Located (Located)
import Hummingbird.Surface.Located qualified as Located
import Hummingbird.Surface.Parsec (
    P,
    parse,
    between,
    choice,
    expect,
    try,
  )
import Hummingbird.Surface.Token qualified as Token
import Hummingbird.Surface.Tokenize

hummingbirdP :: P (Module Name)
hummingbirdP = do
  modName <- nameP
  expect $ Token.Keyword Token.Module
  expect Token.Begin
  Module (coerce modName) <$> many (featherP <* expect Token.Newline)

featherP :: P (Declaration Name)
featherP = do
  name <- nameP
  choice [
      Fun name <$ expect Token.Equals <*> someTermsP
    , Sig name <$ expect Token.Colon <*> funTyP
    ]
{-
bindP :: Name -> P (Name)
bindP name =
  Bind name <$ expect Token.Equals <*> someTermsP
-}
someTermsP :: P (Term Name)
someTermsP = do
  xs <- some termP
  case xs of
    [x] -> pure x
    _   -> pure $ Concat xs

termsP :: P (Term Name)
termsP = do
  xs <- many termP
  case xs of
    [x] -> pure x
    _   -> pure $ Concat xs

termP :: P (Term Name)
termP = wordP <|> lambdaP <|> quotedP <|> parenP someTermsP

wordP :: P (Term Name)
wordP = Word <$> nameP

lambdaP :: P (Term Name)
lambdaP = do
  expect Token.Lambda
  bndr <- nameP
  expect Token.ArrowR
  Lambda bndr <$> someTermsP

quotedP :: P (Term Name)
quotedP = Quoted <$> bracketP termsP

funTyP :: P (Type Name)
funTyP = funTyP
{-
funTyP =
  FunTy <$> stackTyP <* expect Token.ArrowR <*> stackTyP

sigP :: Name -> P (Name)
sigP name =
  Sig name <$ expect Token.Colon <*> funTyP

typeP :: P (Type Name)
typeP = choice [
    VarTy <$> nameP
  , try $ parenP funTyP
  , try $ parenP $ ConcatTy <$> some typeP
  ]

stackTyP :: P (Type Name)
stackTyP = do
  name <- nameP
  expect (Token.Operator (Name.Name ".."))
  StackTy name <$> many typeP
-}
nameP :: P Name
nameP = unqualified

bracketP :: P a -> P a
bracketP = between (expect Token.BracketL) (expect Token.BracketR)

parenP :: P a -> P a
parenP = between (expect Token.ParenL) (expect Token.ParenR)
