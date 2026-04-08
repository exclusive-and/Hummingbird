{-# Language NamedFieldPuns #-}

module Hummingbird.Surface.Layoutize where

import Control.Applicative
import Control.Monad
import Control.Monad.Chronicle
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Prelude
import Text.Parsec qualified

import Hummingbird.Error (Error)
import Hummingbird.Error qualified as Error
import Hummingbird.Surface.Located (Located)
import Hummingbird.Surface.Located qualified as Located
import Hummingbird.Surface.Parsec hiding (anyToken, tokenPrim)
import Hummingbird.Surface.Parsec qualified as Parsec
import Hummingbird.Surface.Token as Token
import Hummingbird.Surface.Tokenize (MonadToken (..))

-- | The lexical layout parsing monad.
type Layoutize =
  Parsec [Located (Token 'Layout)] LayoutState

instance MonadToken (Token 'NonLayout) Layoutize where
  tokenLex = do
    state <- popState
    let
      newline' = do
        Located.At loc _ <- located indentation
        pushState (DoNewline loc)
        tokenLex
    case state of
      Zero -> layoutKeyword <|> anyToken <|> newline'
      DoNewLayoutContext -> newLayoutContext
      DoNewline loc -> newline loc
      DoEmptyLayout loc -> pure $ Located.At loc End

  tokensLex = reverse <$> go []
    where
      go xs = do
        input <- getInput
        if null input then
          pure xs
        else do
          token <- tokenRun @(Token 'NonLayout) pure
          go (token : xs)

instance MonadToken (Token 'Layout) Layoutize where
  tokenLex = Parsec.anyToken
  tokensLex = getInput

layoutize ::
  (MonadError [Error] m)
  => [Keyword]
  -> FilePath
  -> [Located (Token 'Layout)]
  -> m [Located (Token 'NonLayout)]

layoutize kws path tokens =
  case runParser tokensLex (initLayoutState kws) path tokens of
    Right tokens -> pure tokens
    Left errs -> throwError
      [Error.CannotParse path (Text.pack $ show errs)]

defaultKws :: [Keyword]
defaultKws = [Module]

layoutKeyword :: Layoutize (Located (Token 'NonLayout))
layoutKeyword = do
  kws <- getLayoutKeywords
  let
    go (Located.At loc token) = case token of
      SpecialWord kw ->
        if kw `elem` kws then
          Just $ Located.At loc (Keyword kw)
        else
          Nothing
      _ -> Nothing
  kw <- tokenPrim go
  pushState DoNewLayoutContext
  pure kw

newLayoutContext :: Layoutize (Located (Token 'NonLayout))
newLayoutContext = do
  Located.At loc _ <- located indentation
  ctx <- getContext
  indent <- getIndentLevel
  case ctx of
    n:_ | indent <= n -> do
      pushState (DoEmptyLayout loc)
      pure $ Located.At loc Begin
    _ -> do
      pushContext indent
      pure $ Located.At loc Begin

newline :: Located.Span -> Layoutize (Located (Token 'NonLayout))
newline loc = do
  indent <- compareIndent
  case indent of
    GT -> anyToken
    EQ -> pure (Located.At loc Newline)
    LT -> do
      popContext
      pushState (DoNewline loc)
      pure (Located.At loc End)

compareIndent :: Layoutize Ordering
compareIndent = do
  ctx <- getContext
  case ctx of
    []  -> pure GT
    n:_ -> flip compare n <$> getIndentLevel
  
indentation :: Layoutize ()
indentation = do
  let
    go :: Located (Token 'Layout) -> Maybe Int
    go token =
      case Located.unLoc token of
        Indent n -> Just n
        _        -> Nothing
  Located.At loc n <- located $ tokenPrim go
  setIndentLevel n

data LayoutState = LayoutState
  {
    control :: NonEmpty LayoutControl
  , context :: [Int]
  , indentLevel :: Int
  , layoutKeywords :: [Keyword]
  }

data LayoutControl
  = Zero
  | DoNewLayoutContext
  | DoNewline Located.Span
  | DoEmptyLayout Located.Span

popState :: Layoutize LayoutControl
popState = do
  LayoutState{control} <- getState
  case NonEmpty.uncons control of
    (x, Nothing) -> pure x
    (x, Just xs) -> do
      modifyState (\state -> state{control = xs})
      pure x

pushState :: LayoutControl -> Layoutize ()
pushState x = modifyState
  (\state -> state{control = x `NonEmpty.cons` control state})

getContext :: Layoutize [Int]
getContext = do
  LayoutState{context} <- getState
  pure context

popContext :: Layoutize ()
popContext = modifyState
  (\state -> state{context = drop 1 $ context state})

pushContext :: Int -> Layoutize ()
pushContext x = modifyState
  (\state -> state{context = x : context state})

getIndentLevel :: Layoutize Int
getIndentLevel = indentLevel <$> getState

setIndentLevel :: Int -> Layoutize ()
setIndentLevel n = modifyState
  (\state -> state{indentLevel = n})

getLayoutKeywords :: Layoutize [Keyword]
getLayoutKeywords = layoutKeywords <$> getState

initLayoutState :: [Keyword] -> LayoutState
initLayoutState layoutKeywords =
  let
    control = Zero NonEmpty.:| []
    context = []
    indentLevel = 0
  in
    LayoutState{control, context, indentLevel, layoutKeywords}

anyToken :: Layoutize (Located (Token 'NonLayout))
anyToken = tokenPrim
  (\(Located.At loc token) -> Located.At loc <$> Token.fromLayout token)

satisfy ::
  (Show token, Monad m)
  => (Located token -> Bool)
  -> ParsecT [Located token] u m (Located token)

satisfy predicate = tokenPrim
  (\token -> if predicate token then Just token else Nothing)

tokenPrim ::
  (Show token, Monad m)
  => (Located token -> Maybe a)
  -> ParsecT [Located token] u m a

tokenPrim = Text.Parsec.tokenPrim show advance
  where
    advance sourcePos _ tokens =
      case tokens of
        [] -> sourcePos
        Located.At origin _ : _ -> beginningOf origin
