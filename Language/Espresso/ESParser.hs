module Language.Espresso.ESParser where

import Text.Trifecta.Parser
import Text.Trifecta (position)
import Text.Trifecta.Delta (Delta(..), column)
import Text.Parser.Combinators (unexpected, notFollowedBy, eof, (<?>), try, many)
import Text.Parser.Token (Unlined(..), symbol, whiteSpace)
import Text.Parser.Char (char)
import Data.HashSet
import Control.Monad (unless, when, void)
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative ((<|>))
import Language.Espresso.AST (Identifier)
import Text.PrettyPrint.ANSI.Leijen (putDoc, pretty, Pretty)

import Data.Int

type ESParser = StateT ESState Parser

data ESState = ESState {
        indentStack :: [Int64]
    ,   context :: HashSet Identifier
    } deriving (Show, Eq)

initESState :: ESState
initESState = ESState [0] mempty

runESParserTest :: (MonadIO m, Show a) => ESParser a -> String -> m ()
runESParserTest p = parseTest $ evalStateT p initESState

runESParserTestFile :: (MonadIO m, Pretty a) => ESParser a -> String -> m ()
runESParserTestFile p fp = do
    (Just r) <- parseFromFile (evalStateT p initESState) fp
    liftIO (putDoc $ pretty r)

blockStart :: ESParser ()
blockStart = do
    p <- position
    modify $ \ s -> s { indentStack = column p : indentStack s }

lineCont :: ESParser ()
lineCont = do
    (i : is) <- gets indentStack
    p <- position
    when (column p <= i) (unexpected $ "line continue indentation" ++ show i)
    <?> "line continue"

lineEnd :: ESParser ()
lineEnd = do
    (i : is) <- gets indentStack
    p <- position
    when (column p /= i) (unexpected $ "line end indentation" ++ show i)
    <|> void (symbol ";")
    <?> "line end"

blockEnd :: ESParser ()
blockEnd = do
    (i : is) <- gets indentStack
    p <- position
    if column p > i then unexpected $ "block end indentation" ++ show i
                    else modify $ \ s -> s { indentStack = is }

blockEnd' :: ESParser ()
blockEnd' = do
    modify $ \ s -> s { indentStack = tail (indentStack s) }

lineNumber :: Delta -> Int64
lineNumber (Columns _ _)  = 0
lineNumber (Tab _ _ _) = 0
lineNumber (Lines l _ _ _) = l
lineNumber (Directed _ l _ _ _) = l
