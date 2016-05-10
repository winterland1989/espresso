{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Espresso.Parser where

import Control.Monad
import Control.Applicative ((<|>))
import Text.Parser.Combinators
import qualified Text.Parser.Char as C
import Text.Parser.Expression
import Text.Parser.LookAhead
import qualified Text.Parser.Token as T
import qualified Text.Parser.Token.Highlight as T
import qualified Text.Parser.Token.Style as TS

import Language.Espresso.AST
import Data.HashSet (member)
import Data.List (foldl')

import Data.Char (isSpace)

import Language.Espresso.ESParser


--------------------------------------------------------------------------------

reservedWords = [
        "break", "case", "catch", "const", "continue", "debugger",
        "default", "delete", "do", "else", "enum", "false", "finally",
        "for", "function", "if", "instanceof", "in", "let", "new",
        "null", "return", "switch", "this", "throw", "true", "try",
        "typeof", "var", "void", "while", "with"
      ]

-- | identifier style
identStyle :: T.IdentifierStyle ESParser
identStyle = T.IdentifierStyle
    { T._styleName = "identifier"
    , T._styleStart = C.letter <|> C.oneOf "_$"
    , T._styleLetter = C.alphaNum <|> C.oneOf "_$"
    , T._styleReserved = reservedWords
    , T._styleHighlight = T.Identifier
    , T._styleReservedHighlight = T.ReservedIdentifier
    }

-- | eat a reserved word
reserve :: String -> ESParser ()
reserve w = T.reserve identStyle w <?> "reserved word"

-- | Single quoted string literal
stringLit :: ESParser Expr
stringLit = StringLit <$> T.stringLiteral' <?> "single quoted string"

-- | number
numLit :: ESParser Expr
numLit = NumLit <$> T.integerOrDouble <?> "number"

-- | boolean
boolLit :: ESParser Expr
boolLit = BoolLit True <$ reserve "true"
    <|> BoolLit False <$ reserve "false"
    <?> "bool"

-- | void
voidLit :: ESParser Expr
voidLit = VoidLit <$ reserve "void" <?> "void"

-- | null
nullLit :: ESParser Expr
nullLit = VoidLit <$ reserve "null" <?> "null"

-- | identifier lexme
ident :: ESParser Identifier
ident = T.ident identStyle <?> "identifier"

identExpr :: ESParser Expr
identExpr = IdentExpr <$> ident

-- | key for object, can be reserved word
objectKey :: ESParser Expr
objectKey = do
    s <- some $ C.satisfy $ \ c -> not (isSpace c) && c /= '.' && c /= '['  && c /= ':'
    return (StringLit s)

--------------------------------------------------------------------------------

-- | Array pattern: [x, y = 2, ...]
arrayPat :: ESParser Pat
arrayPat = do
    i <- optional (try $ ident <* T.symbol "@")
    ps <- T.brackets (T.commaSep1 pat)
    d <- optional (T.symbol "=" *> expr)
    return (PatArray i ps d)

-- | Object pattern: {x, y: y2, z: z2 = 0, a = 1, ...}
objectPat :: ESParser Pat
objectPat = do
    i <- optional (try $ ident <* T.symbol "@")
    ps <- T.braces . T.commaSep1 $ do
        key <- optional $ try $
            (objectKey <* T.whiteSpace <|> T.brackets expr) <* T.symbol ":"
        case key of
            Just key -> do
                p <- pat
                return (key, p)
            _        -> do
                p@(PatIdent i _) <- identPat
                return (StringLit i, p)
    d <- optional (T.symbol "=" *> expr)
    return (PatObject i ps d)

-- | default pattern helper: x = 0
identPat :: ESParser Pat
identPat = do
    i <- ident
    d <- optional (T.symbol "=" *> expr)
    return (PatIdent i d)

-- | es6 style pattern
pat :: ESParser Pat
pat = arrayPat <|> objectPat <|> (PatIdent <$> ident <*> pure Nothing) <?> "pattern"

--------------------------------------------------------------------------------

functionExpr :: ESParser Expr
functionExpr = do
    argsPat <- T.symbol "\\" *> T.commaSep pat <* T.symbol "->"
    lineCont
    blockStart
    d <- decl `sepBy` lineEnd
    blockEnd'
    return (FunctionExpr argsPat d)
    <?> "function"

--------------------------------------------------------------------------------

arrayExpr :: ESParser Expr
arrayExpr = ArrayExpr <$> T.brackets (T.commaSep expr) <?> "array expression"

kvPair :: ESParser (Expr, Expr)
kvPair = do
    key <- identExpr
    _ <- T.symbol ":"
    value <- expr
    return (key, value)

objectExpr :: ESParser Expr
objectExpr = ObjectExpr <$> T.braces (T.commaSep kvPair) <?> "object expression"

--------------------------------------------------------------------------------

opStyle :: T.IdentifierStyle ESParser
opStyle = T.IdentifierStyle
    { T._styleName = "operator"
    , T._styleStart = T._styleLetter opStyle
    , T._styleLetter = C.oneOf ":!#$%&*+./<=>?@^|-~)]}"
    , T._styleReserved = [
        "|=", "^=", "&=", "<<=", ">>=", ">>>=", "+=", "-=", "*=", "/=",
        "%=", "=", ";", ",", "?", ":", "||", "&&", "|", "^", "&",
        "===", "==", "=", "!==", "!=", "<<", "<=", "<", ">>>", ">>",
        ">=", ">", "++", "--", "+", "-", "*", "/", "%", "!", "~", ".",
        "[", "]", "{", "}", "(", ")","</","instanceof"
      ]
    , T._styleHighlight = T.Operator
    , T._styleReservedHighlight = T.ReservedOperator
    }


simpleExpr :: ESParser Expr
simpleExpr =  parens
    <|> arrayExpr <|> objectExpr
    <|> numLit <|> stringLit <|> boolLit <|> nullLit <|> voidLit
    <|> T.runUnspaced (T.Unspaced identExpr) <|> functionExpr
    <|> doExpr
  where
    parens = do
        es <- T.parens (T.commaSep expr)
        case es of
            [e] -> return e
            _ -> return (MultiExpr es)

simpleExpr' :: ESParser Expr
simpleExpr' =  do
    e <- simpleExpr
    accessors <- many (C.char '.' *> objectKey <|> between (T.symbolic '[') (C.char ']') expr)
    return $ foldl' (InfixExpr InfixOpAccess) e accessors

expr :: ESParser Expr
expr = buildExpressionParser table simpleExpr' <?> "expression"
  where
    applyOp = lineCont *> notFollowedBy
        (void (T._styleLetter opStyle) <|> lineEnd <|> blockEnd)

    operator op = lineCont *> T.reserve opStyle op <* lineCont

    rightPipe = Infix (ApplyExpr <$ operator "<|") AssocRight
    leftPipe = Infix (flip <$> (ApplyExpr <$ operator "|>")) AssocLeft


    infL  name op  = Infix (InfixExpr op <$ operator name) AssocLeft
    infR  name op  = Infix (InfixExpr op <$ operator name) AssocRight
    infN  name op  = Infix (InfixExpr op <$ operator name) AssocNone
    pre   name op  = Prefix (PrefixExpr op <$ operator name)
    post  name op  = Postfix (PostfixExpr op <$ operator name)
    table =
        [
            [ Infix (ApplyExpr <$ applyOp) AssocLeft ]
        ,   [ infR "<<" InfixOpCompose ]

        ,   [ pre "new " PrefixOpNew ]

        ,   [ pre "-" PrefixOpNeg,       pre "+" PrefixOpPos ]
        ,   [ pre "++" PrefixOpIncr,     post "++" PostfixOpIncr ]
        ,   [ pre "--" PrefixOpIncr,     post "--" PostfixOpIncr ]

        ,   [ infL "*" InfixOpMul,       infL "/" InfixOpDiv ]
        ,   [ infL "+" InfixOpAdd,       infL "-" InfixOpSub ]

        ,   [ infL "<<" InfixOpLshift,   infL ">>" InfixOpRshift,   infL ">>>" InfixOpURshift ]
        ,   [ infL "<" InfixOpLt,        infL "<=" InfixOpLe
            , infL ">" InfixOpGt,        infL ">=" InfixOpGe
            ]

        ,   [ infN "instanceof" InfixOpInstanceOf, infN "in" InfixOpIn ]

        ,   [ infL "==" InfixOpEq, infL "!=" InfixOpNeq ]

        ,   [ infL "&" InfixOpBitAnd ]
        ,   [ infL "^" InfixOpBitXor ]
        ,   [ infL "|" InfixOpBitOr ]
        ,   [ infL "&&" InfixOpAnd ]
        ,   [ infL "||" InfixOpOr ]

        ,   [ leftPipe, rightPipe]
        ,   [ infL ".>" InfixOpChainDot ]
        ]

--------------------------------------------------------------------------------

doExpr :: ESParser Expr
doExpr = do
    T.symbol "do"
    blockStart *> (DoExpr <$> decl `sepBy` lineEnd) <* blockEnd

--------------------------------------------------------------------------------

decl :: ESParser Decl
decl = letDecl <|> returnDecl <|> assignDecl <|> exprDecl

letDecl :: ESParser Decl
letDecl = do
    reserve "let"
    lineCont
    p <- pat
    T.symbol "="
    e <- expr
    return (LetDecl p e)
    <?> "let declaration"

returnDecl :: ESParser Decl
returnDecl = do
    reserve "return"
    lineCont
    ReturnDecl <$> expr
    <?> "let declaration"

assignDecl :: ESParser Decl
assignDecl = do
    p <- try (expr <* T.symbol "=")
    e <- expr
    return (AssignDecl p e)
    <?> "assign declaration"

exprDecl :: ESParser Decl
exprDecl = ExprDecl <$> expr  <?> "expression declaration"

--------------------------------------------------------------------------------

program :: ESParser Program
program = Program <$> many decl
