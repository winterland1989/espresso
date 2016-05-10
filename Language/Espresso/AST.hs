{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Espresso.AST where

import Data.Data
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

type Identifier = String

data Position = Position !Int !Int deriving (Data, Eq, Show, Typeable)

data Annotation
    = Annotation !Position ![String] -- ^ Annotation: position and comments
    | NoAnnotation                    -- ^ No annotation
    deriving (Data, Eq, Show, Typeable)

data Pat
    = PatArray (Maybe Identifier) [Pat] (Maybe Expr)
    | PatObject (Maybe Identifier) [(Expr, Pat)] (Maybe Expr)  -- ^ list of key - pat pair
    | PatIdent Identifier (Maybe Expr) -- ^ binding 'Identifier' and default value pair
  deriving (Show, Eq, Data, Typeable)

instance Pretty Pat where
    pretty (PatArray i pats d) = "PatArray"
    pretty (PatObject i oPats d) = "PatObject"
    pretty (PatIdent id d) = "Pat"

data Decl
    = AssignDecl Expr Expr
    | LetDecl Pat Expr
    | ExprDecl Expr
    | ReturnDecl Expr
    | BreakDecl
    | Try [Decl]
    | TryCatch [Decl] Identifier [Decl]
  deriving (Show, Eq, Data, Typeable)

instance Pretty Decl where
    pretty (AssignDecl l r) = pretty l <+> "=" <+> pretty r
    pretty (LetDecl p e) = "let" <+> pretty p <+> "=" <+> pretty e
    pretty (ExprDecl e) = pretty e

newtype Program = Program [Decl]  deriving (Show, Eq, Data, Typeable)

instance Pretty Program where
    pretty (Program ds) = vsep (map pretty ds)

data Expr
    = NumLit (Either Integer Double)
    | StringLit String
    | BoolLit Bool
    | NullLit
    | VoidLit

    | IdentExpr Identifier

    | InfixExpr InfixOp Expr Expr
    | PrefixExpr PrefixOp Expr
    | PostfixExpr PostfixOp Expr

    | FunctionExpr [Pat] [Decl]

    | ObjectExpr [(Expr, Expr)]
    | ArrayExpr [Expr]

    | ApplyExpr Expr Expr
    | MultiExpr [Expr] -- ^ (a, b, c)

    | LetExpr [Decl] Expr
    | DoExpr [Decl]

  deriving (Show, Eq, Data, Typeable)

instance Pretty Expr where
    pretty (NumLit (Left i)) = pretty i
    pretty (NumLit (Right f)) = pretty f
    pretty (StringLit str)  = pretty str
    pretty (BoolLit True)   = "true"
    pretty (BoolLit False)  = "false"
    pretty NullLit          = "null"
    pretty VoidLit          = "void"
    pretty (IdentExpr i)   = pretty i
    pretty (InfixExpr op l r)   = pretty l <> pretty op <> pretty r
    pretty (PrefixExpr op e) = pretty op <> pretty e
    pretty (PostfixExpr op e) = pretty e <> pretty op

    pretty (FunctionExpr pats decls) = pretty pats <+> "->" <$> indent 4 (vsep (map pretty decls))


    pretty (ArrayExpr es) = pretty es
    pretty (ObjectExpr kvs) = pretty kvs

    pretty (ApplyExpr f x) = pretty f <+> "ap" <+> pretty x
    pretty (MultiExpr es) = pretty es

    pretty (DoExpr ds) = "do" <$> indent 4 (vsep (map pretty ds))

data InfixOp
    = InfixOpOr
    | InfixOpAnd
    | InfixOpBitAnd
    | InfixOpBitOr
    | InfixOpBitXor
    | InfixOpEq
    | InfixOpNeq
    | InfixOpLe
    | InfixOpLt
    | InfixOpGe
    | InfixOpGt
    | InfixOpMod
    | InfixOpAdd
    | InfixOpSub
    | InfixOpDiv
    | InfixOpMul
    | InfixOpLshift
    | InfixOpRshift
    | InfixOpURshift
    | InfixOpAccess
    | InfixOpMaybeAccess
    | InfixOpCompose
    | InfixOpIn
    | InfixOpInstanceOf
    | InfixOpChainDot
    deriving (Data, Eq, Show, Typeable)

instance Pretty InfixOp where
    pretty InfixOpOr           = "or"
    pretty InfixOpAnd          = "and"
    pretty InfixOpBitAnd       = "&"
    pretty InfixOpBitOr        = "|"
    pretty InfixOpBitXor       = "^"
    pretty InfixOpEq           = "=="
    pretty InfixOpNeq          = "!="
    pretty InfixOpLe           = "<="
    pretty InfixOpLt           = "<"
    pretty InfixOpGe           = ">="
    pretty InfixOpGt           = ">"
    pretty InfixOpMod          = "%"
    pretty InfixOpAdd          = "+"
    pretty InfixOpSub          = "-"
    pretty InfixOpDiv          = "/"
    pretty InfixOpMul          = "*"
    pretty InfixOpLshift       = "<<"
    pretty InfixOpRshift       = ">>"
    pretty InfixOpURshift      = ">>>"
    pretty InfixOpAccess       = "."
    pretty InfixOpMaybeAccess  = "?."
    pretty InfixOpCompose      = "<<"
    pretty InfixOpIn           = "in"
    pretty InfixOpInstanceOf   = "instanceof"
    pretty InfixOpChainDot     = ".>"


data PrefixOp
    = PrefixOpDecr
    | PrefixOpIncr
    | PrefixOpNeg
    | PrefixOpPos
    | PrefixOpNot
    | PrefixOpTilde
    | PrefixOpTypeof
    | PrefixOpNew
    | PrefixOpDelete
    deriving (Data, Eq, Show, Typeable)

instance Pretty PrefixOp where
    pretty PrefixOpDecr       = "--"
    pretty PrefixOpIncr       = "++"
    pretty PrefixOpNeg        = "-"
    pretty PrefixOpPos        = "+"
    pretty PrefixOpNot        = "not"
    pretty PrefixOpTilde      = "~"
    pretty PrefixOpTypeof     = "typeof"
    pretty PrefixOpNew        = "new"
    pretty PrefixOpDelete     = "delete"

data PostfixOp
    = PostfixOpDecr
    | PostfixOpIncr
    | PostfixOpExist
    deriving (Data, Eq, Show, Typeable)

instance Pretty PostfixOp where
    pretty PostfixOpDecr      = "--"
    pretty PostfixOpIncr      = "++"
    pretty PostfixOpExist     = "?"
