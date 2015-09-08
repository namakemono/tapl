module Parser where

import System.IO
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import Syntax(Term(..))

-- ## References
-- [1]. Parsing expressions and statements, <https://wiki.haskell.org/Parsing_expressions_and_statements>


-- Define symbols
def :: LanguageDef st
def = emptyDef {
    commentStart = "/*",
    commentEnd = "*/",
    reservedNames = ["if", "then", "else", "true", "false", "0", "iszero", "succ", "pred"]
}

-- Make token parser
-- lexer = makeTokenParser def
TokenParser {
    parens = m_parens,
    reserved = m_reserved,
    whiteSpace = m_whiteSpace,
    integer = m_integer
} = makeTokenParser def

-- |
-- Expression and Statement parser
exprParser :: Parser Term
exprParser = buildExpressionParser [] term <?> "expression"
term = m_parens exprParser
    <|> do { -- if t1 then t2 else t3
        m_reserved "if";
        t1 <- exprParser;
        m_reserved "then";
        t2 <- exprParser;
        m_reserved "else";
        t3 <- exprParser;
        return (TmIf t1 t2 t3)
    }
    <|> do { -- succ t1
        m_reserved "succ";
        t1 <- exprParser;
        return (TmSucc t1)
    }
    <|> do { -- pred t1
        m_reserved "pred";
        t1 <- exprParser;
        return (TmPred t1)
    }
    <|> do { -- iszero t1
        m_reserved "iszero";
        t1 <- exprParser;
        return (TmIsZero t1)
    }
    <|> (m_reserved "0" >> return TmZero)
    <|> (m_reserved "true" >> return TmTrue)
    <|> (m_reserved "false" >> return TmFalse)

-- |
-- >>> parseArith "if true then false else true" == (TmIf TmTrue TmFalse TmTrue)
-- True 
-- >>> parseArith "0" == TmZero
-- True
-- >>> parseArith "succ (pred 0)" == TmSucc (TmPred TmZero)
-- True
-- >>> parseArith "iszero (pred (succ (succ 0)))" == (TmIsZero (TmPred (TmSucc (TmSucc TmZero))))
-- True
parseArith :: String -> Term
parseArith input = case parse exprParser "" input of 
                Left e -> error $ show e
                Right t -> t

