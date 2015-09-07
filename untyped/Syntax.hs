module Syntax where

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
        deriving(Eq)

-- | 項が数値かどうか
-- >>> isnumericval (TmSucc (TmSucc TmZero))
-- True
-- >>> isnumericval (TmPred (TmSucc TmZero))
-- False
isnumericval :: Term -> Bool
isnumericval TmZero = True
isnumericval (TmSucc t) = isnumericval t
isnumericval _ = False

-- | 項が値であるかどうか
-- >>> isval TmFalse
-- True
-- >>> isval (TmIf TmTrue TmZero TmZero)
-- False
isval :: Term -> Bool
isval TmTrue = True
isval TmFalse = True
isval t = isnumericval t

-- |
-- >>> termToInteger (TmSucc (TmSucc TmZero))
-- 2
termToInteger :: Term -> Int
termToInteger TmZero        = 0
termToInteger (TmSucc t)    = (termToInteger t) + 1
termToInteger _             = error("Succ Error.")

instance Show Term where
    show TmTrue                         = "true"
    show TmFalse                        = "false"
    show (TmIf t1 t2 t3)                = "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3
    show TmZero                         = "0"
    show (TmSucc t1) | isnumericval t1  = show $ termToInteger (TmSucc t1)
                     | otherwise        = show "(succ " ++ show t1 ++ ")"
    show (TmPred t1)                    = "(pred " ++ show t1 ++ ")"
    show (TmIsZero t1)                  = "iszero " ++ show t1

