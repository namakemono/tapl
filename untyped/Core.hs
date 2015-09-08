module Core where

import Control.Applicative((<$>))
import Syntax(Term(..), Context, emptyContext, Binding(..), isval)

-- | シフト(Def.6.2.1) 
-- >>> walkForTermShift 1 0 (TmVar 0 1) == (TmVar 1 2)
-- True
walkForTermShift :: Int -> Int -> Term -> Term
walkForTermShift d c t = case t of
    (TmVar x n)         -> if x >= c then (TmVar (x+d) (n+d)) else (TmVar x (n+d))
    (TmAbs x t1)        -> TmAbs x (walkForTermShift d (c+1) t1)
    (TmApp t1 t2)       -> TmApp (walkForTermShift d c t1) (walkForTermShift d c t2)

termShift :: Int -> Term -> Term
termShift d t = walkForTermShift d 0 t 

-- | 代入[j |-> s] t
-- >>> termSubst 0 (TmAbs "x" (TmVar 1 1)) (TmVar 0 0) == TmAbs "x" (TmVar 1 1)
-- True
-- >>> termSubst 0 (TmAbs "x" (TmVar 0 1)) (TmVar 1 1) == (TmVar 1 1)
-- True
walkForTermSubst :: Int -> Term -> Int -> Term -> Term
walkForTermSubst j s c t = case t of
    (TmVar x n)         -> if (x == j+c) then (termShift c s) else (TmVar x n) 
    (TmAbs x t1)        -> TmAbs x (walkForTermSubst j s (c+1) t1)
    (TmApp t1 t2)       -> TmApp (walkForTermSubst j s c t1) (walkForTermSubst j s c t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = walkForTermSubst j s 0 t

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

-- | 単一ステップの評価器
-- >>> eval1 emptyContext (TmVar 0 0) == Nothing
-- True
eval1 :: Context -> Term -> Maybe Term
eval1 ctx t = case t of
    (TmApp (TmAbs x t12) v2) | isval v2             -> Just (termSubstTop v2 t12)
    (TmApp v1 t2) | isval v1                        -> (\t2' -> TmApp v1 t2') <$> eval1 ctx t2
    (TmApp t1 t2)                                   -> (\t1' -> TmApp t1' t2) <$> eval1 ctx t1
    _                                               -> Nothing -- No Rule Applies

-- | 多ステップの評価を行う
-- >>> print $ eval emptyContext (TmApp (TmAbs "x" (TmVar 0 1)) (TmAbs "x" (TmVar 0 0))) == TmAbs "x" (TmVar 0 0)
-- True
eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
            Just t' -> eval ctx t'
            Nothing -> t

