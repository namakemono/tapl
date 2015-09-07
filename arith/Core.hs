module Core where

import Control.Applicative((<$>))
import Syntax(Term(..), isnumericval)

-- | 単一ステップの評価器
-- >>> (eval1 (TmIf TmTrue TmFalse TmTrue)) == Just TmFalse
-- True
-- >>> (eval1 (TmIf TmFalse TmFalse TmTrue)) == Just TmTrue
-- True
-- >>> (eval1 (TmIf (TmIf TmTrue TmFalse TmTrue) TmZero TmFalse)) == Just (TmIf TmFalse TmZero TmFalse)
-- True
-- >>> eval1 (TmPred TmZero) == Just TmZero
-- True
eval1 :: Term -> Maybe Term
eval1 t = case t of
    (TmIf TmTrue t2 t3)                             -> Just t2
    (TmIf TmFalse t2 t3)                            -> Just t3
    (TmIf t1 t2 t3)                                 -> (\t1' -> TmIf t1' t2 t3) <$> eval1 t1
    (TmSucc t1)                                     -> TmSucc <$> eval1 t1
    (TmPred TmZero)                                 -> Just TmZero
    (TmPred (TmSucc nv1)) | isnumericval nv1        -> Just nv1
    (TmPred t1)                                     -> TmPred <$> eval1 t1
    (TmIsZero TmZero)                               -> Just TmTrue
    (TmIsZero (TmSucc nv1)) | isnumericval nv1      -> Just TmFalse
    (TmIsZero t1)                                   -> TmIsZero <$> eval1 t1
    _                                               -> Nothing

-- |
-- 最終的にeval1がどの規則も適用されないポイントに達すると，例外No Rule Applies を投げて，間約列の最後の項を返す
-- >>> (eval (TmIf (TmIf TmTrue TmFalse TmTrue) TmZero TmFalse)) == TmFalse 
-- True
eval :: Term -> Term
eval t = case eval1 t of
            Just t' -> eval t'
            Nothing -> t
