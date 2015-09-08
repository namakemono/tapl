module Syntax where

-- 項(Def. 6.1.2) ------------------------------------------------------------------
data Term = TmVar Int Int -- 変数をde Bruijnインデックスで表した値, 文脈全体の長さ
          | TmAbs String Term -- ラムダ式
          | TmApp Term Term -- 関数適用
        deriving(Show, Eq)

-- | 項が値であるかどうか
-- >>> isval (TmVar 0 1)
-- False
-- >>> isval (TmAbs "x" (TmVar 0 1))
-- True
isval :: Term -> Bool
isval (TmAbs _ _) = True
isval _ = False

-- 束縛 ----------------------------------------------------------------------------
data Binding = NameBind

-- 文脈 ----------------------------------------------------------------------------
newtype Context = Context [(String, Binding)]

-- 空の文脈
emptyContext :: Context
emptyContext = Context []
