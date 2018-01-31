{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
module Linear where

import Bound
import Bound.Name
import Control.Lens hiding (Context)
import Control.Monad
import Data.Bifunctor
import Data.Deriving
import Data.Foldable
import Data.Functor
import Data.Functor.Classes
import Data.List
import Data.Map (Map)
import Data.Monoid

import qualified Data.Map as Map

data Type
  = Con String
  | Lolly Type Type
  | Times Type Type
  | Plus Type Type
  | With Type Type
  | Bang Type
  | Unit
  | Top
  | TyVar String
  deriving (Eq, Show, Ord)

renderType :: Type -> String
renderType (Con a) = a
renderType (Lolly a b) = nested a <> " -o " <> renderType b
  where
    nested a@Lolly{} = "(" <> renderType a <> ")"
    nested a = renderType a
renderType (Times a b) = renderType a <> " * " <> renderType b
renderType (Plus a b) = renderType a <> " + " <> renderType b
renderType (With a b) = renderType a <> " & " <> renderType b
renderType (Bang a) = "!" <> renderType a
renderType Unit = "Unit"
renderType Top = "Top"
renderType (TyVar s) = s

data Expr n a
  = Var a
  | Lam n Type (Scope (Name n ()) (Expr n) a)
  | BangLam n Type (Scope (Name n ()) (Expr n) a)
  | App (Expr n a) (Expr n a)
  | MkL (Expr n a)
  | MkR (Expr n a)
  | MkTimes (Expr n a) (Expr n a)
  | MkWith (Expr n a) (Expr n a)
  | MkTop
  | MkUnit
  deriving (Functor, Foldable, Traversable)

deriveEq1 ''Expr
deriveOrd1 ''Expr
deriveShow1 ''Expr

instance (Eq n, Eq a) => Eq (Expr n a) where (==) = eq1
instance (Ord n, Ord a) => Ord (Expr n a) where compare = compare1
instance (Show n, Show a) => Show (Expr n a) where showsPrec = showsPrec1

makeBound ''Expr

lam :: Eq a => a -> Type -> Expr a a -> Expr a a
lam v t b = Lam v t (abstract1Name v b)

banglam :: Eq a => a -> Type -> Expr a a -> Expr a a
banglam v t b = BangLam v t (abstract1Name v b)

data TypeError a
  = AlreadyUsed a
  | NotInScope a
  | Unused [a]
  | MultipleUses [a]
  | TypeMismatch Type Type
  deriving (Eq, Show)

data Context a
  = Context
  { _linears :: [(a, Type)]
  , _bangs :: Map a Type
  }

makeLenses ''Context

insert :: Ord a => a -> Type -> Context a -> Context a
insert v t ctxt =
  case t of
    Bang t' -> ctxt & bangs %~ Map.insert v t'
    _ -> ctxt & linears %~ ((v, t) :)

find :: Ord a => a -> Context a -> Either (TypeError a) Type
find v ctxt
  | ls <- ctxt ^. linears
  , Just t <- lookup v ls =
      case ls of
        [_] -> Right t
        _ -> Left $ Unused (fst <$> ls)
  | Just t <- Map.lookup v (ctxt ^. bangs) =
      let
        ls = ctxt ^. linears
      in
        if null ls
        then Right t
        else Left $ Unused (fst <$> ls)
  | otherwise = Left $ NotInScope v

lookupDel :: Eq a => a -> [(a, b)] -> Maybe (b, [(a, b)])
lookupDel _ [] = Nothing
lookupDel a ((b, c):rest)
  | a == b = Just (c, rest)
  | otherwise = bimap id ((b, c) :) <$> lookupDel a rest

-- | Partition the
partition :: Eq a => Expr a a -> Expr a a -> Context a -> Either (TypeError a) (Context a, Context a)
partition a b ctxt =
  let
    afrees = toList a
    (actxt, ctxt') = partition' afrees ctxt
    bfrees = toList b
    (bctxt, ctxt'') = partition' bfrees ctxt'
    ls = ctxt'' ^. linears
  in
    if null ls
    then Right (ctxt & linears .~ actxt, ctxt & linears .~ bctxt)
    else Left $ Unused (fst <$> ls)
  where
    partition' [] ctxt = ([], ctxt)
    partition' (x:xs) ctxt =
      case lookupDel x (ctxt ^. linears) of
        Nothing
          | -> partition' xs ctxt
        Just (x', linears') ->
          bimap ((x, x') :) id $
          partition' xs (ctxt & linears .~ linears')

{-
isBang :: Type -> Bool
isBang Bang{} = True
isBang _ = False

infer :: (Eq a, Show a) => Context a -> Expr a a -> Either (TypeError a) Type
infer ctxt (Var a) =
  case lookup a ctxt of
    Just t
      | isBang t ->
        if length (filter (not . isBang . snd) ctxt) == 0
        then Right t
        else
          let
            names = fmap fst ctxt
          in
            Left . Unused $ filter (/= a) names
      | length (filter (not . isBang . snd) ctxt) == 1 -> Right t
    _
      | names <- fmap fst ctxt
      , a `elem` names -> Left . Unused $ filter (/= a) names
      | otherwise -> Left $ NotInScope a
infer ctxt (Lam x t s) = Lolly t <$> infer ((x, t) : ctxt) (instantiate1 (Var x) s)
infer ctxt (BangLam x t s) = Lolly (Bang t) <$> infer ((x, Bang t) : ctxt) (instantiate1 (Var x) s)
infer ctxt (App a b) =
  let
    actxt = usedIn ctxt a
    bctxt = usedIn ctxt b
  in
    if distinct actxt bctxt
    then do
      let unused = filter (`notElem` fmap fst (mappend actxt bctxt)) (fst <$> ctxt)
      when (not $ null unused) . Left . Unused $ unused
      aty <- infer actxt a
      case aty of
        Lolly from to -> do
          ty' <- infer bctxt b
          if from == ty' then Right from else Left $ TypeMismatch from ty'
          pure to
        _ -> Left $ TypeMismatch aty (Lolly (TyVar "a") (TyVar "b"))
    else Left . MultipleUses . fmap fst $ filter ((`elem` bctxt)) actxt
infer ctxt (MkL a) = Plus <$> infer ctxt a <*> pure (TyVar "a")
infer ctxt (MkR a) = Plus (TyVar "a") <$> infer ctxt a
infer ctxt (MkTimes a b) =
  let
    actxt = usedIn ctxt a
    bctxt = usedIn ctxt b
  in
    if distinct actxt bctxt
    then do
      let unused = filter (`notElem` fmap fst (mappend actxt bctxt)) (fst <$> ctxt)
      when (not $ null unused) . Left . Unused $ unused
      Times <$> infer actxt a <*> infer bctxt b
    else Left . MultipleUses . fmap fst $ filter ((`elem` bctxt)) actxt
infer ctxt (MkWith a b) = With <$> infer ctxt a <*> infer ctxt b
infer ctxt MkTop = Right Top
infer ctxt MkUnit =
  case filter (not . isBang . snd) ctxt of
    [] -> Right Unit
    _ -> Left . Unused $ fmap fst ctxt
-}
