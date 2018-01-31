{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
module Linear where

import Bound
import Bound.Name
import Control.Applicative
import Control.Lens hiding (Context)
import Control.Monad
import Data.Bifunctor
import Data.Deriving
import Data.Foldable
import Data.Functor
import Data.Functor.Classes
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

data Entry a
  = LinearEntry a Type Bool
  | BangEntry a Type
  deriving (Eq, Show, Ord)

entryName :: Entry a -> a
entryName (LinearEntry a _ _) = a
entryName (BangEntry a _) = a

isBang :: Entry a -> Bool
isBang BangEntry{} = True
isBang _ = False

isUnusedLinear :: Entry a -> Bool
isUnusedLinear (LinearEntry _ _ b) = not b
isUnusedLinear _ = False

newtype Context a = Context [Entry a]
  deriving (Eq, Show, Ord)

makeWrapped ''Context

insert :: a -> Type -> Context a -> Context a
insert v t (Context ctxt) =
  Context $
  case t of
    Bang t' -> BangEntry v t' : ctxt
    _ -> LinearEntry v t False : ctxt

retrieve :: Eq a => a -> Context a -> Either (TypeError a) (Type, Context a)
retrieve v (Context []) = Left $ NotInScope v
retrieve v (Context (e:es)) =
  case e of
    BangEntry v' t
      | v == v' ->
          case filter isUnusedLinear es of
            [] -> Right (Bang t, Context $ e : es)
            unused -> Left . Unused $ entryName <$> unused
      | otherwise -> over (mapped._2._Wrapped) (e :) $ retrieve v (Context es)
    LinearEntry v' t True
      | v == v' -> Left (AlreadyUsed v)
      | otherwise -> over (mapped._2._Wrapped) (e :) $ retrieve v (Context es)
    LinearEntry v' t False
      | v == v' ->
          case filter isUnusedLinear es of
            [] -> Right (t, Context $ LinearEntry v' t True : es)
            unused -> Left . Unused $ entryName <$> unused
      | otherwise ->
          case find ((==v) . entryName) es of
            Nothing -> Left $ NotInScope v
            Just res ->
              case res of
                LinearEntry _ _ True -> Left $ AlreadyUsed v
                _ ->
                  Left . Unused $
                  entryName <$>
                  filter (liftA2 (&&) isUnusedLinear ((/=v) . entryName)) (e:es)

infer :: Eq a => Expr a a -> Context a -> Either (TypeError a) (Type, Context a)
infer (Var a) ctxt =
  retrieve a ctxt

infer (Lam x t s) ctxt = do
  (ty, Context (_:ctxt)) <- over (mapped._1) (Lolly t) $ infer (instantiate1 (Var x) s) (insert x t ctxt)
  pure (ty, Context ctxt)

infer (BangLam x t s) ctxt = do
  (ty, Context (_:ctxt)) <- over (mapped._1) (Lolly $ Bang t) $ infer (instantiate1 (Var x) s) (insert x (Bang t) ctxt)
  pure (ty, Context ctxt)

infer (App a b) ctxt = do
  (aty, ctxt') <- infer a ctxt
  case aty of
    Lolly from to -> do
      (bty, ctxt'') <- infer b ctxt'
      if from == bty then Right from else Left $ TypeMismatch from bty
      pure (to, ctxt'')
    _ -> Left $ TypeMismatch aty (Lolly (TyVar "a") (TyVar "b"))

infer (MkL a) ctxt = do
  (aty, ctxt') <- infer a ctxt
  pure (Plus aty $ TyVar "a", ctxt')

infer (MkR a) ctxt = do
  (aty, ctxt') <- infer a ctxt
  pure (Plus (TyVar "a") aty, ctxt') 

infer (MkTimes a b) ctxt = do
  (aty, ctxt') <- infer a ctxt
  (bty, ctxt'') <- infer b ctxt'
  pure (Times aty bty, ctxt'')

infer (MkWith a b) ctxt = do
  (aty, _) <- infer a ctxt
  (bty, _) <- infer b ctxt
  pure (With aty bty, ctxt)

infer MkTop ctxt = Right (Top, ctxt)

infer MkUnit (Context ctxt) =
  case filter isUnusedLinear ctxt of
    [] -> Right (Unit, Context ctxt)
    unused -> Left . Unused $ entryName <$> unused
