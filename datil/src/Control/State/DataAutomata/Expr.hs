{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.State.DataAutomata.Expr where

import           Data.Sequence (Seq)
import           Data.Text (Text)
import           Data.Typeable (TypeRep, Typeable, typeOf)
import           GHC.Exts (IsString, fromString)


data Expr t where
  VarE  :: (Show t, Eq t) => Var t -> Expr t
  Const :: (Show t, Eq t) => t -> Expr t
  (:==) :: (Typeable n, Eq n, Show n)  => Expr n -> Expr n -> Expr Bool
  (:<)  :: (Typeable n, Ord n, Show n) => Expr n -> Expr n -> Expr Bool
  (:<=) :: (Typeable n, Ord n, Show n) => Expr n -> Expr n -> Expr Bool
  (:*)  :: (Typeable n, Num n, Show n) => Expr n -> Expr n -> Expr n
  (:+)  :: (Typeable n, Num n, Show n) => Expr n -> Expr n -> Expr n
  -- | Semigroup binary operator
  (:<>) :: (Typeable a
           , Semigroup a
           , Show a
           ) => Expr a -> Expr a -> Expr a
  -- | List construction, by appending an element.
  (:|>)  :: (Typeable a, Show a, Eq a) => Expr (Seq a) -> Expr a -> Expr (Seq a)
  -- | Function application, 1 argument.
  Fapply  :: ( Show a
             , Typeable a
             , Eq a
             ) => FunctionName -> Expr a -> Expr b
  -- | Function application, 2 arguments.
  Fapply2 :: ( Show a
             , Typeable a
             , Eq a
             , Show b
             , Typeable b
             , Eq b
             ) => FunctionName -> Expr a -> Expr b -> Expr c
  -- | Function application, 3 arguments.
  Fapply3 :: ( Show a
             , Typeable a
             , Eq a
             , Show b
             , Typeable b
             , Eq b
             , Show c
             , Typeable c
             , Eq c
             ) => FunctionName -> Expr a -> Expr b -> Expr c -> Expr d

infix  4 :<
infix  4 :<=
infixl 5 :*
infixl 6 :+

newtype FunctionName = FunctionName Text
  deriving (Eq, Ord, IsString, Show)

class (Typeable b, Show b) => ToExpr a b | a -> b where
  toExpr :: a -> Expr b

instance (Typeable a, Show a) => ToExpr (Expr a) a where
  toExpr = id

instance ToExpr Word Word where
  toExpr = Const

instance (Typeable t, Show t, Eq t) => ToExpr (Var t) t where
  toExpr v = VarE v

(.==) :: forall a b c . (ToExpr a c, ToExpr b c, Eq c) => a -> b -> Expr Bool
x .== y = toExpr @a @c x  :== toExpr @b @c y

(.<) :: forall a b c . (ToExpr a c, ToExpr b c, Ord c) => a -> b -> Expr Bool
x .< y = toExpr @a @c x  :< toExpr @b @c y

(.<=) :: forall a b c . (ToExpr a c, ToExpr b c, Ord c) => a -> b -> Expr Bool
x .<= y = toExpr @a @c x  :<= toExpr @b @c y

(.*) :: forall a b c . (ToExpr a c, ToExpr b c, Num c) => a -> b -> Expr c
x .* y = toExpr @a @c x  :* toExpr @b @c y

(.+) :: forall a b c . (ToExpr a c, ToExpr b c, Num c) => a -> b -> Expr c
x .+ y = toExpr @a @c x  :+ toExpr @b @c y

infix  4 .==
infix  4 .<
infix  4 .<=
infixl 7 .*
infixl 6 .+

instance (Show t, Eq t) => IsString (Expr t) where
  fromString = VarE . fromString

instance Show (Expr t) where
  -- TODO: add parenthesis to remove ambiguity.
  show (VarE var)        = "VarE " ++ show var
  show (Const t)         = "Const " ++ show t
  show (expr0 :== expr1) = show expr0 ++ " :== " ++ show expr1
  show (expr0 :< expr1)  = show expr0 ++ " :< " ++ show expr1
  show (expr0 :<= expr1) = show expr0 ++ " :<= " ++ show expr1
  show (expr0 :+ expr1)  = show expr0 ++ " :+ " ++ show expr1
  show (expr0 :* expr1)  = show expr0 ++ " :* " ++ show expr1
  show (expr0 :<> expr1) = show expr0 ++ " :<> " ++ show expr1

  show (Fapply f expr) =
    "Fapply " ++ show f ++ " (" ++ show expr  ++ ")"
  show (Fapply2 f expr0 expr1)
    = "Fapply2 " ++ show f
                 ++ " (" ++ show expr0  ++ ")"
                 ++ " (" ++ show expr1  ++ ")"
  show (Fapply3 f expr0 expr1 expr2)
    = "Fapply3 " ++ show f
                 ++ " (" ++ show expr0  ++ ")"
                 ++ " (" ++ show expr1  ++ ")"
                 ++ " (" ++ show expr2  ++ ")"

  show (expr0 :|> expr1) = show expr0 ++ ":|>" ++ show expr1

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

newtype Var t = Var Text
  deriving (IsString, Show, Eq, Ord)

varType :: forall t . Typeable t => Var t -> TypeRep
varType (Var _) = typeOf (undefined :: t)
