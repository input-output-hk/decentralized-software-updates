{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Control.State.DataAutomata.Interpreter.Memory where

import           Control.Monad.Except (Except, throwError)
import           GHC.Exts (IsList)
import           Data.Map.Strict (Map)
import           Data.Typeable (Typeable, cast, TypeRep, typeOf)
import           Data.Text (Text)
import qualified Data.Map.Strict as Map


newtype Memory = Memory { unMemory :: Map Text Cell }
  deriving (Show, IsList, Semigroup, Monoid)

data Cell = forall v . (Typeable v, Show v, Eq v) => Cell v

cellType :: Cell -> TypeRep
cellType (Cell v) = typeOf v

cellValue :: Typeable t => Cell -> Maybe t
cellValue (Cell v) = cast v

deriving instance (Show Cell)

data Error
  = NotFound Text
  | WrongType Text (Expected TypeRep) (Actual TypeRep)
  | NameExists Text
  deriving (Show)

newtype Expected v = Expected v
  deriving (Eq, Show)

newtype Actual v = Actual v
  deriving (Eq, Show)

lookupCell :: Text -> Memory -> Except Error Cell
lookupCell name (Memory cellMap) = maybe notFound pure $ Map.lookup name cellMap
  where
    notFound = throwError $ NotFound name

lookupCellType :: Text -> Memory -> Except Error TypeRep
lookupCellType name mem = cellType <$> lookupCell name mem


(.?) :: forall t . Typeable t => Memory -> Text -> Except Error t
mem .? name = do
  cell <- lookupCell name mem
  maybe (wrongType cell) pure $ cellValue cell
  where
    wrongType cell = throwError $ WrongType name expected actual
      where
        expected = Expected $ typeOf $ (undefined :: t)
        actual = Actual (cellType cell)

(.??) :: Memory -> Text -> (forall t . Typeable t => Except Error (Maybe t))
mem.?? name = do
  cell <- lookupCell name mem
  pure $ cellValue cell

assign
  :: forall t
   . (Typeable t, Show t, Eq t)
  => Memory
  -> Text
  -> t
  -> Except Error Memory
assign mem@(Memory cellMap) name newValue = do
  maybeT :: (Maybe t) <- mem .?? name
  case maybeT of
    Nothing -> do
      theCellType <- lookupCellType name mem
      throwError $ WrongType name expected (Actual theCellType)
    Just _ -> pure $ Memory $ Map.insert name (Cell newValue) cellMap
  where
    expected = Expected $ typeOf $ (undefined :: t)

extend
  :: Memory
  -> Text
  -> Cell
  -> Except Error Memory
extend (Memory cellMap) name newValue = do
  case Map.lookup name cellMap of
    Nothing -> pure $ Memory $ Map.insert name newValue cellMap
    Just _ -> throwError $ NameExists name

remove
  :: Memory
  -> Text
  -> Memory
remove (Memory cellMap) name = Memory $ Map.delete name cellMap

contains :: Memory -> Text -> Bool
contains (Memory cellMap) name = Map.member name cellMap
