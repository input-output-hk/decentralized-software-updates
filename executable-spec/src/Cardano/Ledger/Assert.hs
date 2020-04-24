{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Ledger.Assert
  ( assert
  -- * Comparison assertions
  , (<!)
  , (<=!)
  , (==!)
  , (>!)
  , (>=!)
  -- ** LHS is maybe
  , (?<!)
  , (?<=!)
  , (?==!)
  , (?>!)
  , (?>=!)
  -- ** RHS is maybe
  , (<?!)
  , (<=?!)
  , (==?!)
  , (>?!)
  , (>=?!)
  -- ** Both sides are maybe
  , (?<?!)
  , (?<=?!)
  , (?==?!)
  , (?>?!)
  , (?>=?!)
  -- * Comparison with maybe values
  , (?<)
  , (?<=)
  , (?==)
  , (?>)
  , (?>=)
  -- * Assertions on containers
  , holdsForAllElementsIn
  , doesNotContain
  , doesNotContainKey
  , doesNotContainMaybeKey
  -- * Show utilities
  , cShow
  , orElseShow
  )
where

import           GHC.Stack (HasCallStack)

import           Control.Monad.Validate (Validate, dispute, runValidate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as  Map
import           Data.Foldable (traverse_)

#if PRETTY_PRINT
import qualified Text.Pretty.Simple as Pretty
#endif

import qualified Data.Text.Lazy as Text.Lazy

assert :: HasCallStack => Assertion -> a -> a
assert assertion a =
#if ENABLE_ASSERTIONS
  case runValidate assertion of
    Left msg -> error $ unlines
                      $ "Failed assertions" : fmap Text.Lazy.unpack msg
    Right _  -> a
#else
  a
#endif

failBecause :: Text.Lazy.Text -> Assertion
failBecause = dispute . pure

pass :: Assertion
pass = pure ()

type Assertion = Validate [Text.Lazy.Text] ()

(<!), (<=!), (==!), (>!), (>=!) :: (Show a, Ord a) => a -> a -> Assertion
(<!)  = assertOrder LessThan
(<=!) = assertOrder LessEqualThan
(==!) = assertOrder Equal
(>!)  = assertOrder GreaterThan
(>=!) = assertOrder GreaterEqualThan

(?<!), (?<=!), (?==!), (?>!), (?>=!)
  :: (Show a, Ord a) => Maybe a -> a -> Assertion
ma ?<! b  = maybe pass (<!  b) ma
ma ?<=! b = maybe pass (<=! b) ma
ma ?==! b = maybe pass (==! b) ma
ma ?>! b  = maybe pass (>!  b) ma
ma ?>=! b = maybe pass (>=!  b) ma

(<?!), (<=?!), (==?!), (>?!), (>=?!)
  :: (Show a, Ord a) => a -> Maybe a -> Assertion
a <?!  mb = maybe pass (a <! ) mb
a <=?! mb = maybe pass (a <=!) mb
a ==?! mb = maybe pass (a ==!) mb
a >?!  mb = maybe pass (a >! ) mb
a >=?! mb = maybe pass (a >=!) mb

(?<?!), (?<=?!), (?==?!), (?>?!), (?>=?!)
  :: (Show a, Ord a) => Maybe a -> Maybe a -> Assertion
ma ?<?!  mb = maybe pass (ma ?<! ) mb
ma ?<=?! mb = maybe pass (ma ?<=!) mb
ma ?==?! mb = maybe pass (ma ?==!) mb
ma ?>?!  mb = maybe pass (ma ?>! ) mb
ma ?>=?! mb = maybe pass (ma ?>=!) mb

(?<), (?<=), (?==), (?>), (?>=)
  :: Ord a => Maybe a -> a -> Bool
ma ?< b  = maybe True (<  b) ma
ma ?<= b = maybe True (<= b) ma
ma ?== b = maybe True (== b) ma
ma ?> b  = maybe True (>  b) ma
ma ?>= b = maybe True (>  b) ma

data CompareBy
  = LessThan
  | LessEqualThan
  | Equal
  | GreaterThan
  | GreaterEqualThan

assertOrder :: (Show a, Ord a) => CompareBy -> a -> a -> Assertion
assertOrder ordering a b
  | compareBy ordering a b = pass
  | otherwise              = failBecause $  cShow a
                                         <> orderRelationDoesNotHold ordering
                                         <> cShow b

compareBy :: Ord a => CompareBy -> a -> a -> Bool
compareBy LessThan         = (<)
compareBy LessEqualThan    = (<=)
compareBy Equal            = (==)
compareBy GreaterThan      = (>)
compareBy GreaterEqualThan = (>)

orderRelationDoesNotHold :: CompareBy -> Text.Lazy.Text
orderRelationDoesNotHold LessThan         = " is not less than "
orderRelationDoesNotHold LessEqualThan    = " is not less or equal than "
orderRelationDoesNotHold Equal            = " is not equal to "
orderRelationDoesNotHold GreaterThan      = " is not greater than"
orderRelationDoesNotHold GreaterEqualThan = " is not greater or equal than"

-- | Customized show. It can show pretty formatted data structures, or not
-- depending on the PRETTY_PRINT_ASSERTIONS compile time flag.
cShow :: Show a => a -> Text.Lazy.Text
cShow =
#if PRETTY_PRINT
  Pretty.pShow
#else
  Text.Lazy.pack . show
#endif

orElseShow :: Bool -> Text.Lazy.Text -> Assertion
orElseShow False reason = failBecause reason
orElseShow True  _      = pass

doesNotContain
  :: (Eq a, Show a, Foldable f) => f a -> a -> Assertion
doesNotContain xs a
  | a `elem` xs = failBecause $ cShow a <> " was found in the structure"
  | otherwise   = pass

doesNotContainKey
  :: (Ord k, Show k) => Map k v -> k -> Assertion
doesNotContainKey m k
  | Map.member k m = failBecause $ "key " <> cShow k <> " was found in the map"
  | otherwise      = pass

doesNotContainMaybeKey
  :: (Ord k, Show k) => Map k v -> Maybe k -> Assertion
doesNotContainMaybeKey m mk = maybe pass (doesNotContainKey m) mk

holdsForAllElementsIn
  :: (Show a, Foldable f) => (a -> Bool) -> f a -> Assertion
holdsForAllElementsIn p fa =
  traverse_ (p `holdsFor`) fa

holdsFor :: Show a => (a -> Bool) -> a -> Assertion
holdsFor p a
  | p a       = pass
  | otherwise = failBecause $ "Predicate does not hold for " <> cShow a
