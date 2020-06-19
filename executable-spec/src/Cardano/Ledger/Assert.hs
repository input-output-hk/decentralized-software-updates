{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.Assert
  ( assert
  , assertAndReturn
  , runAssertion
  , Assertion
  , pass
  , failBecause
  -- * Comparison assertions
  , (<!)
  , (<=!)
  , (==!)
  , (/=!)
  , (>!)
  , (>=!)
  -- ** LHS is maybe
  , (?<!)
  , (?<=!)
  , (?==!)
  , (?/=!)
  , (?>!)
  , (?>=!)
  -- ** RHS is maybe
  , (<?!)
  , (<=?!)
  , (==?!)
  , (/=?!)
  , (>?!)
  , (>=?!)
  -- ** Both sides are maybe
  , (?<?!)
  , (?<=?!)
  , (?==?!)
  , (?/=?!)
  , (?>?!)
  , (?>=?!)
    -- ** Logical operators
  , (||!)
  -- * Comparison with maybe values
  , (?<)
  , (?<=)
  , (?==)
  , (?/=)
  , (?>)
  , (?>=)
  -- * Assertions on containers
  , holdsForAllElementsIn
  , doesNotContain
  , doesNotContainKey
  , doesNotContainMaybeKey
  -- ** Assertions on lists
  , allUnique
  -- * Quantifications
  , forall
  , exists
  -- * Show utilities
  , cShow
  , orElseShow
#if PRETTY_PRINT
  , prettyShow
#endif
  , showErrors
  )
where

import           GHC.Stack (HasCallStack)
import           Control.Monad.Validate (Validate, dispute, runValidate, mapErrors)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as  Map
import           Data.Foldable (traverse_)
import           Data.List.Unique (repeated)
import           Data.List (intercalate)

#if PRETTY_PRINT
import qualified Text.Pretty.Simple as Pretty
#endif

import qualified Data.Text.Lazy as Text.Lazy

assert :: HasCallStack => Assertion -> a -> a
assert assertion a =
#if ENABLE_ASSERTIONS
  case runAssertion assertion of
    Just msg -> error $ unlines
                      $ "Failed assertions" : fmap Text.Lazy.unpack msg
    Nothing -> a
#else
  a
#endif


-- | Check the assertion. If it succeeds return 'Nothing', if it fails return
-- the error.
runAssertion :: Assertion -> Maybe [Text.Lazy.Text]
runAssertion = either Just (const Nothing) . runValidate

assertAndReturn
  :: HasCallStack => (a -> Assertion) -> a -> a
assertAndReturn p x = assert (p x) x

failBecause :: Text.Lazy.Text -> Assertion
failBecause = dispute . pure

pass :: Assertion
pass = pure ()

type Assertion = Validate [Text.Lazy.Text] ()

(==!), (/=!) :: (Show a, Eq a) => a -> a -> Assertion
(<!), (<=!), (>!), (>=!) :: (Show a, Ord a) => a -> a -> Assertion
(<!)  = assertRelation (<)  " is not less than "
(<=!) = assertRelation (<=) " is not less or equal than "
(==!) = assertRelation (==) " is not equal to "
(/=!) = assertRelation (/=) " is equal to "
(>!)  = assertRelation (>)  " is not greater than"
(>=!) = assertRelation (>=) " is not greater or equal than"
infix 4 <!, <=!, ==!, /=!, >!, >=!

(?==!), (?/=!)
  :: (Show a, Eq a) => Maybe a -> a -> Assertion
(?<!), (?<=!), (?>!), (?>=!)
  :: (Show a, Ord a) => Maybe a -> a -> Assertion
ma ?<! b  = maybe pass (<!  b) ma
ma ?<=! b = maybe pass (<=! b) ma
ma ?==! b = maybe pass (==! b) ma
ma ?/=! b = maybe pass (/=! b) ma
ma ?>! b  = maybe pass (>!  b) ma
ma ?>=! b = maybe pass (>=!  b) ma
infix 4 ?<!, ?<=!, ?==!, ?/=!, ?>!, ?>=!

(==?!), (/=?!)
  :: (Show a, Eq a) => a -> Maybe a -> Assertion
(<?!), (<=?!), (>?!), (>=?!)
  :: (Show a, Ord a) => a -> Maybe a -> Assertion
a <?!  mb = maybe pass (a <! ) mb
a <=?! mb = maybe pass (a <=!) mb
a ==?! mb = maybe pass (a ==!) mb
a /=?! mb = maybe pass (a /=!) mb
a >?!  mb = maybe pass (a >! ) mb
a >=?! mb = maybe pass (a >=!) mb
infix 4 <?!, <=?!, ==?!, /=?!, >?!, >=?!

(?==?!), (?/=?!)
  :: (Show a, Eq a) => Maybe a -> Maybe a -> Assertion
(?<?!), (?<=?!), (?>?!), (?>=?!)
  :: (Show a, Ord a) => Maybe a -> Maybe a -> Assertion
ma ?<?!  mb = maybe pass (ma ?<! ) mb
ma ?<=?! mb = maybe pass (ma ?<=!) mb
ma ?==?! mb = maybe pass (ma ?==!) mb
ma ?/=?! mb = maybe pass (ma ?/=!) mb
ma ?>?!  mb = maybe pass (ma ?>! ) mb
ma ?>=?! mb = maybe pass (ma ?>=!) mb
infix 4 ?<?!, ?<=?!, ?==?!, ?/=?!, ?>?!, ?>=?!

(?==), (?/=)
  :: Eq a => Maybe a -> a -> Bool
(?<), (?<=), (?>), (?>=)
  :: Ord a => Maybe a -> a -> Bool
ma ?< b  = maybe True (<  b) ma
ma ?<= b = maybe True (<= b) ma
ma ?== b = maybe True (== b) ma
ma ?/= b = maybe True (/= b) ma
ma ?> b  = maybe True (>  b) ma
ma ?>= b = maybe True (>  b) ma
infix 4 ?<, ?<=, ?==, ?/=, ?>, ?>=

assertRelation :: (Show a) => (a -> a -> Bool) -> Text.Lazy.Text -> a -> a -> Assertion
assertRelation rel relationDoesNotHoldText x y
  | x `rel` y = pass
  | otherwise = failBecause $  cShow x
                            <> relationDoesNotHoldText
                            <> cShow y

--------------------------------------------------------------------------------
-- Logical operators
--------------------------------------------------------------------------------

(||!) :: Assertion -> Assertion -> Assertion
a0 ||! a1 =
  case runValidate a0 of
    Right _    -> pass
    Left errs0 ->
      case runValidate a1 of
        Right _    -> pass
        Left errs1 ->
          -- TODO: we might want to use a data structure like a tree to give
          -- some structure to the errors.
          dispute $ "No term satisfies the assertion: "
                  : fmap (Text.Lazy.cons '\t') (errs0 ++ errs1)
infixr 2 ||!

--------------------------------------------------------------------------------
-- Custom show functions
--------------------------------------------------------------------------------

-- | Customized show. It can show pretty formatted data structures, or not
-- depending on the PRETTY_PRINT_ASSERTIONS compile time flag.
cShow :: Show a => a -> Text.Lazy.Text
cShow =
#if PRETTY_PRINT
  Pretty.pShow
#else
  Text.Lazy.pack . show
#endif

#if PRETTY_PRINT
prettyShow :: Show a => a -> String
prettyShow = Text.Lazy.unpack . Pretty.pShow
#endif

-- | Convert the errors to a string. Each error will be separated by a newline.
showErrors :: [Text.Lazy.Text] -> String
showErrors = intercalate "\n" . fmap Text.Lazy.unpack

orElseShow :: Bool -> Text.Lazy.Text -> Assertion
orElseShow False reason = failBecause reason
orElseShow True  _      = pass

--------------------------------------------------------------------------------
-- Operations on collections
--------------------------------------------------------------------------------

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

allUnique :: (Show a, Ord a) => [a] -> Assertion
allUnique xs =
  case repeated xs of
    [] -> pass
    ys -> failBecause $  "The list should contain unique elements,"
                      <> " however these elements are repeated: "
                      <> cShow ys

holdsFor :: Show a => (a -> Bool) -> a -> Assertion
holdsFor p a
  | p a       = pass
  | otherwise = failBecause $ "Predicate does not hold for " <> cShow a

--------------------------------------------------------------------------------
-- Quantifications
--------------------------------------------------------------------------------

forall :: (Foldable t, Show a) => t a -> (a -> Assertion) -> Assertion
forall domain term = traverse_ checkTerm domain
  where
    checkTerm x
      = mapErrors ((("forall: element does not satisfy the assertion: \n" <> cShow x) :)
                  . fmap (Text.Lazy.cons '\t')
                  )
      $ term x

data Any e = Found | Errors [e]

exists :: (Foldable t) => t a -> (a -> Assertion) -> Assertion
exists domain term = do
  case foldl elementSatisfies (Errors []) domain of
    Found     -> pass
    Errors xs -> dispute $ "exists: No element satisfies the given assertion \n":
                           (reverse $ fmap (Text.Lazy.cons '\t') xs)
  where
    elementSatisfies Found       _ = Found
    elementSatisfies (Errors es) x =
      case runValidate (term x) of
        Right _   -> Found
        Left errs -> Errors (errs ++ es) -- TODO: use a structure with constant concatenation to accumulate errors.
