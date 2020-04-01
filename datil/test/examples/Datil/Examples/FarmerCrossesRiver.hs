{-# LANGUAGE OverloadedStrings #-}

-- | A farmer crosses a river
--
-- Farmer wants to cross a river and take with him a wolf, a goat, and a cabbage.
--
-- There is a boat that can fit himself plus either the wolf, the goat, or the
-- cabbage.
--
-- If the wolf and the goat are alone on one shore, the wolf will eat the goat.
-- If the goat and the cabbage are alone on the shore, the goat will eat the
-- cabbage.
--
-- How can the farmer bring the wolf, the goat, and the cabbage across the
-- river?
--
-- Source: https://www.mathsisfun.com/puzzles/farmer-crosses-river.html
--
module Datil.Examples.FarmerCrossesRiver where

import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Test.QuickCheck (Gen, arbitrary, elements, oneof)
import           Test.Tasty (TestTree, testGroup)

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Expr
import           Control.State.DataAutomata.Interpreter.Gen
import           Control.State.DataAutomata.Interpreter.Memory hiding (remove)
import           Control.State.DataAutomata.Interpreter.Run
import           Control.State.DataAutomata.Interpreter.Trace

import           Control.State.DataAutomata.Test.Properties
import           Control.State.DataAutomata.Test.Run


farmer :: DataAutomaton
farmer =
  DataAutomaton
  { start       = "InA"
  , transitions =
    [ -- Going to B.
      "InA" .--
      ( a ∈ shoreA
      , "leftWith" #? a
      , [ shoreA := shoreA // a
        , boat   .= a
        ]
      )
      .--> "AtoB"
    , "AtoB"
      .-- ( safe shoreA
          , "arrivedToB" #! ()
          , shoreB := shoreB ∪ boat
          )
      .--> "InBChecking"
    , "InBChecking"
      .-- ( allInB shoreB
          , "success" #! ()
          )
      .--> "Succeeded"
    , "InBChecking"
      .-- ( Not $ allInB shoreB
          , "continues" #! ()
          )
      .--> "InB"
    -- Coming back to A.
    , "InB"
      .-- ( a ∈ shoreB
          , "leftWith" #? a
          , [ shoreB := shoreB // a
            , boat   .= a
            ]
          )
      .--> "BtoA"
    , "BtoA"
      .-- ( safe shoreB
          , "arrivedToA" #! ()
          , shoreA := shoreA ∪ boat
          )
      .--> "InA"
    -- Checking for failures.
    , "AtoB"
      .-- ( Not $ safe shoreA
          , "fail" #!! why shoreA
          )
      .--> "Failed"
    , "BtoA"
      .-- ( Not $ safe shoreB
          , "fail" #!! why shoreB
          )
      .--> "Failed"
    ]
  }
  where
    a :: Var (Maybe Item)
    a = "a"

data Item = Wolf | Goat | Cabbage
  deriving (Eq, Ord, Show)

data Failure = WolfAteGoat | GoatAteCabbage
  deriving (Eq, Show)

(∈) :: Var (Maybe Item) -> Var (Set Item) -> Expr Bool
vi ∈ vis = Fapply2 "member" (VarE vi) (VarE vis)

-- | Check whether the item is in the shore. 'Nothing' is always in the shore :)
member :: Maybe Item -> Set Item -> Bool
member Nothing  _     = True
member (Just i) items = Set.member i items

(//) :: Var (Set Item) -> Var (Maybe Item) -> Expr (Set Item)
xs // x = Fapply2 "remove" (VarE x) (VarE xs)

remove :: Maybe Item -> Set Item -> Set Item
remove Nothing  items = items
remove (Just i) items = Set.delete i items

(∪) :: Var (Set Item) -> Var (Maybe Item) -> Expr (Set Item)
xs ∪ x = Fapply2 "insert" (VarE x) (VarE xs)

insert :: Maybe Item -> Set Item -> Set Item
insert Nothing  items = items
insert (Just i) items = Set.insert i items

shoreA :: Var (Set Item)
shoreA = "shoreA"

shoreB :: Var (Set Item)
shoreB = "shoreB"

boat :: Var (Maybe Item)
boat = "boat"

safe :: Var (Set Item) -> Expr Bool
safe shore = Fapply "safe'" (VarE shore)

safe' :: Set Item -> Bool
safe' items =
  case why' items of
    Nothing -> True
    _       -> False

why :: Var (Set Item) -> Expr (Maybe Failure)
why shore = Fapply "why'" (VarE shore)

why' :: Set Item -> Maybe Failure
why' items
  | Set.member Wolf items && Set.member Goat     items = Just WolfAteGoat
  | Set.member Goat items && Set.member Cabbage  items = Just GoatAteCabbage
  | otherwise                                          = Nothing

allInB :: Var (Set Item) -> Expr Bool
allInB items = Fapply "allInB'" (VarE items)

allInB' :: Set Item -> Bool
allInB' items = Set.size items == 3

runnableFarmer :: RunnableModel
runnableFarmer =
  RunnableModel
  { initialMemory = Leaf   initMem
  , automata      = Single farmer
  }
  where
    initMem :: Memory
    initMem
      = Memory
      $ Map.fromList
      $ [ (unVar shoreA, Cell (Set.fromList [Wolf, Goat, Cabbage]))
        , (unVar shoreB, Cell (Set.fromList [] :: Set Item))
        , (unVar boat  , Cell (undefined :: Maybe Item))
        , ("member"    , Cell (Function member))
        , ("remove"    , Cell (Function remove))
        , ("insert"    , Cell (Function insert))
        , ("safe'"     , Cell (Function safe'))
        , ("why'"      , Cell (Function why'))
        , ("allInB'"   , Cell (Function allInB'))
        ]

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

tests :: TestTree
tests
  = testGroup "farmer crosses the river"
  $ with runnableFarmer
  $ [ []                                    `shouldEndInState` "InA"

    , [ leftWith Nothing ]                  `shouldEndInState` "AtoB"

    , [ leftWith Nothing
      , failBecause (Just WolfAteGoat) ]    `shouldEndInState` "Failed"

    , [ leftWith (Just Wolf)
      , failBecause (Just GoatAteCabbage) ] `shouldEndInState` "Failed"

    , [ leftWith (Just Cabbage)
      , failBecause (Just WolfAteGoat) ]    `shouldEndInState` "Failed"

    , [ leftWith (Just Goat)
      , theFarmer "arrivedToB" ]            `shouldEndInState` "InBChecking"

    , [ leftWith (Just Goat)
      , theFarmer "arrivedToB"
      , theFarmer "continues" ]             `shouldEndInState` "InB"

    , [ leftWith (Just Goat)
      , theFarmer "arrivedToB"
      , theFarmer "continues"
      , leftWith (Just Goat) ]              `shouldEndInState` "BtoA"

    , [ leftWith (Just Goat)
      , theFarmer "arrivedToB"
      , theFarmer "continues"
      , leftWith Nothing ]                  `shouldEndInState` "BtoA"

    , [ leftWith (Just Goat)
      , theFarmer "arrivedToB"
      , theFarmer "continues"
      , leftWith Nothing
      , theFarmer "arrivedToA" ]            `shouldEndInState` "InA"
    , solution                              `shouldEndInState` "Succeeded"
    ]
  where
    solution =
      [ -- In A: W, G, C
        leftWith (Just Goat)
        -- In B: G
      , theFarmer "arrivedToB"
      , theFarmer "continues"
      , leftWith Nothing
      -- In A: W, C
      , theFarmer "arrivedToA"
      , leftWith (Just Wolf)
      -- In B: W, G
      , theFarmer "arrivedToB"
      , theFarmer "continues"
      , leftWith (Just Goat)
      -- In A: C, G
      , theFarmer "arrivedToA"
      , leftWith (Just Cabbage)
      -- In B: W, C
      , theFarmer "arrivedToB"
      , theFarmer "continues"
      , leftWith Nothing
      -- In A: C
      , theFarmer "arrivedToA"
      , leftWith (Just Goat)
      -- In B: W, C, G
      , theFarmer "arrivedToB"
      , CAction "success" ()
      ]

leftWith :: Maybe Item -> CAction
leftWith item = CAction "leftWith" item

failBecause :: Maybe Failure -> CAction
failBecause failure = CAction "fail" failure

theFarmer :: ActionName -> CAction
theFarmer what = CAction what ()

--------------------------------------------------------------------------------
-- Property tests
--------------------------------------------------------------------------------

testableFarmer :: GeneratorModel
testableFarmer =
  GeneratorModel
  { actionGenerators = Map.fromList
                     [ ("leftWith", Cell <$> someItem)]
  , runnableModel    = runnableFarmer
  }
  where
    someItem :: Gen (Maybe Item)
    someItem = oneof [pure Nothing, Just <$> elements [Wolf, Goat, Cabbage]]


propertyTests :: TestTree
propertyTests
  = testGroup "farmer crosses the river"
  $ with testableFarmer
  [ actionIsTriggered 10000 (Desired 500) "success"
  ]
