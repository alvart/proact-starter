{-
  @license MIT
  FilterMenu.purs
-}

module Test.FilterMenu
where

import Control.Apply (lift2)
import Control.Monad.Eff.Class (liftEff)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Tuple (Tuple(..))
import Enzyme (ENZYME)
import Enzyme.Shallow (shallow)
import Enzyme.ShallowWrapper (childAt, hasClass, simulate, text)
import FilterMenu (Filter(..), filterMenu) as Filter
import Prelude
import Proact as P
import React as R
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall fx . Spec (enzyme :: ENZYME | fx) Unit
spec = traverseWithIndex_ testFilterTuple $ lift2 Tuple filters filters
  where
  loadMenu state =
    liftEff
      $ shallow
      $ flip R.createFactory { }
      $ R.createClass
      $ P.spec Filter.filterMenu state

  filters = [ Filter.All, Filter.Active, Filter.Completed ]

  readBtn menu btnIndex = liftEff $ childAt btnIndex menu

  readName menu btnIndex = liftEff $ childAt btnIndex menu >>= text

  readSelected menu btnIndex =
    do
    filterButton <- readBtn menu btnIndex
    liftEff $ hasClass "active" filterButton

  testFilterTuple globalIndex (Tuple filter1 filter2) =
    do
    let fName1 = "'" <> show filter1 <> "'"
    let fName2 = "'" <> show filter2 <> "'"

    (it $ "should deactivate " <> fName1 <> " and activate " <> fName2)
      do
      let index1 = globalIndex `div` 3
      let index2 = globalIndex `mod` 3

      menu <- loadMenu filter1

      name1 <- readName menu index1
      name1 `shouldEqual` show filter1

      selected1 <- readSelected menu index1
      selected1 `shouldEqual` true

      button2 <- readBtn menu index2
      liftEff $ void $ simulate "click" button2

      name2 <- readName menu index2
      name2 `shouldEqual` show filter2

      selected2 <- readSelected menu index2
      selected2 `shouldEqual` true
