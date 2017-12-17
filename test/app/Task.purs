{-
  @license MIT
  Task.purs
-}

module Test.Task
where

import Control.Monad.Eff.Class (liftEff)
import Data.Lens ((.~))
import Data.Monoid (mempty)
import Enzyme (ENZYME)
import Enzyme.Shallow (shallow)
import Enzyme.ShallowWrapper (childAt, text)
import Prelude
import Proact as P
import React as R
import Task (_description, task) as Task
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall fx . Spec (enzyme :: ENZYME | fx) Unit
spec =
  (it "should have a description")
    do
    let description = "Task Description"

    task <- loadTask $ mempty # Task._description .~ description
    description' <- readDescription task

    description `shouldEqual` description'
  where
  loadTask taskState =
    liftEff
      $ shallow
      $ flip R.createFactory { }
      $ R.createClass
      $ P.spec (Task.task noAction) taskState

  noAction _ = pure unit

  readDescription task = liftEff $ childAt 1 task >>= childAt 0 >>= text
