{-
  @license MIT
  Task.purs
-}

module Test.Task
where

import Control.Monad.Eff.Class (liftEff)
import Data.Array (singleton)
import Data.Lens ((.~))
import Data.Lens.Indexed (itraversed)
import Enzyme (ENZYME)
import Enzyme.Shallow (shallow)
import Enzyme.ShallowWrapper (childAt, text)
import Prelude
import Proact.React (iFocus) as P
import React (createClass, createFactory) as R
import React.DOM (div') as R
import Task (_description, empty, task) as Task
import Test.Program (TodoF(..))
import Test.Program (mock, spec) as P
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)
import Todo.Program (TodoF(..)) as App

spec :: forall e . Spec (enzyme :: ENZYME | e) Unit
spec =
  (it "should have a description")
    do
    let description = "Task Description"
    task <- loadTask $ Task.empty # Task._description .~ description
    description' <- readDescription task
    description `shouldEqual` description'
  where
  loadTask state =
    liftEff
      $ shallow
      $ flip R.createFactory { }
      $ R.createClass
      $ P.spec [ state ]
      $ map R.div'
      $ P.iFocus itraversed
      $ map singleton
      $ P.mock mockF
      $ Task.task \_ -> pure unit

  mockF :: App.TodoF ~> TodoF
  mockF (App.SetDocumentTitle _ a) = TodoF (pure a)

  readDescription task =
    liftEff $ childAt 0 task >>= childAt 1 >>= childAt 0 >>= text
