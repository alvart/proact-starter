{-
  Main.purs
-}

module Test.Main
where

import Control.Monad.Eff (Eff)
import Enzyme (ENZYME)
import Prelude
import Test.FilterMenu (spec) as Filter
import Test.Spec (SpecEffects, describe)
import Test.Spec.Mocha (MOCHA, runMocha)
import Test.Task (spec) as Task
import Test.Todo (spec) as Todo

main :: Eff (SpecEffects (enzyme :: ENZYME, mocha :: MOCHA)) Unit
main =
  runMocha
    do
    describe "Tasks" Task.spec
    describe "Filter spec" Filter.spec
    describe "To-do application" Todo.spec
