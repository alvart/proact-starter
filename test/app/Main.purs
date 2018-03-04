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
import Test.Router (spec) as Router
import Test.Task (spec) as Task
import Test.Todo (spec) as Todo

main :: Eff (SpecEffects (enzyme :: ENZYME, mocha :: MOCHA)) Unit
main =
  runMocha
    do
    describe "Router" Router.spec
    describe "Task" Task.spec
    describe "Filter Menu" Filter.spec
    describe "Application" Todo.spec
