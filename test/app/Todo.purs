{-
  @license MIT
  Todo.purs
-}

module Test.Todo
where

import Enzyme (ENZYME)
import Prelude
import Test.Spec (Spec, it)
import Test.Spec.Assertions (fail)

spec :: forall fx . Spec (enzyme :: ENZYME | fx) Unit
spec = it "should fail" $ fail "TODO: Write tests for TODO"
