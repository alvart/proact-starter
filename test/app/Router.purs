{-
  @license MIT
  Router.purs
-}

module Test.Router
where

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.Class (liftEff)
import Enzyme (ENZYME)
import Prelude
import Proact.Trans.Class.Interpret (interpret) as P
import Router (Path(..), routerHandle) as Router
import Test.Program (TodoF(..))
import Test.Program (dispatch) as P
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)
import Todo.Program (TodoF(..)) as App

spec :: forall e . Spec (enzyme :: ENZYME | e) Unit
spec =
  do
  (it "should route to the home page")
    do
    let url = ""
    liftEff
      $ P.dispatch Router.Home
      $ P.interpret (mockF Router.Home)
      $ Router.routerHandle url
  (it "should route to 404 page")
    do
    let url = "NOT FOUND"
    liftEff
      $ P.dispatch Router.Home
      $ P.interpret (mockF (Router.NotFound url))
      $ Router.routerHandle url
  where
  mockF :: Router.Path -> (App.TodoF ~> TodoF)
  mockF path (App.SetDocumentTitle title a) =
    TodoF
      do
      liftEff $ launchAff_ $ show path `shouldEqual` title
      pure a
