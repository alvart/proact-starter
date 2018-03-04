{-
  Main.purs
-}

module Main
where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Lens (Lens')
import Data.Maybe (fromJust)
import DOM (DOM)
import DOM.HTML (window) as D
import DOM.HTML.Window (document) as D
import DOM.HTML.Types (HISTORY, htmlDocumentToParentNode) as D
import DOM.Node.ParentNode (QuerySelector(..), querySelector) as D
import Partial.Unsafe (unsafePartial)
import Prelude
import Proact.React (ReactEff) as P
import Pux.DOM.History (sampleURL)
import React (createClass, createFactory) as R
import ReactDOM (render) as R
import Router (routerHandle) as Router
import Signal (runSignal)
import Signal.Channel (CHANNEL)
import Todo (State, _path, empty, todo) as Todo
import Todo.Proact ((..))
import Todo.Proact (EventHandler, dispatch, spec) as P

main :: Eff (channel :: CHANNEL, dom :: DOM, history :: D.HISTORY) Unit
main =
  unsafePartial
    do
    rDocument <- map D.htmlDocumentToParentNode $ D.window >>= D.document
    rApp <- fromJust <$> D.querySelector (D.QuerySelector "#app") rDocument
    let element = flip R.createFactory { } $ R.createClass spec
    void $ R.render element rApp
  where
  spec = (P.spec Todo.empty Todo.todo) { componentDidMount = registerUrlSignal }

  registerUrlSignal this =
    do
    urlSignal <- D.window >>= sampleURL
    unsafeCoerceEff $ runSignal $ map routeHandler urlSignal
    where
    dispatcher
      :: forall s
       . Lens' Todo.State s
      -> (forall v . (v -> P.EventHandler s Unit) -> (v -> P.ReactEff Unit))
    dispatcher _lens = (..) $ P.dispatch _lens this

    routeHandler = dispatcher Todo._path Router.routerHandle
