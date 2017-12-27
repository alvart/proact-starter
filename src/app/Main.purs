{-
  Main.purs
-}

module Main
where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (fromJust)
import DOM (DOM)
import DOM.HTML (window) as D
import DOM.HTML.Window (document) as D
import DOM.HTML.Types (HISTORY, htmlDocumentToParentNode) as D
import DOM.Node.ParentNode (QuerySelector(..), querySelector) as D
import Partial.Unsafe (unsafePartial)
import Prelude
import Proact as P
import ProactPlus (withEvent)
import Pux.DOM.History (sampleURL)
import React (createClass, createFactory) as R
import ReactDOM (render) as R
import Router (RouterFx, routerHandle) as Router
import Signal (runSignal)
import Signal.Channel (CHANNEL)
import Todo (_path, mempty', todo) as Todo

type ReactFx =
  P.EventFx
    (Router.RouterFx (channel :: CHANNEL, dom :: DOM, history :: D.HISTORY))

main :: Eff ReactFx Unit
main =
  unsafePartial
    do
    let element = flip R.createFactory { } $ R.createClass spec
    rDocument <- map D.htmlDocumentToParentNode $ D.window >>= D.document
    rApp <- fromJust <$> D.querySelector (D.QuerySelector "#app") rDocument
    void $ R.render element rApp
  where
  spec =
    (P.spec Todo.todo Todo.mempty') { componentDidMount = registerUrlSignal }

  registerUrlSignal this =
    do
    urlSignal <- D.window >>= sampleURL
    liftEff $ runSignal $ map routeHandler urlSignal
    where
    dispatcher = withEvent $ P.dispatcher' this'

    routeHandler = dispatcher Router.routerHandle

    this' = P.focusThis Todo._path $ P.ReactThis this
