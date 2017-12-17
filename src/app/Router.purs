{-
  Router.purs
-}

module Router
( Path(..)
, RouterFx
, State
, router
)
where

import Control.Monad.Eff.Class (liftEff)
import Data.Lens ((.=))
import Data.Maybe (fromMaybe)
import Data.Profunctor (lmap)
import Document (DOCUMENT, setDocumentTitle)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (HISTORY)
import Prelude
import Proact as P
import ProactPlus (_this)
import Pux.Router (end, router) as PX
import Pux.DOM.History (sampleURL)
import React (ReactElement, handle) as R
import React.DOM (img) as R
import React.DOM.Props (src, style, unsafeMkProps) as R
import Signal (runSignal)
import Signal.Channel (CHANNEL)

-- | The list of accepted URLs for this application.
data Path =
  Home
  | NotFound String

-- | A type synonymous for the application path that is also the state of the
-- | router component.
type State = Path

-- | A type synonymous for the effects associated with the Router component.
type RouterFx fx =
  ( channel :: CHANNEL
  , dom :: DOM
  , history :: HISTORY
  , document :: DOCUMENT
  | fx
  )

-- Path :: Show
instance showPath :: Show (Path)
  where
  show Home = "Welcome Page"
  show (NotFound _) = "404 Not Found"

-- A component that subscribes to changes to the window's URL and sets the
-- current path in the application's state accordingly. The mechanism requires
-- an attempt to load a dummy HTML element that will be hidden from the user.
router :: forall fx .  P.Component (RouterFx fx) State R.ReactElement
router =
  do
  dispatcher <- P.eventDispatcher
  pure $ view dispatcher
  where
  view dispatcher =
    R.img
      [ R.style { display : "none" }
      , R.src ""
      , onError \_ -> registerUrlSignal
      ]
      [ ]
    where
    decodeUrl url = fromMaybe (NotFound url) $ PX.router url urlDecoder

    onError = R.unsafeMkProps "onError" <<< R.handle

    registerUrlSignal =
      do
      urlSignal <- DOM.window >>= sampleURL
      liftEff
        $ runSignal
        $ flip map urlSignal
        $ dispatcher
        $ lmap decodeUrl setUrl

    setUrl path =
      do
      _this .= path
      liftEff $ setDocumentTitle $ show path

    urlDecoder = Home <$ PX.end
