{-
  Router.purs
-}

module Router
( Path(..)
, RouterFx
, State
, routerHandle
)
where

import Control.Monad.Eff.Class (liftEff)
import Data.Lens ((.=))
import Data.Maybe (fromMaybe)
import Document (DOCUMENT, setDocumentTitle)
import Prelude
import Proact as P
import ProactPlus (_this)
import Pux.Router (end, router) as Pux

-- | The list of accepted URLs for this application.
data Path =
  Home
  | NotFound String

-- | A type synonymous for the application path that is also the state of the
-- | router component.
type State = Path

-- | A type synonymous for the effects associated with the Router component.
type RouterFx fx = (document :: DOCUMENT | fx)

-- Path :: Show
instance showPath :: Show (Path)
  where
  show Home = "Welcome Page"
  show (NotFound _) = "404 Not Found"

routerHandle :: String -> forall fx . P.EventHandler (RouterFx fx) State Unit
routerHandle url =
  do
  let path = fromMaybe (NotFound url) $ Pux.router url urlDecoder
  _this .= path
  liftEff $ setDocumentTitle $ show path
  where
  urlDecoder = Home <$ Pux.end
