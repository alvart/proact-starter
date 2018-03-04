{-
  Router.purs
-}

module Router
( Path(..)
, State
, routerHandle
)
where

import Data.Lens ((.=))
import Data.Maybe (fromMaybe)
import Prelude
import Pux.Router (end, router) as Pux
import Todo.Proact (EventHandler, _this, setDocumentTitle)

-- | The list of accepted URLs for this application.
data Path =
  Home
  | NotFound String

-- | A type synonymous for the application path that is also the state of the
-- | router component.
type State = Path

-- Path :: Show
instance showPath :: Show (Path)
  where
  show Home = "Welcome Page"
  show (NotFound _) = "404 Not Found"

routerHandle :: String -> EventHandler State Unit
routerHandle url =
  do
  let path = fromMaybe (NotFound url) $ Pux.router url urlDecoder
  _this .= path
  setDocumentTitle $ show path
  where
  urlDecoder = Home <$ Pux.end
