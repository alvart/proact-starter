{-
  @license MIT
  Proact.purs
-}

module Todo.Proact
  ( Component
  , Dispatcher
  , EventDispatcher
  , EventHandler
  , IndexedComponent
  , (..)
  , _this
  , dispatch
  , setDocumentTitle
  , spec
  , use'
  )
where

import Control.Comonad.Store (StoreT(..))
import Control.Monad.IOSync (IOSync)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Identity (Identity(..))
import Data.Lens (Getter, Iso, Lens', (^.))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude
import Proact.Monad.Class.MonadFree (class MonadFree, liftFree)
import Proact.React
  ( ComponentT
  , Dispatcher
  , EventHandlerT
  , IndexedComponentT
  , ReactEff
  , dispatch
  , spec
  ) as P
import React (ReactElement, ReactSpec, ReactThis, readState)
import Todo.Program (Idle(..), TodoF(..))

-- | An alias for Semigroupoid composition.
infixr 9 compose as ..

-- | A type synonym for a React Component with no additional side effects.
type Component s = P.ComponentT s Idle Identity TodoF Identity

-- | A type synonym for a Dispatcher with no additional side effects..
type Dispatcher s a = P.Dispatcher s TodoF Identity a

-- | A type synonym for a function dispatching React actions given an event
-- | argument.
type EventDispatcher v a = v -> P.ReactEff a

-- | A type synonym for an Event Handler with no additional side effects.
type EventHandler s = P.EventHandlerT s TodoF Identity

-- | A type synonym for an Indexed Component with no special side effects.
type IndexedComponent i s = P.IndexedComponentT i s Idle Identity TodoF Identity

-- | An alias for the identity isomorphism.
_this :: forall a b . Iso a b a b
_this = id

-- | Executes actions of an event handler over a React object.
dispatch :: forall s1 s2 . Lens' s1 s2 -> ReactThis { } s1 -> Dispatcher s2 Unit
dispatch _lens this eventHandler =
  do
  state <- readState this
  P.dispatch _lens Idle (start state) this eventHandler

-- | Sets the document's title.
setDocumentTitle :: forall m . MonadFree TodoF m => String -> m Unit
setDocumentTitle title = liftFree $ SetDocumentTitle title unit

-- | Creates a `ReactSpec` from a React Component.
spec
  :: forall s e
   . s -> Component s ReactElement -> ReactSpec { } s ReactElement e
spec state = P.spec Idle (start state)

-- | An implementation of `use` from the Data.Lens library working in the
-- | `MonadAsk` context.
use' :: forall s t a b m . MonadAsk s m => Getter s t a b -> m a
use' _get = asks (_ ^. _get)

-- The initial step of the interpreter Coalgebra.
start :: forall s . s -> StoreT (Maybe s) Identity (IOSync Unit)
start = StoreT <<< Tuple (Identity $ const $ pure unit) <<< Just
