{-
  @license MIT
  Program.purs
-}

module Test.Program
where

import Control.Comonad.Store (StoreT(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.IOSync (IOSync, runIOSync)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Tuple (Tuple(..))
import Enzyme (ENZYME)
import Prelude
import Proact
  (ComponentT, EventHandlerT, VaultT(..), cointerpret, dispatch) as P
import Proact.Comonad.Trans.Cofree (coiterT)
import Proact.React (spec) as P
import Proact.Functor.Pairing (class PairingM)
import Proact.Trans.Class.Interpret (interpret) as P
import React (ReactElement, ReactSpec)
import Todo.Program (Idle, TodoF) as App

newtype Idle a = Idle a

newtype TodoF a = TodoF (IOSync a)

-- Idle: Newtype, Functor
derive instance newtypeIdle :: Newtype (Idle a) _

derive instance functorIdle :: Functor Idle

-- TodoF: Newtype, Functor
derive instance newtypeTodoF :: Newtype (TodoF a) _

derive instance functorTodoF :: Functor TodoF

-- (Idle, TodoF, IOSync) :: PairingM
instance pairingMIdleTodoFIOSync :: PairingM Idle TodoF IOSync
  where
  pairM p (Idle a) = (_ >>= p a) <<< unwrap

dispatch
  :: forall s e
   . s
  -> P.EventHandlerT s TodoF Identity Unit
  -> Eff (enzyme :: ENZYME | e) Unit
dispatch state =
  unsafeCoerceEff
    <<< runIOSync
    <<< P.dispatch (P.VaultT $ coiterT Idle $ start state)

mock
  :: forall s t a
   . (App.TodoF ~> TodoF)
  -> P.ComponentT s t IOSync App.Idle Identity App.TodoF Identity a
  -> P.ComponentT s t IOSync Idle Identity TodoF Identity a
mock f = P.cointerpret (over Idle id) <<< P.interpret f

spec
  :: forall s e
   . s
  -> P.ComponentT s s IOSync Idle Identity TodoF Identity ReactElement
  -> ReactSpec { } s ReactElement e
spec state = P.spec Idle (start state)

start :: forall s . s -> StoreT (Maybe s) Identity (IOSync Unit)
start = StoreT <<< Tuple (Identity $ const $ pure unit) <<< Just
