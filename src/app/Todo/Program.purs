{-
  @license MIT
  Program.purs
-}

module Todo.Program
  ( Idle(..)
  , TodoF(..)
  )
where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.IOSync (IOSync)
import Data.Newtype (class Newtype)
import Document (setDocumentTitle)
import Prelude
import Proact.Functor.Pairing (class PairingM)

-- | Represents the chat commands used by components.
data TodoF a = SetDocumentTitle String a

-- Represents the absence of actions.
newtype Idle a = Idle a

-- TodoF :: Functor
derive instance functorTodoF :: Functor TodoF

-- Idle :: Newtype, Functor
derive instance newtypeIdle :: Newtype (Idle a) _

derive instance functorIdle :: Functor Idle

-- (Idle, TodoF, IOSync) :: PairingM
instance pairingMIdleTodoFIOSync :: PairingM Idle TodoF IOSync
  where
  pairM p (Idle a) (SetDocumentTitle title b) =
    do
    liftEff $ setDocumentTitle title
    p a b
