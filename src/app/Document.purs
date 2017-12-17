{-
  @license MIT
  Document.purs
-}

module Document
where

import Control.Monad.Eff (kind Effect, Eff)
import Prelude

foreign import data DOCUMENT :: Effect

foreign import setDocumentTitle ::
  forall fx . String -> Eff (document :: DOCUMENT | fx) Unit
