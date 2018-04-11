module DOM.HTML.Document.Extra (setDocumentTitle) where

import Prelude (Unit)
import DOM (DOM)
import DOM.HTML.Types (HTMLDocument)
import DOM.HTML.History (DocumentTitle)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)


foreign import setDocumentTitleImpl :: forall eff. EffFn2 (dom :: DOM | eff) HTMLDocument DocumentTitle Unit

setDocumentTitle :: forall eff. HTMLDocument -> DocumentTitle -> Eff (dom :: DOM | eff) Unit
setDocumentTitle = runEffFn2 setDocumentTitleImpl
