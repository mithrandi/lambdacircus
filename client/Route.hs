{-# LANGUAGE QuasiQuotes #-}
module Route where

import Types (QuoteId)
import Yesod.Core (renderRoute)
import Yesod.Core.Dispatch (mkYesod, parseRoutes)

mkYesod "LC" [parseRoutes|
/ RootR GET
/top/pages/#Integer TopQuotesR GET
/quotes DefQuotesR GET
/quotes/!from/#Integer QuotesR GET
/quotes/#QuoteId QuoteR GET DELETE
/newQuote NewQuoteR GET POST
|]

data LC = LC

