module Types where

import Control.Lens
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Data.Char (toLower)
import Data.Int (Int64)
import Data.List.Lens (prefixed)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Thyme (UTCTime)
import Data.Thyme.Format.Aeson ()

type QuoteId = Int64

data Quote = Quote
             { _quoteId           :: QuoteId
             , _quoteContent      :: !Text
             , _quoteAdded        :: !UTCTime
             , _quoteVotesFor     :: !Int64
             , _quoteVotesAgainst :: !Int64
             , _quoteRating       :: !Int64
             , _quoteVotes        :: !Int64
             } deriving (Show, Eq)

$(deriveJSON
  defaultOptions
  { fieldLabelModifier = over _head toLower . view (prefixed "_quote") }
  ''Quote)
makeLenses ''Quote

data QuoteList = QuoteList
                 { _qlPrev   :: !Text
                 , _qlNext   :: !Text
                 , _qlQuotes :: [Quote]
                 } deriving (Show, Eq)

$(deriveJSON
  defaultOptions
  { fieldLabelModifier = over _head toLower . view (prefixed "_ql") }
  ''QuoteList)
makeLenses ''QuoteList

data CircusS = CSQuotes
               { _csQuotes :: !(Seq Quote)
               } deriving (Show, Eq)

makeLenses ''CircusS
makePrisms ''CircusS

data CircusA = UpdateQuote Quote
             | ReplaceQuotes QuoteList
             deriving (Show, Eq)

data CircusR = FetchQuotes { url :: Text }
