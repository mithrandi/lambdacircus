module Types where

import           Control.Lens
import           Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import           Data.Char (toLower)
import           Data.Int (Int64)
import           Data.List.Lens (prefixed)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.Thyme (UTCTime)
import           Data.Thyme.Format.Aeson ()

type QuoteId = Int64

data Quote = Quote
             { _quoteId           :: !QuoteId
             , _quoteContent      :: !Text
             , _quoteAdded        :: !UTCTime
             , _quoteVotesFor     :: !Int64
             , _quoteVotesAgainst :: !Int64
             , _quoteRating       :: !Int64
             , _quoteVotes        :: !Int64
             , _quoteDeletable    :: !Bool
             , _quoteVoteUp       :: !Text
             , _quoteVoteDown     :: !Text
             } deriving (Show, Read, Eq)

$(deriveJSON
  defaultOptions
  { fieldLabelModifier = over _head toLower . view (prefixed "_quote") }
  ''Quote)
makeLenses ''Quote

data QuoteList = QuoteList
                 { _qlPrev   :: Maybe Text
                 , _qlNext   :: Maybe Text
                 , _qlQuotes :: [Quote]
                 }
               deriving (Show, Read, Eq)

$(deriveJSON
  defaultOptions
  { fieldLabelModifier = over _head toLower . view (prefixed "_ql")
  , omitNothingFields = True}
  ''QuoteList)
makeLenses ''QuoteList
makePrisms ''QuoteList

data QuoteState = QSNormal
                | QSVoting
                | QSVoted
                deriving (Show, Read, Eq)
makePrisms ''QuoteState

data CircusS = CSQuotes
               { _csQuotes      :: !QuoteList
               , _csQuoteStates :: !(M.Map QuoteId QuoteState)
               }
             | CSNewQuote
               { _csContent     :: !Text
               }
             deriving (Show, Read, Eq)
makeLenses ''CircusS
makePrisms ''CircusS

data CircusA = UpdateQuote Quote
             | UpdateQuoteState QuoteId QuoteState
             | ReplaceQuotes QuoteList
             | VoteA QuoteId Text
             | ChangeRoute Text
             | ChangeContent Text
             | CreateQuoteA
             deriving (Show, Eq)

data CircusR = FetchQuotes Text
             | FetchQuote Text
             | VoteR QuoteId Text
             | InitRoute
             | CreateQuoteR Text
             deriving (Show, Eq)
