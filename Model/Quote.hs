module Model.Quote where

import Prelude ((-), (+))
import Data.Int (Int64)
import Control.Applicative (liftA2)
import Model (Quote, quoteVotesFor, quoteVotesAgainst)

quoteRating :: Quote -> Int64
quoteRating = liftA2 (-) quoteVotesFor quoteVotesAgainst


quoteVotes :: Quote -> Int64
quoteVotes = liftA2 (+) quoteVotesFor quoteVotesAgainst
