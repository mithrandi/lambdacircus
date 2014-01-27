module Handler.Quotes where

import Import
import Data.Time (getCurrentTime, formatTime)
import System.Locale (defaultTimeLocale)
import qualified Data.HashMap.Strict as H
import Data.Aeson.Types (Pair)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Model.Quote (quoteRating, quoteVotes)

quoteWidget :: QuoteId -> Quote -> Widget
quoteWidget quoteId quote = do
    $(widgetFile "quote")


getQuotesR :: Handler TypedContent
getQuotesR = do
    quotes <- runDB $ selectList [] [Desc QuoteAdded]
    selectRep $ do
        provideRep $ do
            values <- mapM (\(Entity qid q) -> quoteJson (qid, q)) quotes
            return $ toJSON values
        provideRep $ defaultLayout $ do
                setTitle "Quotes"
                $(widgetFile "quotes")


jsonWithExtras :: (ToJSON a) => [Pair] -> a -> Value
jsonWithExtras extras entity = case toJSON entity of
    Object o -> Object $ foldl (\m (k, v) -> H.insert k v m) o extras
    _ -> error "Unexpected JSON representation of entity"


jsonWithId :: (ToJSON a, ToJSON b) => a -> b -> Value
jsonWithId eid = jsonWithExtras ["id" .= eid]


quoteJson :: (QuoteId, Quote) -> Handler Value
quoteJson (qid, quote) = do
    r <- getUrlRender
    return $ jsonWithExtras
        ["id" .= qid,
         "voteUp" .= r (QuoteUpR qid),
         "voteDown" .= r (QuoteDownR qid),
         "self" .= r (QuoteR qid),
         "rating" .= quoteRating quote,
         "votes" .= quoteVotes quote] quote


getQuoteR :: QuoteId -> Handler TypedContent
getQuoteR qid = do
    quote <- runDB $ get404 qid
    selectRep $ do
        provideRep $ quoteJson (qid, quote)
        provideRep $ defaultLayout $ do
            setTitle "Quote"
            quoteWidget qid quote


postQuoteUpR :: QuoteId -> Handler Html
postQuoteUpR quoteId = do
    runDB $ update quoteId [QuoteVotesFor +=. 1]
    redirect (QuoteR quoteId)


postQuoteDownR :: QuoteId -> Handler Html
postQuoteDownR quoteId = do
    runDB $ update quoteId [QuoteVotesAgainst +=. 1]
    redirect (QuoteR quoteId)


getNewQuoteR :: Handler Html
getNewQuoteR = defaultLayout $ do
    setTitle "New quote"
    $(widgetFile "newquote")

data PartialQuote = PartialQuote {
    content :: Text
    }

$(deriveJSON defaultOptions ''PartialQuote)

postNewQuoteR :: Handler Value
postNewQuoteR = do
    value <- parseJsonBody_
    created <- liftIO getCurrentTime
    let q = Quote (content value) created 0 0
    qid <- runDB $ insert q
    quoteJson (qid, q)
