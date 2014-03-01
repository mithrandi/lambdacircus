module Handler.Quotes where

import Import
import Data.Time (getCurrentTime, formatTime)
import System.Locale (defaultTimeLocale)
import qualified Data.HashMap.Strict as H
import Data.Aeson.Types (Pair)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Network.HTTP.Types (status204)
import Yesod.Auth (maybeAuth)
import Model.Quote (quoteRating, quoteVotes)
import Data.Maybe (fromMaybe)
import Data.Text.Read (decimal)


quoteWidget :: QuoteId -> Quote -> Widget
quoteWidget quoteId quote = do
    $(widgetFile "quote")


parseQid :: Text -> Maybe QuoteId
parseQid = either (const Nothing) (Just . Key . PersistInt64 . fst) . decimal


getQuotesR :: Handler TypedContent
getQuotesR = do
    limitS <- lookupGetParam "limit"
    let limit = fromMaybe 10 (either (const Nothing) (Just . fst) . decimal =<< limitS)
    from <- lookupGetParam "from"
    let criteria = maybe [] (\q -> [QuoteId <=. q]) (parseQid =<< from)
    quotes <- runDB $ selectList criteria [Desc QuoteAdded, LimitTo limit]
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


quoteJson :: (QuoteId, Quote) -> Handler Value
quoteJson (qid, quote) = do
    mu <- maybeAuth
    let deletable = case mu of
            Nothing -> False
            Just (Entity _ user) -> userModerator user
    r <- getUrlRender
    return $ jsonWithExtras
        ["id" .= qid,
         "voteUp" .= r (QuoteUpR qid),
         "voteDown" .= r (QuoteDownR qid),
         "deletable" .= deletable,
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


deleteQuoteR :: QuoteId -> Handler ()
deleteQuoteR qid = do
    runDB $ delete qid
    sendResponseStatus status204 ()


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
