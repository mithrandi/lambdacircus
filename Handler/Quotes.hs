module Handler.Quotes where

import Import
import Data.Time (getCurrentTime, formatTime)
import System.Locale (defaultTimeLocale)
import qualified Data.HashMap.Strict as H
import Data.Aeson.Types (Pair)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Network.HTTP.Types (status204)
import Yesod.Auth (maybeAuth)
import Data.Maybe (catMaybes)
import Control.Monad (guard)


quoteWidget :: QuoteId -> Quote -> Widget
quoteWidget quoteId quote = do
    $(widgetFile "quote")


renderQuotes :: [Entity Quote] -> Maybe Text -> Maybe Text -> Handler TypedContent
renderQuotes quotes prev next =
    selectRep $ do
        provideRep $ do
            values <- mapM (\(Entity qid q) -> quoteJson (qid, q)) quotes
            return . object . catMaybes $ [
                Just $ "quotes" .= values,
                ("prev" .=) `fmap` prev,
                ("next" .=) `fmap` next]
        provideRep $ defaultLayout $ do
                setTitle "Quotes"
                $(widgetFile "quotes")


getDefQuotesR :: Handler TypedContent
getDefQuotesR = do
    Just (Entity (Key (PersistInt64 qid)) _) <- runDB $ selectFirst [] [Desc QuoteAdded, LimitTo 1]
    redirect $ QuotesR (fromIntegral qid)


getQuotesR :: Integer -> Handler TypedContent
getQuotesR from = do
    let limit = 10
    quotes <- runDB $ selectList
        [QuoteId <=. Key (PersistInt64 (fromInteger from))]
        [Desc QuoteAdded, LimitTo limit]
    r <- getUrlRender
    let prev = Just . r . QuotesR $ from + limit
        next = guard (from > 1) >> (Just . r . QuotesR) (max (from - limit) 1)
    renderQuotes quotes prev next


getTopQuotesR :: Integer -> Handler TypedContent
getTopQuotesR page = do
    let limit = 10
    quotes <- runDB $ selectList [] [
        Desc QuoteRating, LimitTo limit, OffsetBy . fromInteger $ page * limit]
    r <- getUrlRender
    let prev = guard (page > 0) >> (Just . r . TopQuotesR) (page - 1)
        next = guard (length quotes > 0) >> (Just . r . TopQuotesR) (page + 1)
    renderQuotes quotes prev next


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
         "self" .= r (QuoteR qid)] quote


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
    runDB $ update quoteId [
        QuoteVotesFor +=. 1,
        QuoteRating +=. 1,
        QuoteVotes +=. 1]
    redirect (QuoteR quoteId)


postQuoteDownR :: QuoteId -> Handler Html
postQuoteDownR quoteId = do
    runDB $ update quoteId [
        QuoteVotesAgainst +=. 1,
        QuoteRating -=. 1,
        QuoteVotes +=. 1]
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
    let q = Quote {
        quoteContent = (content value),
        quoteAdded = created,
        quoteVotesFor = 0,
        quoteVotesAgainst = 0,
        quoteRating = 0,
        quoteVotes = 0}
    qid <- runDB $ insert q
    quoteJson (qid, q)
