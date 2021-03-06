module Handler.Quotes where

import Control.Lens hiding ((.=))
import Control.Monad (guard)
import Data.Aeson.Lens (key, _String, AsValue(), _Object, _Bool, _JSON)
import Data.Time (getCurrentTime, formatTime)
import Data.Time.Format (defaultTimeLocale)
import Database.Persist.Sql (fromSqlKey, toSqlKey, rawSql)
import Import
import Network.HTTP.Types (status204)
import Yesod.Auth (maybeAuth)


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
  maybeMatches <- lookupGetParam "matches"
  case maybeMatches of
   Nothing -> do
     Just entity <- runDB $ selectFirst [] [Desc QuoteAdded, LimitTo 1]
     redirect $ QuotesR (fromIntegral . fromSqlKey . entityKey $ entity)
   Just matches -> do
     quotes <- runDB $ rawSql q [toPersistValue matches]
     renderQuotes quotes Nothing Nothing
  where q = "SELECT ?? FROM quote, plainto_tsquery(?) query WHERE to_tsvector(translate(content, '<>', '  ')) @@ query ORDER BY ts_rank_cd(to_tsvector(translate(content, '<>', '  ')), query) DESC LIMIT 50"


getQuotesR :: Integer -> Handler TypedContent
getQuotesR fromId = do
    let limit = 10
    quotes <- runDB $ selectList
        [QuoteId <=. toSqlKey (fromInteger fromId)]
        [Desc QuoteId, LimitTo limit]
    r <- getUrlRender
    let prev = Just . r . QuotesR $ fromId + limit
        next = guard (fromId > 1) >> (Just . r . QuotesR) (max (fromId - limit) 1)
    renderQuotes quotes prev next


getTopQuotesR :: Integer -> Handler TypedContent
getTopQuotesR page = do
    let limit = 10
    quotes <- runDB $ selectList [] [
        Desc QuoteRating,
        Asc QuoteId,
        LimitTo limit,
        OffsetBy . fromInteger $ page * limit]
    r <- getUrlRender
    let prev = guard (page > 0) >> (Just . r . TopQuotesR) (page - 1)
        next = guard (length quotes > 0) >> (Just . r . TopQuotesR) (page + 1)
    renderQuotes quotes prev next


key' :: AsValue t => Text -> Traversal' t (Maybe Value)
key' i = _Object . at i


quoteJson :: (QuoteId, Quote) -> Handler Value
quoteJson (qid, quote) = do
    deletable <- andOf (_Just . to entityVal . userModerator) <$> maybeAuth
    r <- getUrlRender
    return $ toJSON quote
      & key' "id"        ?~ _JSON # qid
      & key' "voteUp"    ?~ _String # r (QuoteUpR qid)
      & key' "voteDown"  ?~ _String # r (QuoteDownR qid)
      & key' "deletable" ?~ _Bool # deletable
      & key' "self"      ?~ _String # r (QuoteR qid)


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


postNewQuoteR :: Handler Value
postNewQuoteR = do
    value <- requireJsonBody
    created <- liftIO getCurrentTime
    let q = Quote {
        _quoteContent = (value :: Value) ^?! key "content" . _String,
        _quoteAdded = created,
        _quoteVotesFor = 0,
        _quoteVotesAgainst = 0,
        _quoteRating = 0,
        _quoteVotes = 0}
    qid <- runDB $ insert q
    quoteJson (qid, q)
