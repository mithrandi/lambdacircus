module Main where

import           Blaze.Core (App(..), ApplyActionM, runApplyActionM, submitRequest)
import           Blaze.ReactJS.Run (runApp')
import           Control.Applicative ((<$>))
import           Control.Lens
import           Control.Monad (unless)
import           Control.Monad.State (put)
import           Data.Aeson.Lens (_Object, _String)
import           Data.Function (on)
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Text.Lens (_Text)
import           GHCJS.Foreign (fromJSString, jsNull)
import           Network.URI (parseURIReference, uriPath)

import           History (pushState)
import           Render (render)
import           Types
import           XHR (fetchJSON, clearBody, postEmptyJSON, postJSON, pathname)

main :: IO ()
main = do
  clearBody
  runApp' render (handle' <$> app)
    where handle' :: [CircusR] -> (CircusA -> IO ()) -> IO ()
          handle' reqs chan = mapM_ (handle chan) reqs

app :: App CircusS CircusA [CircusR]
app = App
      { appInitialState   = initialState
      , appInitialRequest = [InitRoute]
      , appApplyAction    = runApplyActionM . applyCircusA
      }

initialState :: CircusS
initialState = CSQuotes
               { _csQuotes = QuoteList Nothing Nothing []
               , _csQuoteStates = []
               }


parseRoute :: T.Text -> [String]
parseRoute = toListOf $ _Text
             . to parseURIReference
             . _Just
             . to uriPath
             . to (splitOn "/")
             . folded

applyCircusA :: CircusA -> ApplyActionM CircusS [CircusR] ()
applyCircusA action = case action of
  UpdateQuote q -> do
    let qidEq = (==) `on` view quoteId
    csQuotes . qlQuotes . traversed . filtered (qidEq q) .= q
  UpdateQuoteState qid qs ->
    csQuoteStates . ix qid .= qs
  ReplaceQuotes ql -> do
    put $
      CSQuotes
      ql
      (M.fromList (ql ^.. qlQuotes.traversed.quoteId.to (, QSNormal)))
  ChangeRoute url -> do
    submitRequest [PushState url]
    case parseRoute url of
     ("":"top":"pages":_)   -> submitRequest [FetchQuotes url]
     ("":"quotes":"from":_) -> submitRequest [FetchQuotes url]
     ["","quotes",_]        -> submitRequest [FetchQuote url]
     ["","quotes"]          -> submitRequest [FetchQuotes url]
     ["","newQuote"]        -> put (CSNewQuote "")
     _                      -> return ()
  VoteA qid url -> submitRequest [VoteR qid url]
  ChangeContent content ->
    csContent .= content
  CreateQuoteA -> do
    content <- use csContent
    unless (T.null content) $
      submitRequest [CreateQuoteR content]

handle :: (CircusA -> IO ()) -> CircusR -> IO ()
handle chan req =
  case req of
   FetchQuotes url -> do
     quoteList <- fetchJSON url
     chan (ReplaceQuotes quoteList)
   FetchQuote url -> do
     quote <- fetchJSON url
     chan (ReplaceQuotes (QuoteList Nothing Nothing [quote]))
   VoteR qid url -> do
     chan (UpdateQuoteState qid QSVoting)
     quote <- postEmptyJSON url
     chan (UpdateQuote quote)
     chan (UpdateQuoteState qid QSVoted)
   InitRoute -> do
     path <- fromJSString <$> pathname
     chan (ChangeRoute path)
   CreateQuoteR content -> do
     quote <- postJSON
             "/newQuote"
             (_Object._Wrapped # [("content", _String # content)] :: T.Text)
     pushState jsNull "" (Just (quote^.quoteSelf))
     chan (ReplaceQuotes (QuoteList Nothing Nothing [quote]))
   PushState url ->
     pushState jsNull "" (Just url)
