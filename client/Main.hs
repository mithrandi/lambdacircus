module Main where

import           Blaze.Core (App(..), ApplyActionM, runApplyActionM, submitRequest)
import           Blaze.ReactJS.Run (runApp')
import           Control.Applicative ((<$>), (<*>))
import           Control.Lens
import           Data.Aeson.Lens (key, _JSON, _Array)
import           Data.Function (on)
import qualified Data.Sequence as Seq

import           Render (render)
import           Types
import           XHR (fetchJSON, clearBody, postEmptyJSON)

main :: IO ()
main = do
  clearBody
  runApp' render (handle' <$> app)
    where handle' :: [CircusR] -> (CircusA -> IO ()) -> IO ()
          handle' reqs chan = mapM_ (handle chan) reqs

app :: App CircusS CircusA [CircusR]
app = App
      { appInitialState   = initialState
      , appInitialRequest = [FetchQuotes "/quotes"]
      , appApplyAction    = runApplyActionM . applyCircusA
      }

initialState :: CircusS
initialState = CSQuotes
               { _csQuotes = QuoteList Nothing Nothing []
               }

applyCircusA :: CircusA -> ApplyActionM CircusS [CircusR] ()
applyCircusA action = case action of
  UpdateQuote q -> do
    let qid = q^.quoteId
    csQuotes . qlQuotes . traversed . filtered ((== qid) . view quoteId) .= q
  ReplaceQuotes ql -> do
    csQuotes .= ql
  VoteA url -> submitRequest [(VoteR url)]

handle :: (CircusA -> IO ()) -> CircusR -> IO ()
handle chan req =
  case req of
   FetchQuotes url -> do
     quoteList <- fetchJSON url
     chan (ReplaceQuotes quoteList)
   FetchQuote url -> do
     quote <- fetchJSON url
     chan (ReplaceQuotes (QuoteList Nothing Nothing [quote]))
   VoteR url -> do
     quote <- postEmptyJSON url
     chan (UpdateQuote quote)
