module Main where

import           Blaze.Core (App(..), ApplyActionM, runApplyActionM)
import           Blaze.ReactJS.Run (runApp')
import           Control.Applicative ((<$>), (<*>))
import           Control.Lens
import           Data.Aeson.Lens (key, _JSON, _Array)
import qualified Data.Map.Strict as M

import           Render (render)
import           Types
import           XHR (fetchJSON, clearBody)

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
               { _csQuotes = []
               }

mapFromValues :: Ord k => (v -> k) -> [v] -> M.Map k v
mapFromValues f = foldr (flip M.insert <*> f) M.empty

applyCircusA :: CircusA -> ApplyActionM CircusS [CircusR] ()
applyCircusA action = case action of
  UpdateQuote q -> do
    csQuotes . ix (q^.quoteId) .= q
  ReplaceQuotes ql -> do
    csQuotes .= mapFromValues (view quoteId) (ql^.qlQuotes)

handle :: (CircusA -> IO ()) -> CircusR -> IO ()
handle chan req =
  case req of
   FetchQuotes url -> do
     quoteList <- fetchJSON url
     chan (ReplaceQuotes quoteList)
