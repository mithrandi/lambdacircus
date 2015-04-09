module Main where

import           Blaze.Core (App(..), ApplyActionM, runApplyActionM)
import           Blaze.ReactJS.Base (WindowState(..))
import           Blaze.ReactJS.Run (runApp')
import           Control.Applicative ((<$>), (<*>))
import           Control.Lens
import           Control.Monad (void)
import           Data.Aeson.Lens (key, _JSON, _Array)
import           Data.Foldable (foldMap)
import qualified Data.Map.Strict as M
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Types
import           XHR (fetchJSON)

main :: IO ()
main = runApp' render (handle' <$> app)
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
  ReplaceQuotes qs -> do
    csQuotes .= mapFromValues (view quoteId) qs

render :: CircusS -> WindowState CircusA
render state = WindowState (renderBody state) ""

renderBody :: CircusS -> H.Html CircusA
renderBody state = do
  H.div $ do
    H.div H.! A.class_ "quotes" $
      foldMap renderQuote (state^.csQuotes)
    H.a H.! A.class_ "prev" $
      H.i H.! A.class_ "icon-white icon-backward" $
        ""
    H.a H.! A.class_ "next" $
      H.i H.! A.class_ "icon-white icon-forward" $
        ""

_Html :: (H.ToMarkup a) => Getter a (H.Html b)
_Html = to H.toHtml

renderQuote :: Quote -> H.Html CircusA
renderQuote quote = do
  H.div H.! A.class_ "quote row-fluid" $ do
    H.div H.! A.class_ "span3 info" $ do
      H.a
        H.! A.rel "bookmark"
        H.! A.href "#"
        $ H.h1 (quote^.quoteId._Html)
      H.div H.! A.class_ "rating" $ do
        H.span H.! A.class_ "votes-for" $
          quote^.quoteVotesFor._Html
        void " / "
        H.span H.! A.class_ "votes-against" $
          quote^.quoteVotesAgainst._Html
      H.div H.! A.class_ "timestamp" $
        quote^.quoteAdded.to show._Html
    H.div H.! A.class_ "span9 content" $
      H.p (quote^.quoteContent._Html)

handle :: (CircusA -> IO ()) -> CircusR -> IO ()
handle chan req =
  case req of
   FetchQuotes url -> do
     quoteData <- fetchJSON url
     let quotes = quoteData ^?! qlQuotes
     chan (ReplaceQuotes quotes)
