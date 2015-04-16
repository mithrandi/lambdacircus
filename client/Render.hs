module Render where

import           Blaze.ReactJS.Base (WindowState(..))
import           Control.Lens hiding (pre)
import           Control.Monad (when, void)
import           Data.Monoid ((<>), mempty)
import           Data.Text (Text)
import           Prelude (($), (.), show, Maybe(..), Bool, otherwise, error)
import qualified Text.Blaze.Event as E
import           Text.Blaze.Html5 hiding (title)
import           Text.Blaze.Html5.Attributes hiding (span, form, disabled)
import           Text.Blaze.Internal (Attributable, boolAttribute)
import           Types

_html :: (ToMarkup a) => Getter a (Html b)
_html = to toHtml

_value :: (ToValue a) => Getter a AttributeValue
_value = to toValue

disabled :: Bool -> Attribute ev
disabled = boolAttribute "disabled"
{-# INLINE disabled #-}

-- Version of (!?) using Maybe instead of a Bool flag
(?) :: Attributable h ev => h -> Maybe (Attribute ev) -> h
(?) h Nothing  = h
(?) h (Just attr) = h ! attr
{-# INLINE (?) #-}

render :: CircusS -> WindowState CircusA
render state = WindowState (renderBody state) ""

routedLink :: Text -> Html CircusA -> Html CircusA
routedLink url = routedLink' (Just url)

routedLink' :: Maybe Text -> Html CircusA -> Html CircusA
-- React doesn't know how to remove attributes (see
-- <https://github.com/facebook/react/issues/1448>) so instead, we make the
-- entire <a> element conditional.
routedLink' (Just url) = a ! href (toValue url)
                           ! E.onClick' (ChangeRoute url)
routedLink' Nothing = mempty

quotesLink :: Text -> AttributeValue -> Html CircusA -> Html CircusA
quotesLink url = quotesLink' (Just url)

quotesLink' :: Maybe Text -> AttributeValue -> Html CircusA -> Html CircusA
quotesLink' url iconClass linkTitle =
  routedLink' url $ do
    i ! class_ ("icon-white " <> iconClass) $ ""
    " " <> linkTitle

renderHeader :: Text -> Html CircusA
renderHeader searchContent = 
  div ! class_ "header" $
    div ! class_ "navbar navbar-fixed-top" $
      div ! class_ "navbar-inner" $
        div ! class_ "container" $ do
          a ! class_ "btn btn-navbar"
            ! dataAttribute "toggle" "collapse"
            ! dataAttribute "target" ".nav-collapse" $ do
              span ! class_ "icon-white icon-bar" $ ""
              span ! class_ "icon-white icon-bar" $ ""
              span ! class_ "icon-white icon-bar" $ ""
          a ! class_ "brand"
            ! href "/" $
            "Slipgate Quote Database"
          div ! class_ "nav-collapse" $
            ul ! class_ "nav pull-right" $ do
              li $ quotesLink "/random"      "icon-comment" "Random"
              li $ quotesLink "/top/pages/0" "icon-comment" "Top"
              li $ quotesLink "/quotes"      "icon-comment" "Browse"
              li $ routedLink "/newQuote" $ do
                  i ! class_ "icon-white icon-comment" $ ""
                  " New Quote"
          div ! class_ "search" $
            div ! class_ "icon" $
              form ! E.onSubmit SearchA $
                input ! type_ "text"
                      ! name "matches"
                      ! placeholder "Search"
                      ! value (toValue searchContent)
                      ! E.onValueChange (ChangeSearchContent)

renderFooter :: Html a
renderFooter =
  div ! class_ "footer" $
    p $ do
      void "“Magnifying Glass” symbol from "
      a ! href "http://thenounproject.com/" $ "The Noun Project"
      " collection."

renderPage :: CircusPageS -> Html CircusA
renderPage state
  | has _CSQuotes state =
      div ! class_ "quotes" $ do
        foldMapOf quotesWithState renderQuote state
        quotesLink' 
          (state ^? csQuotes.qlPrev._Just)
          "icon-backward"
          ""
        quotesLink'
          (state ^? csQuotes.qlNext._Just)
          "icon-forward"
          ""
  | has _CSNewQuote state =
      div $ do
        h1 "Add quote"
        p "Paste your quote in the area below."
        h2 "Quote format"
        ul $ do
          li "Edit quotes for brevity — remove or truncate irrelevant text."
          li "Remove timestamps, unless they are relevant to the quote."
          li $ "Consider quoting usernames with " <> code "<" <> " and " <> code ">" <> " and avoid characters that are legal in IRC nicknames, such as " <> code "[" <> " and " <> code "]" <> "."
        p "An example of a well-formatted quote:"
        div ! class_ "quote-content" $
          pre "<Bob> Why did the chicken cross the road?\n<James> To get to the other side."
        form ! action ""
             ! E.onSubmit (PageA CreateQuoteA) $ do
          textarea ! value (state^?!csContent._value)
                   ! E.onValueChange (PageA . ChangeContent)
          input ! type_ "submit"
                ! value "Add quote"
  | otherwise = error "Impossible"
  where withQuoteState quote = state^?csQuoteStates.ix (quote^.quoteId).to (quote ,)
        quotesWithState = csQuotes.qlQuotes.folded.to withQuoteState._Just

renderBody :: CircusS -> Html CircusA
renderBody state = do
  renderHeader (state^.csSearchContent)
  div ! class_ "page container" $ do
    renderPage (state^.csPage)
    renderFooter

renderQuote :: (Quote, QuoteState) -> Html CircusA
renderQuote (quote, qs) =
  div ! class_ "quote row-fluid" $ do
    div ! class_ "span3 info" $ do
      routedLink (quote^.quoteSelf)
        ! rel "bookmark" $
        h1 (quote^.quoteId._html)
      div ! class_ "rating" $ do
        span ! class_ "votes-for" $
          quote^.quoteVotesFor._html
        void " / "
        span ! class_ "votes-against" $
          quote^.quoteVotesAgainst._html
      div ! class_ "timestamp" $
        quote^.quoteAdded.to show._html
      div ! class_ "controls" $
        case qs of
          QSVoted -> i ! class_ "voted icon-white icon-ok" $ ""
          _       -> do
            button ! class_ "btn btn-mini btn-success"
                   ! title "Vote for"
                   ! disabled (has _QSVoting qs)
                   ! E.onClick' (PageA $ VoteA (quote^.quoteId) (quote^.quoteVoteUp)) $
              i ! class_ "icon-white icon-arrow-up" $ ""
            button ! class_ "btn btn-mini btn-danger"
                   ! title "Vote against"
                   ! disabled (has _QSVoting qs)
                   ! E.onClick' (PageA $ VoteA (quote^.quoteId) (quote^.quoteVoteDown)) $
              i ! class_ "icon-white icon-arrow-down" $ ""
            when (quote^.quoteDeletable) $
              button ! class_ "btn btn-mini"
                     ! title "Remove" $
                i ! class_ "icon-white icon-remove" $ ""
    div ! class_ "span9 content" $
      p (quote^.quoteContent._html)
