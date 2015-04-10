module Render where

import Blaze.ReactJS.Base (WindowState(..))
import Control.Lens
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.String (fromString)
import Control.Applicative ((<$>))
import Prelude (($), (.), show, String)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (span, form)
import Text.Blaze.Internal (Attributable)
import Types

_html :: (ToMarkup a) => Getter a (Html b)
_html = to toHtml

_value :: (ToValue a) => Getter a AttributeValue
_value = to toValue

-- Version of (!?) using Maybe instead of a Bool flag
(?) :: Attributable h ev => h -> Maybe (Attribute ev) -> h
(?) h Nothing  = h
(?) h (Just a) = h ! a
{-# INLINE (?) #-}

render :: CircusS -> WindowState CircusA
render state = WindowState (renderBody state) ""

renderHeader :: Html CircusA
renderHeader = do
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
              li $
                a ! href "/" $ do
                  i ! class_ "icon-white icon-home" $ ""
                  " Overview"
              li $
                a ! href "/top" $ do
                  i ! class_ "icon-white icon-comment" $ ""
                  " Top"
              li $
                a ! href "/quotes" $ do
                  i ! class_ "icon-white icon-comment" $ ""
                  " Browse"
              li $
                a ! href "/newQuote" $ do
                  i ! class_ "icon-white icon-comment" $ ""
                  " New Quote"
          div ! class_ "search" $
            div ! class_ "icon" $
              form ! id "search" $
                input ! type_ "text"
                      ! name "matches"
                      ! placeholder "Search"

renderFooter :: Html a
renderFooter = do
  div ! class_ "footer" $
    p $ do
      "“Magnifying Glass” symbol from "
      a ! href "http://thenounproject.com/" $ "The Noun Project"
      " collection."

renderPage :: CircusS -> Html CircusA
renderPage state
  | has _CSQuotes state = do
      div ! class_ "quotes" $ do
        foldMapOf (csQuotes.qlQuotes.folded) renderQuote state
        a ! class_ "prev"
          ? (href <$> state^?csQuotes.qlPrev._Just._value) $
          i ! class_ "icon-white icon-backward" $
            ""
        a ! class_ "next"
          ? (href <$> state^?csQuotes.qlNext._Just._value) $
          i ! class_ "icon-white icon-forward" $
            ""

renderBody :: CircusS -> Html CircusA
renderBody state = do
  renderHeader
  div ! class_ "page container" $ do
    renderPage state
    renderFooter

renderQuote :: Quote -> Html CircusA
renderQuote quote = do
  div ! class_ "quote row-fluid" $ do
    div ! class_ "span3 info" $ do
      a ! rel "bookmark"
        ! href "#" $
        h1 (quote^.quoteId._html)
      div ! class_ "rating" $ do
        span ! class_ "votes-for" $
          quote^.quoteVotesFor._html
        " / "
        span ! class_ "votes-against" $
          quote^.quoteVotesAgainst._html
      div ! class_ "timestamp" $
        quote^.quoteAdded.to show._html
    div ! class_ "span9 content" $
      p (quote^.quoteContent._html)
