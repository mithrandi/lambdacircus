module Render where

import Blaze.ReactJS.Base (WindowState(..))
import Control.Lens
import Control.Monad (void)
import Data.Foldable (foldMap)
import Data.Monoid ((<>))
import Data.String (fromString)
import Prelude (($), (.), show, String)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (span, data_, form)
import Text.Blaze.Internal (AttributeValue, attribute)
import Types

dataToggle :: AttributeValue -> Attribute ey
dataToggle = attribute "data-toggle" " data-toggle=\""
{-# INLINE dataToggle #-}

dataTarget :: AttributeValue -> Attribute ey
dataTarget = attribute "data-target" " data-target=\""
{-# INLINE dataTarget #-}

render :: CircusS -> WindowState CircusA
render state = WindowState (renderBody state) ""

renderHeader :: Html CircusA
renderHeader = do
  div ! class_ "header" $
    div ! class_ "navbar navbar-fixed-top" $
      div ! class_ "navbar-inner" $
        div ! class_ "container" $ do
          a ! class_ "btn btn-navbar"
            ! dataToggle "collapse"
            ! dataTarget ".nav-collapse" $ do
              span ! class_ "icon-white icon-bar" $ ""
              span ! class_ "icon-white icon-bar" $ ""
              span ! class_ "icon-white icon-bar" $ ""
          a ! class_ "brand"
            ! href "/" $
            "Slipgate Quote Database"
          div ! class_ "nav-collapse" $
            ul ! class_ "nav pull-right" $ do
              li $
                a ! href "" $
                  i ! class_ "icon-white icon-home" $
                    " Overview"
              li $
                a ! href "" $
                  i ! class_ "icon-white icon-comment" $
                    " Top"
              li $
                a ! href "" $
                  i ! class_ "icon-white icon-comment" $
                    " Browse"
              li $
                a ! href "" $
                  i ! class_ "icon-white icon-comment" $
                    " New Quote"
          div ! class_ "search" $
            div ! class_ "icon" $
              form ! id "search" $
                input ! type_ "text"
                      ! name "matches"
                      ! placeholder "Search"
                

renderBody :: CircusS -> Html CircusA
renderBody state = do
  renderHeader
  div ! class_ "page container" $ do
    div ! class_ "quotes" $
      foldMap renderQuote (state^.csQuotes)
    a ! class_ "prev" $
      i ! class_ "icon-white icon-backward" $
        ""
    a ! class_ "next" $
      i ! class_ "icon-white icon-forward" $
        ""
    div ! class_ "footer" $
      p $ do
        void "“Magnifying Glass” symbol from "
        a ! href "http://thenounproject.com/" $ "The Noun Project"
        void " collection."

_Html :: (ToMarkup a) => Getter a (Html b)
_Html = to toHtml

renderQuote :: Quote -> Html CircusA
renderQuote quote = do
  div ! class_ "quote row-fluid" $ do
    div ! class_ "span3 info" $ do
      a ! rel "bookmark"
        ! href "#"
        $ h1 (quote^.quoteId._Html)
      div ! class_ "rating" $ do
        span ! class_ "votes-for" $
          quote^.quoteVotesFor._Html
        void " / "
        span ! class_ "votes-against" $
          quote^.quoteVotesAgainst._Html
      div ! class_ "timestamp" $
        quote^.quoteAdded.to show._Html
    div ! class_ "span9 content" $
      p (quote^.quoteContent._Html)
