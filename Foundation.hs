module Foundation where

import Control.Lens ((^.))
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Settings.Development (development)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Core.Types (Logger)
import Yesod.EmbeddedStatic (EmbeddedStatic, embedStaticContent)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: EmbeddedStatic -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
      (fromMaybe (getApprootText guessApproot app req)
        (appRoot $ appSettings app))

    makeSessionBackend _ = sslOnlySessions $
      Just <$> envClientSessionBackend 120 "LAMBDACIRCUS_CLIENT_SESSION_KEY"

    yesodMiddleware = sslOnlyMiddleware 31536000 . simpleVaryMiddleware
      where simpleVaryMiddleware handler = do
              addHeader "Vary" "Accept"
              authorizationCheck
              handler

    isAuthorized (QuoteR _) True = do
        mu <- maybeAuth
        case mu of
            Nothing -> return AuthenticationRequired
            Just (Entity _ user)
                | user ^. userModerator -> return Authorized
                | otherwise             -> return $ Unauthorized "Must be a moderator"
    isAuthorized _ _ = return Authorized

    defaultLayout widget = do
        master <- getYesod
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            mapM_ (addStylesheet . StaticR) [
                css_bootstrap_min_css,
                css_bootstrap_responsive_min_css]
            mapM_ (addScript . StaticR) [
                js_jquery_min_js,
                js_bootstrap_min_js,
                js_prefixfree_min_js,
                js_xdate_js,
                js_underscore_min_js,
                js_backbone_min_js]
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = embedStaticContent appStatic StaticR mini
      where mini = if development then Right else minifym

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert User
                { _userEmail = credsIdent creds
                , _userModerator = False
                }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = []

    authHttpManager = getHttpManager

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
