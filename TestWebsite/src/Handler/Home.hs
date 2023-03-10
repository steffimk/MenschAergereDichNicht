{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import
import           Text.Julius           (RawJS (..))
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..),
                                        renderBootstrap3)

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo        :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (_, _) <- generateFormPost sampleForm
    let _ = Nothing :: Maybe FileForm
        _ = "getHomeR" :: Text
    defaultLayout $ do
        let (lobbyFormId, lobbynameTextareaId, lobbyListId) = lobbyIds
            (joinButton, leaveButton, startGameButton) = lobbyButtons
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, _), _) <- runFormPost sampleForm
    let _ = "postHomeR" :: Text
        _ = case result of
            FormSuccess res -> Just res
            _               -> Nothing

    defaultLayout $ do
        let (lobbyFormId, lobbynameTextareaId, lobbyListId) = lobbyIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

lobbyIds :: (Text, Text, Text)
lobbyIds = ("js-lobbyForm", "js-createLobbynameTextarea", "js-lobbyList")

lobbyButtons :: (Text, Text, Text)
lobbyButtons = ("js-joinButton", "js-leaveButton", "js-startGameButton")
