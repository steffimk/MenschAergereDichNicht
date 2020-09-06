{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Game where

import Import
import Text.Julius (RawJS (..))
import Controller.Actions
import Model.Board

getGameR :: Handler Html
getGameR = do
    let boardState = initNewBoardState :: BoardState
        turnString = show $ turn boardState
    defaultLayout $ do
        setTitle "Mensch ärgere Dich nicht"
        $(widgetFile "gamepage")

    -- toWidget [lucius| h1 { color: green; } |]
    -- addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
    -- toWidget
    --     [julius|
    --         $(function() {
    --             $("h1").click(function(){
    --                 alert("You clicked on the heading!");
    --             });
    --         });
    --         $(function() {
    --             $("h2").click(function(){
    --                 alert("You clicked on the heading2!");
    --             });
    --         });
    --     |]
    -- toWidgetHead
    --     [hamlet|
    --         <meta name=keywords content="some sample keywords">
    --     |]
    -- [whamlet|<h1>Mensch ärgere Dich nicht</tt> <h2>Am Zug: Gelb|]
    -- toWidgetBody
    --     [julius|
    --         alert("This is included in the body itself");
    --     |]

-- postGameR :: Handler BoardState
-- postGameR = 
