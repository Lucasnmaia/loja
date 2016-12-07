{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Login where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Monoid
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Time
import Text.Lucius
import Text.Julius
import Text.Blaze.Html.Renderer.String (renderHtml)
import Yesod.Form.Bootstrap3



formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,)
        <$> areq emailField  "email "     Nothing
        <*> areq passwordField "password" Nothing 


getLoginR :: Handler Html
getLoginR = do
            (widget,enctype) <- generateFormPost formLogin
            defaultLayout $ do
                $(whamletFile "Templates/login.hamlet")
                addStylesheetRemote "http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.min.css"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcsdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]     
                    
                
                

postLoginR :: Handler Html
postLoginR = do
        ((result,_),_)<- runFormPost formLogin
        case result of
            FormSuccess ("admin@admin.com","admin") -> do
                setSession "_ADMIN" "admin"
                setSession "_NAME" "chefe"
                setSession "_ID" "eu"
                redirect HomeR
            FormSuccess (login,senha) -> do
                usuario <- runDB $ selectFirst [ClienteEmail ==. login, 
                                                ClienteSenha ==. senha] []
                case usuario of
                    Just clie -> do
                        setSession "_ID" (pack $ show $ entityKey clie) 
                        redirect HomeR
                    Nothing ->  do
                        redirect LoginR
            _ -> redirect HomeR



getAdminR :: Handler Html
getAdminR = do
    valor <- lookupSession "_NAME"
    nome <- lookupSession "_ADMIN"
    defaultLayout
        [whamlet|
            <h1> Bem-vindo ADMIN!
            #{show $ nome} 
            #{show $ valor}
        |]



postLogoutR :: Handler ()
postLogoutR = do
    deleteSession "_ID"
    deleteSession "_ADMIN"
    deleteSession "_NAME"
    redirect HomeR