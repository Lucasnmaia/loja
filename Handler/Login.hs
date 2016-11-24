{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Login where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text



formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,)
        <$> areq emailField  "email "     Nothing
        <*> areq passwordField "password" Nothing 


getLoginR :: Handler Html
getLoginR = do
    (widget,enctype) <- generateFormPost formLogin
    defaultLayout $ do
        [whamlet|
            <form action=@{LoginR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Logar">
        |]


postLoginR :: Handler Html
postLoginR = do
        ((result,_),_)<- runFormPost formLogin
        case result of
            FormSuccess ("admin","admin") -> do
                setSession "_ADMIN" "admin"
                redirect AdminR
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
getAdminR = defaultLayout [whamlet| <h1> Bem-vindo ADMIN! |]


postLogoutR :: Handler ()
postLogoutR = do
    deleteSession "_ID"
    redirect HomeR