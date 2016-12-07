{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Cliente where

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


formCliente :: Form Cliente
formCliente = renderBootstrap2  $ Cliente
        <$> areq textField "Nome:"     Nothing
        <*> areq textField  "Telefone:"     Nothing
        <*> areq textField  "cpf:"     Nothing
        <*> areq textField  "endereco:"     Nothing
        <*> areq textField  "bairro:"     Nothing
        <*> areq emailField  "email:"     Nothing
        <*> areq passwordField "senha:"     Nothing

        
        
getClienteR :: Handler Html
getClienteR = do
            (widget,enctype) <- generateFormPost formCliente
            defaultLayout $ do
                $(whamletFile "Templates/addClientes.hamlet")
                addStylesheetRemote "http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.min.css"
                addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/startbootstrap-sb-admin-2/3.3.7+1/css/sb-admin-2.css"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/metisMenu/2.6.1/metisMenu.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcsdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/metisMenu/2.6.0/metisMenu.min.js"
                addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/startbootstrap-sb-admin-2/3.3.7+1/js/sb-admin-2.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]     
                                        


        
postClienteR:: Handler Html
postClienteR = do

        ((result,_),_)<- runFormPost formCliente
        case result of
            FormSuccess cliente -> do
                unicoEmail <- runDB $ getBy $ UniqueEmail (clienteEmail cliente)
                case unicoEmail of
                    Just _ -> redirect ClienteR
                    Nothing -> do
                    
                        pid<-runDB $ insert cliente
                        defaultLayout[whamlet|
                            <h1> Cliente #{fromSqlKey pid} cadastrado!
                            <form action=@{HomeR} method=get >
                                <input type="submit" value="Voltar">                    
                        |]
            _ -> redirect HomeR

getListClieR:: Handler Html
getListClieR = do
        prod <- runDB $ selectList [] [Asc ClienteNome]
        defaultLayout $ do
            $(whamletFile "Templates/listaClientes.hamlet")
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
            addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/metisMenu/2.6.1/metisMenu.min.css"
            addStylesheetRemote "https://cdn.datatables.net/1.10.12/css/dataTables.bootstrap.css"
            addStylesheetRemote "https://cdn.datatables.net/responsive/1.0.1/css/dataTables.responsive.css"
            addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/startbootstrap-sb-admin-2/3.3.7+1/css/sb-admin-2.css"
            addStylesheetRemote "http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.min.css"
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
            addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/metisMenu/2.6.0/metisMenu.min.js"
            addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/startbootstrap-sb-admin-2/3.3.7+1/js/sb-admin-2.min.js"
            toWidgetHead
                [hamlet|
                    <meta charset="UTF-8">  
                |]  
            

getListCliR :: ClienteId -> Handler Html
getListCliR cid = do
        cliente <- runDB $ get404 cid 
        defaultLayout $ do
            $(whamletFile "Templates/perfilClientes.hamlet")
            addStylesheetRemote "http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.min.css"
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
            addScriptRemote "https://maxcsdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
            addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
            toWidgetHead
                [hamlet|
                    <meta charset="UTF-8">  
                |]  
                
postListCliR :: ClienteId -> Handler Html
postListCliR cid = do
     runDB $ delete cid
     redirect ListClieR                