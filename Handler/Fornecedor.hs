{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Fornecedor where

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



formFornecedor :: Form Fornecedor
formFornecedor = renderBootstrap2 $ Fornecedor
        <$> areq textField  "Nome:"     Nothing
        <*> areq textField  "Telefone:"     Nothing
        <*> areq textField  "Cnpj:"     Nothing



getFornecedorR:: Handler Html
getFornecedorR = do
            (widget,enctype) <- generateFormPost formFornecedor
            defaultLayout $ do
                sessao <- lookupSession "_ID"
                $(whamletFile "Templates/addFornecedores.hamlet")
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
                              


postFornecedorR:: Handler Html
postFornecedorR = do

        ((result,_),_)<- runFormPost formFornecedor
        case result of
            FormSuccess fornecedor -> do
                fid<-runDB $ insert fornecedor
                defaultLayout[whamlet|
                    <h1> Fornecedor #{fromSqlKey fid} cadastrado!
                    <form action=@{HomeR} method=get >
                        <input type="submit" value="Voltar">
                |]
            _ -> redirect HomeR




getListFornR:: Handler Html
getListFornR = do
     forn <- runDB $ selectList [] [Asc FornecedorNome]
     defaultLayout $ do
            sessao <- lookupSession "_ID"
            $(whamletFile "Templates/listaFornecedores.hamlet")
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