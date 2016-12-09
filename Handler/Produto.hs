{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Produto where

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




formProduto :: Form Produto
formProduto = renderBootstrap2 $ Produto
        <$> areq textField "Nome:"     Nothing
        <*> areq intField   "Quantidade:"     Nothing
        <*> areq doubleField  "Preco:"     Nothing
        <*> areq (selectField dptos) "Fornecedor" Nothing 

dptos = do
       entidades <- runDB $ selectList [] [Asc FornecedorNome] 
       optionsPairs $ fmap (\ent -> (fornecedorNome $ entityVal ent, entityKey ent)) entidades    

getProdutoR:: Handler Html
getProdutoR = do
            (widget,enctype) <- generateFormPost formProduto
            defaultLayout $ do
                sessao <- lookupSession "_ID"
                $(whamletFile "Templates/addProdutos.hamlet")
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


postProdutoR:: Handler Html
postProdutoR = do

        ((result,_),_)<- runFormPost formProduto
        case result of
            FormSuccess produto -> do
                pid<-runDB $ insert produto
                defaultLayout $ do
                    sessao <- lookupSession "_ID"
                    $(whamletFile "Templates/sucessoProduto.hamlet")
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
            _ -> redirect HomeR

        




getListProdR:: Handler Html
getListProdR = do
     prod <- runDB $ selectList [] [Asc ProdutoNome]
     defaultLayout $ do
            sessao <- lookupSession "_ID"
            $(whamletFile "Templates/listaProdutos.hamlet")
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
            
