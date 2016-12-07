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


formFornecedor :: Form Fornecedor
formFornecedor = renderDivs $ Fornecedor
        <$> areq textField  "Nome:"     Nothing
        <*> areq textField  "Telefone:"     Nothing
        <*> areq textField  "Cnpj:"     Nothing



getFornecedorR:: Handler Html
getFornecedorR = do
            (widget,enctype) <- generateFormPost formFornecedor
            defaultLayout $ do
                $(whamletFile "Templates/addFornecedores.hamlet")
                addStylesheetRemote "http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.min.css"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcsdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
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
         [whamlet|
             <table>
                 <tr>
                     <td> id
                     <td> nome
                     <td> telefone
                     <td> cnpj
                 $forall Entity fid fornecedor <- forn
                     <tr>
                         <td> #{fromSqlKey fid}
                         <td> #{fornecedorNome    fornecedor}
                         <td> #{fornecedorTelefone    fornecedor}
                         <td> #{fornecedorCnpj    fornecedor}
            <form action=@{HomeR} method=get >
                <input type="submit" value="Voltar">

                         
         |]
 