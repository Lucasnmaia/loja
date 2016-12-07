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





formProduto :: Form Produto
formProduto = renderDivs $ Produto
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
                $(whamletFile "Templates/addProdutos.hamlet")
                addStylesheetRemote "http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.min.css"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcsdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Bree+Serif"
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
                    [whamlet|
                        <h1> Produto #{fromSqlKey pid} cadastrado!
                        <form action=@{HomeR} method=get >
                            <input type="submit" value="Voltar">
                    |]
            _ -> redirect HomeR




getListProdR:: Handler Html
getListProdR = do
     prod <- runDB $ selectList [] [Asc ProdutoNome]
     defaultLayout $ do
         [whamlet|
             <table>
                 <tr>
                     <td> id
                     <td> nome
                     <td> quantidade
                     <td> preco
                 $forall Entity pid produto <- prod
                     <tr>
                         <td> #{fromSqlKey pid}
                         <td> #{produtoNome    produto}
                         <td> #{produtoQtde    produto}
                         <td> #{produtoPreco   produto}
             <form action=@{HomeR} method=get >
                 <input type="submit" value="Voltar">

                         
         |]
