{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Produto where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Monoid


formProduto :: Form Produto
formProduto= renderDivs $ Produto
        <$> areq textField   "Nome:       "     Nothing
        <*> areq intField    "Quantidade: "     Nothing
        <*> areq doubleField "Preço:      "     Nothing
        

getProdutoR:: Handler Html
getProdutoR = do
    (widget,enctype) <- generateFormPost formProduto
    defaultLayout $ do
        [whamlet|
            <form action=@{ProdutoR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]


postProdutoR:: Handler Html
postProdutoR = do

        ((result,_),_)<- runFormPost formProduto
        case result of
            FormSuccess Produto -> do
                pid<-runDB $ insert produto
                defaultLayout[whamlet|
                    <h1> Produto #{fromSqlKey pid} cadastrado!
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
                         <td> #{ProdutoNome  Produto}
                         <td> #{ProdutoIdade Produto}
                         <td> #{ProdutoPassaporte   Produto}
                         
         |]
