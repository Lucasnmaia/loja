{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Produto where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Monoid





formProduto :: Form Produto
formProduto = renderDivs $ Produto
        <$> areq textField                    "Nome:"     Nothing
        <*> areq intField                     "Quantidade:"     Nothing
        <*> areq doubleField                  "Preco:"     Nothing
    

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
