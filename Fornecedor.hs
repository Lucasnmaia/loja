{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Fornecedor where

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
        [whamlet|
            <form action=@{FornecedorR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]


postFornecedorR:: Handler Html
postFornecedorR = do

        ((result,_),_)<- runFormPost formFornecedor
        case result of
            FormSuccess fornecedor -> do
                fid<-runDB $ insert fornecedor
                defaultLayout[whamlet|
                    <h1> Fornecedor #{fromSqlKey fid} cadastrado!
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

                         
         |]
 