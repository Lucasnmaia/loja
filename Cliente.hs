{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Cliente where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Monoid


formCliente :: Form Cliente
formCliente = renderDivs $ Cliente
        <$> areq textField "Nome:"     Nothing
        <*> areq textField  "Telefone:"     Nothing
        <*> areq textField  "cpf:"     Nothing
        <*> areq emailField  "email:"     Nothing
        <*> areq textField  "endereco:"     Nothing
        <*> areq textField  "bairro:"     Nothing

getClienteR:: Handler Html
getClienteR = do
    (widget,enctype) <- generateFormPost formCliente
    defaultLayout $ do
        [whamlet|
            <form action=@{ClienteR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postClienteR:: Handler Html
postClienteR = do

        ((result,_),_)<- runFormPost formCliente
        case result of
            FormSuccess cliente -> do
                pid<-runDB $ insert cliente
                defaultLayout[whamlet|
                    <h1> Cliente #{fromSqlKey pid} cadastrado!
                |]
            _ -> redirect HomeR

getListClieR:: Handler Html
getListClieR = do
     prod <- runDB $ selectList [] [Asc ClienteNome]
     defaultLayout $ do
         [whamlet|
             <table>
                 <tr>
                     <td> id
                     <td> nome
                     <td> telefone
                     <td> cpf
                     <td> email
                     <td> endereco
                     <td> bairro
                 $forall Entity pid cliente <- prod
                     <tr>
                         <td> #{fromSqlKey pid}
                         <td> #{clienteNome    cliente}
                         <td> #{clienteTelefone    cliente}
                         <td> #{clienteCpf   cliente}
                         <td> #{clienteEmail   cliente}
                         <td> #{clienteEndereco   cliente}
                         <td> #{clienteBairro   cliente}
                         
         |]