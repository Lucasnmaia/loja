{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Cliente where

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
        <*> areq textField  "endereco:"     Nothing
        <*> areq textField  "bairro:"     Nothing
        <*> areq emailField  "email:"     Nothing
        <*> areq passwordField "senha:"     Nothing

getClienteR:: Handler Html
getClienteR = do
    (widget,enctype) <- generateFormPost formCliente
    defaultLayout $ widgetForm ClienteR enctype widget "Cadastro de Clientes"
    


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
            <form action=@{HomeR} method=get >
                <input type="submit" value="Voltar">                         
                         
         |]