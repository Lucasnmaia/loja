{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}

module Application where

import Foundation
import Yesod

-- AQUI MORAM OS HANDLERS
-- import Add
-- PARA CADA NOVO GRUPO DE HANDLERS, CRIAR UM AQUIVO
-- DE HANDLER NOVO E IMPORTAR AQUI

import Handler.Pedido
import Handler.Produto
import Handler.Cliente
import Handler.Fornecedor
import Handler.PedidoProduto
import Handler.Login
------------------
mkYesodDispatch "App" resourcesApp


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    sessao <- lookupSession "_ID"
    toWidget[lucius|
        ul li {
            display: inline block;
        }
    |]
    [whamlet|
        <h1>Vendas de Sapatilhas
            <ul>
                <li> <a href=@{ClienteR}>Cadastro de cliente
                <li> <a href=@{ProdutoR}>Cadastro de produtos
                <li> <a href=@{FornecedorR}>Cadastro de fornecedores
                <li> <a href=@{ListClieR}> Listagem de cliente
                <li> <a href=@{ListProdR}>Listagem de produtos
                <li> <a href=@{ListFornR}>Listagem de fornecedores
                <li> <a href=@{AdminR}>Administrador
                $maybe sess <- sessao
                    <form method=post action=@{LogoutR}>
                        <input type="submit" value="Logout">
    |]