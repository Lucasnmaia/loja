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

import Pedido
import Produto
import Cliente
import Fornecedor
import PedidoProduto
------------------
mkYesodDispatch "App" resourcesApp


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    
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
    |]