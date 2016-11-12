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
            display: inline;
        }
    |]
    [whamlet|
        <h1>Vendas de Sapatilhas
            <ul>
                <li> <a href=@{PassageiroR}>Cadastro de cliente
                <li> <a href=@{VooR}>Cadastro de produtos
                <li> <a href=@{ListPassR}> Listagem de cliente
                <li> <a href=@{ListVooR}>Listagem de produtos
    |]