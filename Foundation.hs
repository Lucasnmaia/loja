{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}
module Foundation where

import Yesod
import Data.Text
import Data.Time.Calendar
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool)

data App = App {connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Produto
    nome  Text
    qtde  Int
    preco Double
    forne  FornecedorId
    
Fornecedor
    nome        Text
    telefone    Text
    cnpj        Text
    
Cliente
    nome        Text
    telefone    Text
    cpf         Text
    endereco    Text
    bairro      Text
    email       Text
    senha       Text
    
Pedido
    nomeCli     Text
    cpfCli      Text
    nomePro     Text
    qtdePro     Int
    
PedidoProduto
    pedidoid    PedidoId
    prodid      ProdutoId
    dataCompra  Day
    dataEnt     Day
    total       Double
    
    

|]

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
    authRoute _ = Just LoginR
    
    isAuthorized ClienteR _ = return Authorized
    isAuthorized ListProdR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized AdminR _ = ehAdmin
    isAuthorized _ _ = estaAutenticado

ehAdmin :: Handler AuthResult
ehAdmin = do
   msu <- lookupSession "_ADMIN@admin"
   case msu of
       Just _ -> return Authorized
       Nothing -> return $ Unauthorized "Nao eh admin"

estaAutenticado :: Handler AuthResult
estaAutenticado = do
   msu <- lookupSession "_ID"
   case msu of
       Just _ -> return Authorized
       Nothing -> return AuthenticationRequired

instance YesodPersist App where
   type YesodPersistBackend App = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage