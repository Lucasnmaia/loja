{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}
module Foundation where
import Yesod
import Yesod.Static
import Data.Text
import Data.Time.Calendar
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool)

--data App = App {connPool :: ConnectionPool }
data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }
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
    UniqueEmail email
    
    
--Venda
--    clienteid ClienteId
--    produtoid  ProdutoId
  


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

mkYesodData "Sitio" $(parseRoutesFile "routes")

--mkMessage "Sitio" "messages" "pt-br"

--mkYesodData "App" $(parseRoutesFile "routes")


instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool


instance Yesod Sitio where
    authRoute _ = Just LoginR
    
    isAuthorized ClienteR _ = return Authorized
    isAuthorized ListProdR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized AdminR _ = ehAdmin
    isAuthorized _ _ = estaAutenticado

ehAdmin :: Handler AuthResult
ehAdmin = do
   msu <- lookupSession "_ADMIN"
   case msu of
       Just _ -> return Authorized
       Nothing -> return $ Unauthorized "Nao eh admin"

estaAutenticado :: Handler AuthResult
estaAutenticado = do
   msu <- lookupSession "_ID"
   case msu of
       Just _ -> return Authorized
       Nothing -> return AuthenticationRequired



type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage
    
widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y =  [whamlet| 
<h1>
    #{y}
    <form method=post action=@{x} enctype=#{enctype}>
        ^{widget}
        <input type="submit" value="Cadastrar">
        |]