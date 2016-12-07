{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}

module Application where

import Foundation
import Yesod
import Handler.Produto
import Handler.Cliente
import Handler.Fornecedor
import Handler.Venda
import Handler.Login
------------------
mkYesodDispatch "Sitio" resourcesSitio


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    sessao <- lookupSession "_ID"
    $(whamletFile "Templates/index.hamlet")
    addStylesheetRemote "http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.min.css"
    addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/startbootstrap-sb-admin-2/3.3.7+1/css/sb-admin-2.css"
    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/metisMenu/2.6.1/metisMenu.min.css"
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/metisMenu/2.6.0/metisMenu.min.js"
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/startbootstrap-sb-admin-2/3.3.7+1/js/sb-admin-2.min.js"
    [whamlet|
         <meta charset="UTF-8">  
    |]