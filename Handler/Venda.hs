{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Venda where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Monoid
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Time
import Text.Lucius
import Text.Julius
import Text.Blaze.Html.Renderer.String (renderHtml)
import Yesod.Form.Bootstrap3



formVenda :: Form [ProdutoId]
formVenda = renderBootstrap2 $ areq (multiSelectField produtosLista) "Produtos disponiveis: " Nothing
            where
                produtosLista = do
                prod <- runDB $ selectList [] [Asc ProdutoQtde]
                optionsPairs $ Prelude.map (\v -> (mconcat [produtoNome $ entityVal v, " - ",  pack $ show $ produtoPreco $ entityVal v, " - ", pack $ show $ produtoQtde $ entityVal v], entityKey v)) prod
                                

                                
        
getVendaR:: Handler Html
getVendaR = do
            (widget,enctype) <- generateFormPost formVenda
            defaultLayout $ do
                sessao <- lookupSession "_ID"
                $(whamletFile "Templates/addVendas.hamlet")
                addStylesheetRemote "http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.min.css"
                addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/startbootstrap-sb-admin-2/3.3.7+1/css/sb-admin-2.css"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/metisMenu/2.6.1/metisMenu.min.css"
                addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                addScriptRemote "https://maxcsdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/metisMenu/2.6.0/metisMenu.min.js"
                addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/startbootstrap-sb-admin-2/3.3.7+1/js/sb-admin-2.min.js"
                toWidgetHead
                    [hamlet|
                        <meta charset="UTF-8">  
                    |]   
    
postVendaR :: Handler Html
postVendaR = do
        ((result,_),_)<- runFormPost formVenda
        case result of
            FormSuccess vendas  -> do
                userId <- lookupSession "_ID"
                case userId of
                    Nothing -> redirect HomeR
                    Just userStr -> do
                        cid <- (return $ read $ unpack userStr) :: Handler ClienteId
                        --pid <- (return $ read $ unpack userStr) :: Handler ProdutoId
                        sequence $ fmap (\pid -> runDB $ insert $ Venda cid pid) vendas
                        defaultLayout [whamlet| <h1> Vendas cadastradas com sucesso! |]

            _ -> redirect HomeR    



            