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
import Data.Time.Calendar


formVenda :: Form [ProdutoId]
formVenda = renderDivs $ areq (multiSelectField produtosLista) "Produtos disponiveis: " Nothing
            where
                produtosLista = do
                prod <- runDB $ selectList [] [Asc ProdutoQtde]
                optionsPairs $ Prelude.map (\v -> (mconcat [produtoNome $ entityVal v, " - ",  pack $ show $ produtoPreco $ entityVal v, " - ", pack $ show $ produtoQtde $ entityVal v], entityKey v)) prod
                                

                                
                                
                                

        
getVendaR:: Handler Html
getVendaR = do
    (widget,enctype) <- generateFormPost formVenda
    defaultLayout $ widgetForm VendaR enctype widget "Venda de Produtos"     
    
    
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



            