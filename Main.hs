{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
module Main where
import Foundation
import Application () -- for YesodDispatch instance
import Yesod
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql
import Application
import Yesod.Static
import Control.Applicative
import Data.Text



connStr :: ConnectionString
connStr = "dbname=dbtfmbj3jr7ibo host=ec2-54-235-95-188.compute-1.amazonaws.com user=tymgxetpbwhzgc password=iWvPTyCFCuW9MNBgmPQzzs8xnu port=5432"


       
main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       t@(Static settings) <- static "static"
       warp 8080 (Sitio t pool)       