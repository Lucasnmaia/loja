{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Pedido where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Monoid
