{-# LANGUAGE OverloadedStrings #-}

module Snake where

import Web.Scotty
import Control.Monad.Trans
import Data.Aeson
import Network.HTTP.Types.Status
import Control.Monad

import System.Environment
import Data.Maybe
import Text.Read

import Start
import Move
import End

-- See Start.hs for different snake configuration options.
-- Not super important, but perhaps it is nice to change the color.
snakeConfig :: StartResponse
snakeConfig = StartResponse "#FFFFFF"        -- color
                            "A0F000"         -- backup-color
                            "haskell > java" -- name
                            "bendr"         -- head type
                            "pixel"           -- tail type

ex = do
  inp <- liftIO $ lookupEnv "PORT"
  let port = read (fromMaybe "8080" inp) :: Int
  scotty port $ do
    post "/start" $ postStart snakeConfig
    post "/move" postMove
    post "/end" postEnd
    post "/ping" $ status (Status 200 "")

