{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty (scotty, get, json, param)
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)


data Beam = Beam {
    beamer :: String
    } deriving (Show, Generic)

instance ToJSON Beam
instance FromJSON Beam

beamResponse :: String -> Beam
beamResponse beam = Beam {beamer = beam}

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    json $ beamResponse "beam"
  get "/:word" $ do
    beam <- param "word"
    json $ beamResponse beam
