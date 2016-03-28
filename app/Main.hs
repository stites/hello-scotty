{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Text.Lazy (Text)
import Data.Maybe (fromMaybe)

leftErr ::Parsable a => Text -> Either String a
leftErr k = Left $ "the key: " ++ show k ++ " was missing!"

param' :: Parsable a => Text -> ActionM (Either String a)
param' k = rescue (Right <$> param k) (const.return $ leftErr k)

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    a <- param' "1"
    let a' = either (const 0) id a
    liftIO $ print (a :: Either String Int)
    liftIO $ print (a' :: Int)
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]


