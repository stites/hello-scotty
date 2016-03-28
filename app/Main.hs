{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

leftErr ::Parsable a => Text -> Either String a
leftErr k = Left $ "the key: " ++ show k ++ " was missing!"

param' :: Parsable a => Text -> ExceptT String ActionM a
param' k = ExceptT $ rescue (Right <$> param k) (const.return $ leftErr k)

type Reco = (Integer,Integer,Integer,Integer)

tshow = TL.pack . show

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    reco <- runExceptT $ do
      a <- param' "1"
      liftIO $ print a
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift . lift) $ print b
      return ((a, b, c, d) :: Reco)
    case reco of
      Left e -> text (TL.pack e)
      Right r -> html $ mconcat ["<h1>Scotty, ", tshow r, " me up!</h1>"]


