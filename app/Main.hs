{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import Data.Text.Lazy (Text)
import Data.Maybe (fromMaybe)

{-
 - newtype ScottyT e m a =
 -   ScottyT { runS :: State (ScottyState e m) a }
 -   deriving ( Functor, Applicative, Monad )
 -
 - newtype ActionT e m a =
 -   ActionT { runAM :: ExceptT (ActionError e)
 -                              (ReaderT ActionEnv
 -                                       (StateT ScottyResponse m)) a }
 -   deriving ( Functor, Applicative )
 -
 - type ScottyM = ScottyT Text IO
 - type ActionM = ActionT Text IO
 -
 - class MonadTrans t where
 -   lift :: (Monad m) => m a -> t m a
 -}

param' :: Parsable a => Text -> ActionM (Maybe a)
param' k = rescue (Just <$> param k) (const (return Nothing))

main :: IO ()
main = scotty 3000 $ do
  -- get :: RoutePattern -> ActionM () -> ScottyM ()
  get "/:word" $ do
    beam' <- param' "word"
    let beam = fromMaybe "" beam'
    i <- param' "num"
    liftIO $ print (i :: Maybe Integer)
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]


