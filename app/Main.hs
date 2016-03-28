{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT(..))
import Data.Monoid (mconcat)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
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
main = scotty 3000 $ do
  -- get :: RoutePattern -> ActionM () -> ScottyM ()
  get "/:word" $ do
    beam <- param "word"
    -- notice that the IO is placed in ActionM:
    (ActionT
     . (ExceptT . fmap Right)
     . (ReaderT . const)
     . lift) (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]


