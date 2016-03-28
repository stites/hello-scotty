{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
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

param' :: Parsable a => Text -> MaybeT ActionM a
param' k = MaybeT $ rescue (Just <$> param k) (const (return Nothing))

type Reco = (Integer, Integer, Integer, Integer)

main :: IO ()
main = scotty 3000 $ do
  -- get :: RoutePattern -> ActionM () -> ScottyM ()
  get "/:word" $ do
    beam <- param "word"
    -- reco :: Maybe Reco
    reco <- runMaybeT $ do
      a <- param' "1"
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift.lift) $ print b
      return ((a,b,c,d) :: Reco)
    liftIO $ print reco
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]


