{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
-- An example of embedding a custom monad into
-- Scotty's transformer stack, using ReaderT to provide access
-- to a TVar containing global state.
--
-- Note: this example is somewhat simple, as our top level
-- is IO itself. The types of 'scottyT' and 'scottyAppT' are
-- general enough to allow a Scotty application to be
-- embedded into any MonadIO monad.
module UHF.TeeVee.Application (startServer) where

import Control.Concurrent.STM
import Control.Monad.Reader 

import Data.Text.Lazy (pack)

import Network.Wai.Middleware.Static

import UHF.Types (ServerData, ServerApplication)
import UHF.Actions

import Web.Scotty.Trans


startServer :: ServerData -> IO ()
startServer sync = do
        -- Note that 'runM' is only called once, at startup.
    let runM = flip runReaderT sync
        -- 'runActionToIO' is called once per action.
        runActionToIO = runM

    scottyT 3000 runM runActionToIO $ do
        middleware $ staticPolicy (noDots >-> addBase "static")
        
        get "/" $ do
            -- c <- tvM $ gets tickCount
            text $ "waka"
