{-#LANGUAGE NamedFieldPuns #-}

import Control.Monad.Reader

data Config = Config {
  verbose :: Bool
  {- other parameters -}
  }

type ConfigM = Reader Config

getConfiguration :: IO Config
getConfiguration = pure Config { verbose = True {- .. -} }

main :: IO ()
main = do
  config <- getConfiguration
  let result = runReader work config
  print result

work :: Reader Config ()
work = do
  -- ...
  doSomething
  -- ...

doSomething :: Reader Config ()
doSomething = do
  -- ...
  doSomethingSpecial
  -- ..

doSomethingSpecial :: Reader Config ()
doSomethingSpecial = do
  -- ...
  -- Config {verbose} <- ask     ; we could do it this way, or
  vrb <- asks verbose
  when vrb beVerbose
  pure ()

beVerbose :: Reader Config ()
beVerbose = do
  pure ()

silent :: Config -> Config
silent config = config {verbose = False}

doSomethingSilently :: Reader Config ()
doSomethingSilently = local silent doSomethingSpecial
