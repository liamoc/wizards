{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GADTs, KindSignatures #-}
module System.Console.Wizard.BasicIO
        ( BasicIO
        , runBasicIO
        ) where
import System.Console.Wizard
import System.Console.Wizard.Internal 
import Control.Monad.Trans
import Control.Monad.Prompt
import Control.Monad.Trans.Maybe


-- | A very simple standard IO backend for @wizards@, supporting input and output.
--   Default text and password masks are ignored.
--   A more full-featured back-end is based on Haskeline.
--   Arbitrary IO actions can be performed in wizards via a 'MonadIO' instance.
data BasicIO (m :: * -> *) r = ArbitraryIO (IO r)

-- | Runs a Wizard action in the BasicIO backend.
runBasicIO :: Wizard BasicIO a -> IO (Maybe a)
runBasicIO (Wizard (MaybeT c)) = runRecPromptM f c
  where f :: WizardAction BasicIO (RecPrompt (WizardAction BasicIO) ) c -> IO c
        f (Line s) = getLine
        f (Character s) = getChar
        f (Password s m) = getLine
        f (LinePreset s f b) = getLine
        f (Output s) = putStr s
        f (OutputLn s) = putStrLn s
        f (Backend (ArbitraryIO a)) = a

instance MonadIO (Wizard BasicIO) where
    liftIO = prompt . Backend . ArbitraryIO
