{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GADTs #-}
module System.Console.Wizard.Haskeline 
        ( Haskeline
        , UnexpectedEOF (..)
        , runHaskeline
        , withSettings
        ) where
import System.Console.Wizard
import System.Console.Wizard.Internal
import System.Console.Wizard.Internal.SimpleMenu
import System.Console.Haskeline    
import Control.Monad.Trans
import Control.Monad.Prompt
import Control.Monad.Trans.Maybe
import Control.Exception
import Data.Typeable

-- | A Haskeline backend for @wizards@, supporting input, output, default text, and password input.
--   In addition, Haskeline settings can be modified for a single wizard, and arbitrary IO can be
--   performed using the 'MonadIO' instance.
data Haskeline m r = SetSettings (Settings IO) (m r)
                   | ArbitraryIO (IO r)

-- | The Haskeline back-end will throw this exception if EOF is encountered
--   when it is not expected. Specifically, when actions such as 'getInputLine' return 'Nothing'.
data UnexpectedEOF = UnexpectedEOF deriving (Show, Typeable)
instance Exception UnexpectedEOF

-- | Runs a Wizard action in the Haskeline backend.
runHaskeline :: Wizard Haskeline a -> InputT IO (Maybe a)
runHaskeline (Wizard c) = runRecPromptM f $ runMaybeT c
  where f :: WizardAction Haskeline (RecPrompt (WizardAction Haskeline) )  a -> InputT IO a
        f (Line s) = getInputLine s >>= maybeToException UnexpectedEOF
        f (Character s) = getInputChar s >>= maybeToException UnexpectedEOF
        f (Password s m) = getPassword m s >>= maybeToException UnexpectedEOF
        f (LinePreset s f b) = getInputLineWithInitial s (f,b) >>= maybeToException UnexpectedEOF 
        f (Output s) = outputStr s
        f (OutputLn s) = outputStrLn s
        f (Menu p s) = runHaskeline (simpleMenu p s)
        f (Backend (SetSettings s v)) = liftIO $ runInputT s (runRecPromptM f v)
        f (Backend (ArbitraryIO a)) = liftIO $ a

-- | Modifies a wizard so that it will run with different Haskeline 'Settings' to the top level input monad.
withSettings :: Settings IO -> Wizard Haskeline a -> Wizard Haskeline a
withSettings sets (Wizard (MaybeT v)) = Wizard $ MaybeT $ prompt $ Backend $ SetSettings sets $ v

instance MonadIO (Wizard Haskeline) where
    liftIO = prompt . Backend . ArbitraryIO

maybeToException :: (Monad m, Exception e) => e -> Maybe a -> m a
maybeToException e (Just v) = return v
maybeToException e (Nothing) = throw e