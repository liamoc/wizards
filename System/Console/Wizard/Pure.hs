{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GADTs, KindSignatures, EmptyDataDecls, DoAndIfThenElse #-}
module System.Console.Wizard.Pure
        ( Pure
        , runPure
        ) where

import System.Console.Wizard
import System.Console.Wizard.Internal 
import System.Console.Wizard.Internal.SimpleMenu
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.Prompt
import Control.Monad.Trans.Maybe
import Control.Applicative((<$>))
import Data.Typeable
import Data.Sequence(Seq, (|>), (><), fromList, empty)
import Control.Monad
import Control.Exception
import Control.Arrow
import Data.Foldable(toList)

-- | Thrown if the wizard ever unexpectedly runs out of input.
data UnexpectedEOI = UnexpectedEOI deriving (Show, Typeable)
instance Exception UnexpectedEOI

-- | A very simple pure backend for @wizards@, supporting input and output. Prompt strings are ignored.
data Pure (m :: * -> *) r 

type PureState = ([String], Seq Char)

crashIfNull :: State PureState ()
crashIfNull = do (x, y ) <- get
                 when (null x) $ throw UnexpectedEOI

-- | Runs a Wizard action in the Pure backend. Returns all output as a string and the return value, if any.
runPure :: Wizard Pure a     -- ^ Wizard to run
        -> String            -- ^ Input to use
        -> (Maybe a, String) -- ^ (ReturnValue, Output)
runPure w s = let (r, (_,o)) = runState (run w) (lines s, empty)
               in (r, toList o)
  where run :: Wizard Pure a -> State PureState (Maybe a)
        run (Wizard (MaybeT c)) = runRecPromptM f c       
        f :: WizardAction Pure (RecPrompt (WizardAction Pure) ) c -> State PureState c
        f (Line s) = do crashIfNull
                        x <- head . fst <$> get
                        modify (first tail)
                        return x
        f (Character s) = do crashIfNull
                             x <- null . head . fst <$> get
                             if x then do
                                 modify (first tail)
                                 return '\n'
                             else do
                                 r <- head . head . fst <$> get
                                 modify (first (\ (x : r) -> tail x : r))
                                 return r
        f (Password s _) = f (Line s)
        f (LinePreset s _ _) = f (Line s)
        f (Menu p m) = run (simpleMenu p m)
        f (Output s) = modify (second (>< fromList s))
                    >> modify (\s -> s `seq` s)
        f (OutputLn s) = modify (second $ (|> '\n') . (>< fromList s))
                      >> modify (\s -> s `seq` s)

