{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, FlexibleInstances, TypeOperators, DoAndIfThenElse, GeneralizedNewtypeDeriving, Trustworthy #-}
module System.Console.Wizard.Pure
        ( Pure (..)
        , UnexpectedEOI (..)
        , runPure
        , PureState (..)
        ) where

import System.Console.Wizard
import System.Console.Wizard.Internal 
import Control.Monad.Trans
import Control.Monad.State.Lazy
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

-- | The pure backend is actually just a simple state monad, with the following state.
type PureState = ([String], Seq Char)

-- | Run a wizard in the Pure backend
runPure :: Wizard Pure a -> String -> (Maybe a, String)
runPure wz input = let (a,(_,o)) = runState (run wz) (lines input, empty) 
                       in (a, toList o)

getPureLine :: State PureState String
getPureLine = do crashIfNull
                 x <- head . fst <$> get
                 modify (first tail)
                 return x

crashIfNull :: State PureState ()
crashIfNull = do (x, y ) <- get
                 when (null x) $ throw UnexpectedEOI

getPureChar :: State PureState Char
getPureChar = do crashIfNull
                 x <- null . head . fst <$> get
                 if x then do 
                    modify (first tail)
                    return '\n'
                 else do
                    r <- head . head . fst <$> get
                    modify (first (\ (x : r) -> tail x : r))
                    return r
                    
outputPure :: String -> State PureState ()                    
outputPure s = modify (second (>< fromList s))
            >> modify (\s -> s `seq` s)

outputLnPure :: String -> State PureState ()                    
outputLnPure s = modify (second $ (|> '\n') . (>< fromList s))
              >> modify (\s -> s `seq` s)


instance Run (State PureState) Output    where runAlgebra (Output s w)        = outputPure s   >> w
instance Run (State PureState) OutputLn  where runAlgebra (OutputLn s w)      = outputLnPure s >> w
instance Run (State PureState) Line      where runAlgebra (Line s w)          = getPureLine    >>= w
instance Run (State PureState) Character where runAlgebra (Character s w)     = getPureChar    >>= w

-- | The 'Pure' backend supports only simple input and output.
--   Support for 'Password' and 'LinePrewritten' features can be added with 
--   a shim from `System.Console.Wizard.Shim`. 
newtype Pure a = Pure ((Output :+: OutputLn :+: Line :+: Character) a) 
               deriving ( (:<:) Output
                        , (:<:) OutputLn
                        , (:<:) Line
                        , (:<:) Character
                        , Functor
                        , Run (State PureState)
                        )

