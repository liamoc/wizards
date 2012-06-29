{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, FlexibleInstances, TypeOperators, DoAndIfThenElse #-}
module System.Console.Wizard.Pure
        ( Pure (..)
        , UnexpectedEOI (..)
        , runPure
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

type PureState = ([String], Seq Char)

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


instance Run Output      (State PureState) where runAlgebra (Output s w)        = outputPure s   >> w
instance Run OutputLn    (State PureState) where runAlgebra (OutputLn s w)      = outputLnPure s >> w
instance Run Line        (State PureState) where runAlgebra (Line s w)          = getPureLine    >>= w
instance Run Character   (State PureState) where runAlgebra (Character s w)     = getPureChar    >>= w

type Pure = Output :+: OutputLn :+: Line :+: Character :+: Empty

