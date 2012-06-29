{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module System.Console.Wizard.BasicIO 
        ( BasicIO (..)
        , basicIO
        ) where
import System.Console.Wizard
import System.Console.Wizard.Internal
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

instance Run Output      (IO) where runAlgebra (Output s w)        = putStr s   >> w
instance Run OutputLn    (IO) where runAlgebra (OutputLn s w)      = putStrLn s >> w
instance Run Line        (IO) where runAlgebra (Line s w)          = getLine    >>= w
instance Run Character   (IO) where runAlgebra (Character s w)     = getChar    >>= w
instance Run ArbitraryIO (IO) where runAlgebra (ArbitraryIO iov f) = iov        >>= f

type BasicIO = Output :+: OutputLn :+: Line :+: Character :+: ArbitraryIO :+: Empty

basicIO :: Wizard BasicIO a -> Wizard BasicIO a
basicIO = id
