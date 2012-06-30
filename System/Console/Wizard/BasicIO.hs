{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, GeneralizedNewtypeDeriving, Trustworthy #-}
module System.Console.Wizard.BasicIO 
        ( BasicIO
        , basicIO
        ) where
import System.Console.Wizard
import System.Console.Wizard.Internal
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

instance Run IO Output      where runAlgebra (Output s w)        = putStr s   >> w
instance Run IO OutputLn    where runAlgebra (OutputLn s w)      = putStrLn s >> w
instance Run IO Line        where runAlgebra (Line s w)          = getLine    >>= w
instance Run IO Character   where runAlgebra (Character s w)     = getChar    >>= w
instance Run IO ArbitraryIO where runAlgebra (ArbitraryIO iov f) = iov        >>= f

-- | The 'BasicIO' backend supports only simple input and output.
--   Support for 'Password' and 'LinePrewritten' features can be added with 
--   a shim from `System.Console.Wizard.Shim`. 
newtype BasicIO a = BasicIO (( Output 
                           :+: OutputLn 
                           :+: Line 
                           :+: Character 
                           :+: ArbitraryIO) a)
                  deriving ( (:<:) Output
                           , (:<:) OutputLn
                           , (:<:) Line
                           , (:<:) Character
                           , (:<:) ArbitraryIO
                           , Functor
                           , Run IO
                           )

-- | A simple identity function, used to restrict types if the type inferred by GHC is too general.
--   You could achieve the same effect with a type signature, but this is slightly less typing.
basicIO :: Wizard BasicIO a -> Wizard BasicIO a
basicIO = id
