{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeOperators, GeneralizedNewtypeDeriving, Trustworthy #-}
module System.Console.Wizard.Haskeline 
        ( UnexpectedEOF (..)
        , Haskeline
        , haskeline
        , withSettings
        , WithSettings(..)
        ) where
import System.Console.Wizard
import System.Console.Wizard.Internal
import System.Console.Haskeline    
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Exception
import Data.Typeable


-- | The Haskeline back-end will throw this exception if EOF is encountered
--   when it is not expected. Specifically, when actions such as 'getInputLine' return 'Nothing'.
data UnexpectedEOF = UnexpectedEOF deriving (Show, Typeable)
instance Exception UnexpectedEOF

-- | Haskeline supports all the following features completely. 
newtype Haskeline a = Haskeline (( Output 
                               :+: OutputLn 
                               :+: Line 
                               :+: Character 
                               :+: LinePrewritten 
                               :+: Password 
                               :+: ArbitraryIO 
                               :+: WithSettings) a)
                  deriving ( (:<:) Output
                           , (:<:) OutputLn
                           , (:<:) Line
                           , (:<:) Character
                           , (:<:) LinePrewritten
                           , (:<:) Password
                           , (:<:) ArbitraryIO
                           , (:<:) WithSettings
                           , Functor
                           , Run (InputT IO)
                           )                           

-- | Modifies a wizard so that it will run with different Haskeline 'Settings' to the top level input monad.
withSettings :: (WithSettings :<: b) => Settings IO -> Wizard b a -> Wizard b a
withSettings sets (Wizard (MaybeT v)) = Wizard $ MaybeT $ inject (WithSettings sets v)

data WithSettings w = WithSettings (Settings IO) w deriving (Functor) 

instance Run (InputT IO) Output          where runAlgebra (Output s w)               = outputStr s                       >> w
instance Run (InputT IO) OutputLn        where runAlgebra (OutputLn s w)             = outputStrLn s                     >> w
instance Run (InputT IO) Line            where runAlgebra (Line s w)                 = getInputLine s                    >>= mEof w
instance Run (InputT IO) Character       where runAlgebra (Character s w)            = getInputChar s                    >>= mEof w
instance Run (InputT IO) LinePrewritten  where runAlgebra (LinePrewritten p s1 s2 w) = getInputLineWithInitial p (s1,s2) >>= mEof w
instance Run (InputT IO) Password        where runAlgebra (Password p mc w)          = getPassword mc p                  >>= mEof w
instance Run (InputT IO) ArbitraryIO     where runAlgebra (ArbitraryIO iov f)        = liftIO iov                        >>= f
instance Run (InputT IO) WithSettings    where runAlgebra (WithSettings sets w)      = liftIO (runInputT sets w)

mEof = maybe (throw UnexpectedEOF)    


-- | A simple identity function, used to restrict types if the type inferred by GHC is too general.
--   You could achieve the same effect with a type signature, but this is slightly less typing.
haskeline :: Wizard Haskeline a -> Wizard Haskeline a
haskeline = id

