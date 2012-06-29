{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeOperators #-}
module System.Console.Wizard.Haskeline 
        ( UnexpectedEOF (..)
        , Haskeline (..)
        , haskeline
        , withSettings
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

data WithSettings w = WithSettings (Settings IO) w deriving (Functor) 

instance Run Output         (InputT IO) where runAlgebra (Output s w)               = outputStr s                       >> w
instance Run OutputLn       (InputT IO) where runAlgebra (OutputLn s w)             = outputStrLn s                     >> w
instance Run Line           (InputT IO) where runAlgebra (Line s w)                 = getInputLine s                    >>= mEof w
instance Run Character      (InputT IO) where runAlgebra (Character s w)            = getInputChar s                    >>= mEof w
instance Run LinePrewritten (InputT IO) where runAlgebra (LinePrewritten p s1 s2 w) = getInputLineWithInitial p (s1,s2) >>= mEof w
instance Run Password       (InputT IO) where runAlgebra (Password p mc w)          = getPassword mc p                  >>= mEof w
instance Run ArbitraryIO    (InputT IO) where runAlgebra (ArbitraryIO iov f)        = liftIO iov                        >>= f
instance Run WithSettings   (InputT IO) where runAlgebra (WithSettings sets w)      = liftIO (runInputT sets w)

mEof = maybe (throw UnexpectedEOF)    

type Haskeline = Output :+: OutputLn :+: Line :+: Character :+: LinePrewritten :+: Password :+: ArbitraryIO :+: WithSettings :+: Empty

haskeline :: Wizard Haskeline a -> Wizard Haskeline a
haskeline = id

-- | Modifies a wizard so that it will run with different Haskeline 'Settings' to the top level input monad.
withSettings :: (WithSettings :<: b) => Settings IO -> Wizard b a -> Wizard b a
withSettings sets (Wizard (MaybeT v)) = Wizard $ MaybeT $ inject (WithSettings sets v)