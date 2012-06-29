{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveFunctor, FlexibleContexts, TypeOperators, GeneralizedNewtypeDeriving, Trustworthy, ExistentialQuantification, EmptyDataDecls #-}
module System.Console.Wizard.Internal ( Wizard (..)
                                      , PromptString (..)
                                      , (:+:) (..)
                                      , (:<:) (..)
                                      , inject
                                      , Run (..)
                                      , run
                                      , Output (..)
                                      , OutputLn (..)
                                      , Line (..)
                                      , LinePrewritten (..)
                                      , Password (..)
                                      , Character (..)
                                      , ArbitraryIO (..)
                                      , Empty (..)
                                      -- $backend
                                      ) where
import Control.Monad.Free
import Control.Monad.Trans.Maybe
import Control.Applicative

type PromptString = String

data (f :+: g) w = Inl (f w) | Inr (g w) deriving Functor

class (Functor sub, Functor sup) => sub :<: sup where
   inj :: sub a -> sup a

instance Functor f => f :<: f where inj = id
instance (Functor f, Functor g) => f :<: (f :+: g) where inj = Inl
instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where inj = Inr . inj

inject :: (g :<: f ) => g (Free f a) -> Free f a
inject = Impure . inj

class Run a b where
   runAlgebra :: a (b v) -> b v 

instance (Run f b, Run g b) => Run (f :+: g) b where
   runAlgebra (Inl r) = runAlgebra r
   runAlgebra (Inr r) = runAlgebra r


infixr 9 :+:


data Output w = Output String w deriving Functor
data OutputLn w = OutputLn String w deriving Functor
data Line w = Line PromptString (String -> w) deriving Functor
data Character w = Character PromptString (Char -> w) deriving Functor
data LinePrewritten w = LinePrewritten PromptString String String (String -> w) deriving Functor
data Password w = Password PromptString (Maybe Char) (String -> w) deriving Functor
data ArbitraryIO w = forall a. ArbitraryIO (IO a) (a -> w) 
data Empty w deriving Functor
instance Run Empty b where runAlgebra = undefined
instance Functor (ArbitraryIO) where
    fmap f (ArbitraryIO iov f') = ArbitraryIO iov (fmap f f')

-- | A @Wizard a@ is a conversation with the user that will result in a data type @a@, or may fail.
--   A 'Wizard' is made up of one or more \"primitives\" (see below), composed using the 'Applicative',
--  'Monad' and 'Alternative' instances. The 'Alternative' instance is, as you might expect, a maybe-style cascade. 
--   If the first wizard fails, the next one is tried. `mzero` can be used to induce failure directly.
--  
--  The 'Wizard' constructor is exported here for use when developing backends,  but it is better for end-users to 
--  simply pretend that 'Wizard' is an opaque data type. Don't depend on this unless you have no other choice.
-- 
--  'Wizard's are, internally, just a maybe transformer over a free monad built from some coproduct of functors,
--  each of which is a primitive action.
newtype Wizard backend a = Wizard (MaybeT (Free backend) a)
      deriving (Monad, Functor, Applicative, Alternative, MonadPlus)

run' :: (Functor f, Monad b,  Run f b) => Free f a -> b a
run' = foldFree return runAlgebra

run :: (Functor f, Monad b,  Run f b) => Wizard f a -> b (Maybe a)
run (Wizard c) = run' (runMaybeT c)


-- $backend
--   A short tutorial on writing backends.
--
--   Backends consist of two main components:
--   
--      1. A back-end data type (the type parameter to 'Wizard'), which includes constructors
--         for any primitive actions or modifiers that are specific to the back-end.
--
--      2. An interpreter function, of type @Wizard DataType a -> B (Maybe a)@ for some type @B@ (depending on the backend).
--         Typically this function will provide semantics for each 'WizardAction' using 'runRecPromptM' or similar.
--   
--   The 'Backend' constructor can be used to add back-end specific primitives and modifiers.
-- 
--   As an example, suppose I am writing a back-end to @IO@, like "System.Console.Wizard.BasicIO".
--   One additional primitive action that I might want to include is the ability to run arbitrary @IO@ actions while a wizard is running.
--   So, my backend data type will be:
--
-- @
-- data MyBackend (m :: * -> *) r = ArbitraryIO (IO r) -- kind signature to avoid defaulting to *
-- @
-- 
--   And my interpreter function will be:
--
-- @
--   runWizardMyBackend :: Wizard MyBackend a -> IO a
--   runWizardMyBackend (Wizard (MaybeT c)) = runRecPromptM f c
--         where f :: WizardAction MyBackend (RecPrompt (WizardAction MyBackend)) a -> IO a  
--               f (Output s) = putStr s
--               f (...     ) = ...
--               f (Backend (ArbitraryIO io)) = io
-- @
-- 
-- And then the action can be easily defined:
--
-- @
--   runIO :: IO a -> Wizard MyBackend a
--   runIO = prompt . Backend . ArbitraryIO 
-- @
--
-- I might also want to include a /modifier/, which say, colours any output text green. Assuming I have a function
-- @
--    withGreenText :: IO a -> IO a
-- @
-- which causes any output produced by the input action to be coloured green, we can use the 'Backend' constructor to transform
-- this into a wizard modifier.
-- 
-- @
--data MyBackend m r = ArbitraryIO (IO r)
--                   | GreenText (m r)
--
--runWizardMyBackend :: Wizard MyBackend
--runWizardMyBackend (Wizard (MaybeT c)) = runRecPromptM f c
--      where f :: WizardAction MyBackend (RecPrompt (WizardAction MyBackend)) a -> IO a  
--            f (Output s) = putStr s
--            f (...     ) = ...
--            f (Backend (ArbitraryIO io)) = io
--            f (Backend (GreenText a)) = withGreenText $ runRecPromptM f a
--
--greenText :: Wizard MyBackend a -> Wizard MyBackend a
--greenText (Wizard (MaybeT a)) = prompt (Backend (GreenText a))
-- @
--
-- 