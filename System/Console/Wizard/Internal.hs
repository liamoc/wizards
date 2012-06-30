{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveFunctor, FlexibleContexts, TypeOperators, GeneralizedNewtypeDeriving, Trustworthy, ExistentialQuantification, EmptyDataDecls #-}
module System.Console.Wizard.Internal ( Wizard (..)
                                      , PromptString (..)
                                      , (:+:) (..)
                                      , (:<:)
                                      , inject
                                      , Run (..)
                                      , run
                                      -- $functors
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

-- | A string for a prompt
type PromptString = String

-- | A @Wizard b a@ is a conversation with the user via back-end @b@ that will result in a data type @a@, or may fail.
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

-- | Coproduct of two functors
data (f :+: g) w = Inl (f w) | Inr (g w) deriving Functor

-- | Subsumption of two functors. You shouldn't define any of your own instances of this when writing back-ends, rely only on GeneralizedNewtypeDeriving.
class (Functor sub, Functor sup) => sub :<: sup where
   inj :: sub a -> sup a

instance Functor f => f :<: f where inj = id
instance (Functor f, Functor g) => f :<: (f :+: g) where inj = Inl
instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where inj = Inr . inj

-- | Injection function for free monads, see \"Data Types a la Carte\" from Walter Swierstra, @http:\/\/www.cs.ru.nl\/~W.Swierstra\/Publications\/DataTypesALaCarte.pdf@
inject :: (g :<: f ) => g (Free f a) -> Free f a
inject = Impure . inj

-- | A class for implementing actions on a backend. E.g Run IO Output provides an interpreter for the Output action in the IO monad.
class Run a b where
   runAlgebra :: b (a v) -> a v 

instance (Run b f, Run b g) => Run b (f :+: g) where
   runAlgebra (Inl r) = runAlgebra r
   runAlgebra (Inr r) = runAlgebra r

infixr 9 :+:

-- $functors
--  Each of the following functors is a primitive action. A back-end provides interpreters for these actions using the 'Run' class,

data Output w = Output String w deriving Functor
data OutputLn w = OutputLn String w deriving Functor
data Line w = Line PromptString (String -> w) deriving Functor
data Character w = Character PromptString (Char -> w) deriving Functor
data LinePrewritten w = LinePrewritten PromptString String String (String -> w) deriving Functor
data Password w = Password PromptString (Maybe Char) (String -> w) deriving Functor
data ArbitraryIO w = forall a. ArbitraryIO (IO a) (a -> w) 
data Empty w deriving Functor
instance Run b Empty where runAlgebra = undefined
instance Functor (ArbitraryIO) where
    fmap f (ArbitraryIO iov f') = ArbitraryIO iov (fmap f f')



run' :: (Functor f, Monad b,  Run b f) => Free f a -> b a
run' = foldFree return runAlgebra

-- | Run a wizard using some back-end.
run :: (Functor f, Monad b,  Run b f) => Wizard f a -> b (Maybe a)
run (Wizard c) = run' (runMaybeT c)


-- $backend
--   A short tutorial on writing backends.
--
--   Backends consist of two main components:
--   
--      1. A monad, M, in which the primitive actions are interpreted. 'Run' instances specify an interpreter for each supported
--         action, e.g Run M Output will specify an interpreter for the Output primitive action in the monad M.
--
--      2. A newtype, Backend a, which is a functor, usually implemented by wrapping a coproduct of all supported features.
--         (:<:) instances, the Functor instance, and the Run instance are provided by generalized newtype deriving.
-- 
--   As an example, suppose I am writing a back-end to @IO@, like "System.Console.Wizard.BasicIO". I want to support basic input and output,
--   and arbitrary IO, so I declare instances for 'Run' for the 'IO' monad: 
--
--  @
--  instance Run IO Output      where runAlgebra (Output s w)        = putStr s   >> w
--  instance Run IO OutputLn    where runAlgebra (OutputLn s w)      = putStrLn s >> w
--  instance Run IO Line        where runAlgebra (Line s w)          = getLine    >>= w
--  instance Run IO Character   where runAlgebra (Character s w)     = getChar    >>= w
--  instance Run IO ArbitraryIO where runAlgebra (ArbitraryIO iov f) = iov        >>= f
--  @
--  
--  And then I would define the newtype for the backend, which we can call MyIOBackend:
--  
--  @
--  newtype MyIOBackend a = MyIOBackend ((Output :+: OutputLn :+: Line :+: Character :+: ArbitraryIO) a)
--                        deriving (Functor, Run IO
--                                 , (:<:) Output
--                                 , (:<:) OutputLn
--                                 , (:<:) Line
--                                 , (:<:) Character
--                                 , (:<:) ArbitraryIO
--                                 )
--  @
--
--  A useful convenience is to provide a simple identity function to serve as a type coercion:
--  
--  @
--  myIOBackend :: Wizard MyIOBackend a -> Wizard MyIOBackend a
--  myIOBackend = id
--  @
-- 
--  One additional primitive action that I might want to include is the ability to clear the screen at a certain point.
--  So, we define a new data type for the action:
--
--  @
--  data ClearScreen w = ClearScreen w deriving Functor -- via -XDeriveFunctor
--  @
-- 
--  And a \"smart\" constructor for use by the user:
--
--  @
--  clearScreen :: (ClearScreen :\<: b) => Wizard b ()
--  clearScreen = Wizard $ lift $ inject (ClearScreen (Pure ())) 
--  @
--
--  (These smart constructors all follow a similar pattern. See the source of System.Console.Wizard for more examples)
--
--  And then we define an interpreter for it:
-- 
--  @
--  instance Run IO ArbitraryIO where runAlgebra (ClearScreen f) = clearTheScreen >> f
--  @
--
--  Now, we can use this as-is simply by directly extending our back-end:
--
--  @
--  foo :: Wizard (ClearScreen :+: MyIOBackend)
--  foo = clearScreen >> output \"Hello World!\"
--  @
--
--  Or, we could modify @MyIOBackend@ to include the extension directly.
--
--
--  For custom actions that /return/ output, the definition looks slightly different. Here is the definition of Line:
--
--  @
--  data Line w = Line (PromptString) (String -> w) deriving Functor -- via -XDeriveFunctor
--  @
-- 
--  And the smart constructor looks like this:
--
--  @
--  line :: (Line :\<: b) => PromptString -> Wizard b String
--  line s = Wizard $ lift $ inject (Line s Pure) 
--  @