{-# LANGUAGE FlexibleContexts, TypeOperators, Trustworthy #-}
-- Necessary for MonadIO instance.
{-# LANGUAGE UndecidableInstances #-}
module System.Console.Wizard 
    ( -- * Wizards
      -- $intro
      Wizard (..)   
    , PromptString (..)
    , run
    , (:<:)
      -- * Primitives
      -- $primitives
    , Line  
    , line
    , LinePrewritten
    , linePrewritten
    , Password
    , password
    , Character
    , character
    , Output 
    , output
    , OutputLn
    , outputLn
    , ArbitraryIO
      -- * Modifiers
      -- $modifiers
    , retry
    , retryMsg
    , defaultTo
    , parser
    , validator
      -- * Convenience
    , nonEmpty
    , inRange
    , parseRead    
      -- * Utility
    , liftMaybe
    , ensure
    , readP
    ) where

import System.Console.Wizard.Internal

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Monad.Free
import Control.Monad.Reader
import Data.Maybe
import Data.Monoid

-- $primitives
-- /Primitives/ are the basic building blocks for @wizards@. Use these functions to produce wizards that
-- ask for input from the user, or output information.

-- | Output a string. Does not fail.
output :: (Output :<: b) => String -> Wizard b ()
output s = Wizard $ lift $ inject (Output s (Pure ()))

-- | Output a string followed by a newline. Does not fail.
outputLn :: (OutputLn :<: b) => String -> Wizard b ()
outputLn s = Wizard $ lift $ inject (OutputLn s (Pure ()))

-- | Read one line of input from the user. Cannot fail (but may throw exceptions, depending on the backend).
line :: (Line :<: b) => PromptString -> Wizard b String
line s = Wizard $ lift $ inject (Line s Pure) 

-- | Read a single character only from input. Cannot fail (but may throw exceptions, depending on the backend).
character :: (Character :<: b) 
          => PromptString
          -> Wizard b Char
character p = Wizard $ lift $ inject (Character p Pure)


instance (ArbitraryIO :<: b) => MonadIO (Wizard b) where
    liftIO v = Wizard $ lift $ inject (ArbitraryIO v Pure)  
-- | Read one line of input, with some default text already present, before and/or after the editing cursor.
---  Cannot fail (but may throw exceptions, depending on the backend).
linePrewritten :: (LinePrewritten :<: b) 
               => PromptString
               -> String  -- ^ Text to the left of the cursor
               -> String  -- ^ Text to the right of the cursor
               -> Wizard b String
linePrewritten p s1 s2 = Wizard $ lift $ inject (LinePrewritten p s1 s2 Pure)

-- | Read one line of password input, with an optional mask character.
---  Cannot fail (but may throw exceptions, depending on the backend).
password :: (Password :<: b)
         => PromptString
         -> Maybe Char -- ^ Mask character, if any.
         -> Wizard b String
password p mc = Wizard $ lift $ inject (Password p mc Pure)

-- $modifiers
-- /Modifiers/ change the behaviour of existing wizards.

-- | Retry produces a wizard that will retry the entire conversation again if it fails.
-- It is simply @retry x = x \<|\> retry x@.
retry :: Functor b => Wizard b a -> Wizard b a
retry x = x <|> retry x

-- | Same as 'retry', except an error message can be specified.
retryMsg :: (OutputLn :<: b) => String -> Wizard b a -> Wizard b a
retryMsg msg x = x <|> (outputLn msg >> retryMsg msg x)
                    
-- | @x \`defaultTo\` y@ will return @y@ if @x@ fails, e.g @parseRead line \`defaultTo\` 0@.
defaultTo :: Functor b => Wizard b a -> a -> Wizard b a
defaultTo wz d = wz <|> pure d

-- | Like 'fmap', except the function may be partial ('Nothing' causes the wizard to fail).
parser :: Functor b => (a -> Maybe c) -> Wizard b a -> Wizard b c
parser f a = a >>= liftMaybe . f

-- | @validator p@ causes a wizard to fail if the output value does not satisfy the predicate @p@.
validator :: Functor b => (a -> Bool) -> Wizard b a -> Wizard b a
validator = parser . ensure

-- | Simply @validator (not . null)@, makes a wizard fail if it gets an empty string.
nonEmpty :: Functor b => Wizard b [a] -> Wizard b [a]
nonEmpty = validator (not . null)

-- | Makes a wizard fail if it gets an ordered quantity outside of the given range.
inRange :: (Ord a, Functor b) => (a,a) -> Wizard b a -> Wizard b a
inRange (b,t) = validator (\x -> b <= x && x <= t)

-- | Simply @parser readP@. Attaches a simple @read@ parser to a 'Wizard'.
parseRead :: (Read a, Functor b) => Wizard b String -> Wizard b a
parseRead = parser (readP)

-- | Translate a maybe value into wizard success/failure.	
liftMaybe :: Functor b => Maybe a -> Wizard b a
liftMaybe (Just v) = pure v
liftMaybe (Nothing) = mzero

-- | Ensures that a maybe value satisfies a given predicate.
ensure :: (a -> Bool) -> a -> Maybe a
ensure p v | p v       = Just v
           | otherwise = Nothing

-- | A read-based parser for the 'parser' modifier.
readP :: Read a => String -> Maybe a
readP = fmap fst . listToMaybe . reads
