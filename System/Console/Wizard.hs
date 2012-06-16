{-# LANGUAGE GeneralizedNewtypeDeriving, Trustworthy, MultiParamTypeClasses, FlexibleInstances #-}
module System.Console.Wizard 
    ( -- * Wizards
      -- $intro
      Wizard (..)   
    , Menu
    , PromptString (..)
      -- * Primitives
      -- $primitives
    , line
    , linePrewritten
    , password
    , character 
    , output
    , outputLn
    , menu
    , choice
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
import Control.Monad.Reader
import Control.Monad.Prompt
import Data.Maybe
import Data.Monoid

-- | A @Wizard a@ is a conversation with the user that will result in a data type @a@, or may fail.
--   A 'Wizard' is made up of one or more \"primitives\" (see below), composed using the 'Applicative',
--  'Monad' and 'Alternative' instances. The 'Alternative' instance is, as you might expect, a maybe-style cascade. 
--   If the first wizard fails, the next one is tried. `mzero` can be used to induce failure directly.
--  
--  The 'Wizard' constructor is exported here for use when developing backends,  but it is better for end-users to 
--  simply pretend that 'Wizard' is an opaque data type. Don't depend on this unless you have no other choice.
-- 
--  'Wizard's are, internally, just a maybe transformer over a prompt monad for each primitive action.
newtype Wizard backend a = Wizard (MaybeT (RecPrompt (WizardAction backend)) a)
      deriving (Monad, Functor, Applicative, Alternative, MonadPlus)
instance MonadPrompt (WizardAction s (RecPrompt (WizardAction s))) (Wizard s) where    
    prompt = Wizard . lift . prompt


-- | A menu is a collection of labeled wizards. Construct with `choice` and compose with `mappend` @(<>)@ in the 'Monoid' instance
newtype Menu b a = MenuC [(String, Wizard b a)]
   deriving (Monoid)

-- | Constructor for a menu consisting of a single option
choice :: PromptString -> Wizard b a -> Menu b a
choice s w = MenuC [(s,w)]

-- | Run a menu with the given prompt string.
menu :: PromptString -> Menu b a -> Wizard b a
menu s (MenuC l) = join $ join $ fmap liftMaybe $ prompt $ Menu s l


-- $primitives
-- /Primitives/ are the basic building blocks for @wizards@. Use these functions to produce wizards that
-- ask for input from the user, or output information.

-- | Read one line of input from the user. Cannot fail (but may throw exceptions, depending on the backend).
line :: PromptString -> Wizard b String
line str = prompt $ Line str

-- | Read one line of input, with some default text already present, before and/or after the editing cursor.
--   Backends are not required to display this default text, or position the cursor anywhere, it is merely
--   a suggestion. Cannot fail (but may throw exceptions, depending on the backend).
linePrewritten :: PromptString
               -> String  -- ^ Text to the left of the cursor
               -> String  -- ^ Text to the right of the cursor
               -> Wizard b String
linePrewritten p s1 s2 = prompt $ LinePreset p s1 s2

-- | Read one line of password input, with an optional mask character.
--   The exact masking behavior of the password may vary from backend to backend. The masking character
--   does not have to be honoured. Cannot fail (but may throw exceptions, depending on the backend).
password :: PromptString
         -> Maybe Char -- ^ Mask character, if any.
         -> Wizard b String
password str m = prompt $ Password str m 
                 
-- | Read a single character only from input. Cannot fail (but may throw exceptions, depending on the backend).
character :: PromptString -> Wizard b Char
character = prompt . Character 

-- | Output a string, if the backend used supports output. 
output :: String -> Wizard b ()
output = prompt . Output

-- | Output a string followed by a newline, if the backend used supports such output.
outputLn :: String -> Wizard b ()
outputLn = prompt . OutputLn

-- $modifiers
-- /Modifiers/ change the behaviour of existing wizards.

-- | Retry produces a wizard that will retry the entire conversation again if it fails.
-- Conceptually, it could thought of as @retry x = x \<|\> retry x@, however it also prints
-- a user-friendly error message in the event of failure.
retry :: Wizard b a -> Wizard b a
retry = retryMsg "Invalid input. Please try again."

-- | Same as 'retry', except the error message can be specified.
retryMsg :: String -> Wizard b a -> Wizard b a
retryMsg msg x = x <|> (outputLn msg >> retryMsg msg x)
                    
-- | @x \`defaultTo\` y@ will return @y@ if @x@ fails, e.g @parseRead line \`defaultTo\` 0@.
defaultTo :: Wizard b a -> a -> Wizard b a
defaultTo wz d = wz <|> pure d

-- | Like 'fmap', except the function may be partial ('Nothing' causes the wizard to fail).
parser :: (a -> Maybe c) -> Wizard b a -> Wizard b c
parser f a = a >>= liftMaybe . f

-- | @validator p@ causes a wizard to fail if the output value does not satisfy the predicate @p@.
validator :: (a -> Bool) -> Wizard b a -> Wizard b a
validator = parser . ensure

-- | Simply @validator (not . null)@, makes a wizard fail if it gets an empty string.
nonEmpty :: Wizard b [a] -> Wizard b [a]
nonEmpty = validator (not . null)

-- | Makes a wizard fail if it gets an ordered quantity outside of the given range.
inRange :: (Ord a) => (a,a) -> Wizard b a -> Wizard b a
inRange (b,t) = validator (\x -> b <= x && x <= t)

-- | Simply @parser readP@. Attaches a simple @read@ parser to a 'Wizard'.
parseRead :: (Read a) => Wizard b String -> Wizard b a
parseRead = parser (readP)

-- | Translate a maybe value into wizard success/failure.	
liftMaybe :: Maybe a -> Wizard b a
liftMaybe (Just v) = pure v
liftMaybe (Nothing) = mzero

-- | Ensures that a maybe value satisfies a given predicate.
ensure :: (a -> Bool) -> a -> Maybe a
ensure p v | p v       = Just v
           | otherwise = Nothing

-- | A read-based parser for the 'parser' modifier.
readP :: Read a => String -> Maybe a
readP = fmap fst . listToMaybe . reads
