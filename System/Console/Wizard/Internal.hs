{-# LANGUAGE GADTs, KindSignatures #-}
module System.Console.Wizard.Internal ( WizardAction (..)
                                      , PromptString (..)
                                      -- $backend
                                      ) where

type PromptString = String 

-- | Internally, a 'Wizard' is essentially a prompt monad with a 'WizardAction'. A constructor exists for each primitive action, as well
--   as a special \"escape hatch\" constructor ('Backend') used for writing backend-specific primitives and modifiers.
--   Each back-end has a corresponding data type, used as a type parameter for 'Wizard'. This data type is usually opaque, but internally
--   specifies additional primitive actions that are specific to the back-end.
--   'WizardAction' is parameterised by this data type (for use in the 'Backend' constructor), the prompt monad itself (so that modifiers
--   can be made as well as primitives) and the return type of the action.
data WizardAction :: ((* -> *) -> * -> *) -> (* -> *) -> * -> * where
    Line       :: PromptString -> WizardAction b m String
    LinePreset :: PromptString -> String -> String -> WizardAction b m String
    Password   :: PromptString -> Maybe Char -> WizardAction b m String
    Character  :: PromptString -> WizardAction b m Char
    Output     :: String       -> WizardAction b m ()
    OutputLn   :: String       -> WizardAction b m ()    
    Backend    :: b m a        -> WizardAction b m a
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