module System.Console.Wizard.Internal.SimpleMenu (simpleMenu) where

import System.Console.Wizard
import System.Console.Wizard.Internal
import Control.Monad
    
-- | A useful function for implementing simple menu systems on top of existing backend features.
--   Used by Pure, BasicIO and Haskeline to implement their simple menu systems. 
simpleMenu :: PromptString 
           -> [(String, v)] 
           -> Wizard b v
simpleMenu pr items = fmap (map snd items !!) 
                    $ inRange (0, length items - 1) 
                    $ parseRead 
                    $ line 
                    $ unlines 
                    $ (map promptString $ zip [0..] $ map fst items) ++ [pr]
  where promptString (n, str) = show n ++ ". " ++ str                         
