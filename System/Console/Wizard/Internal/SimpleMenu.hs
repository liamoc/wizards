module System.Console.Wizard.Internal.SimpleMenu where

import System.Console.Wizard
import Control.Monad
    
simpleMenu :: String -> [(String, v)] -> Bool -> Wizard b v
simpleMenu pr items out = do when out $ output $ unlines $ map promptString $ zip [0..] $ map fst items
                             fmap (map snd items !!) $ inRange (0, length items - 1) $ parseRead $ line pr
  where promptString (n, str) = show n ++ ". " ++ str                         
