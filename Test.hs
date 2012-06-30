{-# LANGUAGE OverlappingInstances, TypeOperators, FlexibleContexts #-}  
import System.Console.Wizard  
import System.Console.Wizard.BasicIO
import System.Console.Wizard.Shim
import Control.Applicative  
import Control.Monad  
import Control.Monad.Trans  
import Data.Monoid

passwordW :: (Password :<: b, OutputLn :<: b) => String -> Wizard b ()  
passwordW realPassword =   
  let 
    w = do validator (== realPassword) $ password "Enter password: " (Just '*') 
           outputLn "The secret is 42"  
  in w <|> w <|> w <|> outputLn "Password rejected. Goodbye!"  

main = void $ run $ (passwordW "rosebud" :: Wizard (Password :+: BasicIO) ())