{-# LANGUAGE OverlappingInstances, TypeOperators, FlexibleContexts #-}  
import System.Console.Haskeline  
import System.Console.Wizard  
import System.Console.Wizard.Haskeline --  
import System.Console.Wizard.BasicIO   -- choose a backend, Haskeline recommended.  
import System.Console.Wizard.Pure      --  
import Control.Applicative  
import Control.Monad  
import Control.Monad.Trans  
import Data.Monoid

type Name = String  
type Class = Int   
data Student = Student Name Class deriving (Show)

nameWizard :: (Line :<: b) => Wizard b Name
nameWizard = retry $ nonEmpty $ line "Name: "

classWizard :: (Line :<: b) => Wizard b Class
classWizard = retry 
            $ inRange (1,5) 
            $ parseRead 
            $ nonEmpty (line "Class[1]: ") `defaultTo` "1"

studentWizard :: (Line :<: b) => Wizard b Student                        
studentWizard = Student <$> nameWizard <*> classWizard

main22 :: IO ()
main22 = (run $ basicIO $ studentWizard) >>= print  


main12 :: IO ()
main12 = runInputT defaultSettings (run $ haskeline $ studentWizard)   
   >>= print  


   
passwordW :: (Password :<: b, OutputLn :<: b) => String -> Wizard b ()  
passwordW realPassword =   
  let 
    w = do validator (== realPassword) $ password "Enter password: " (Just '*') 
           outputLn "The secret is 42"  
  in w <|> w <|> w <|> outputLn "Password rejected. Goodbye!"  

main1 :: IO ()
main1 = void $  runInputT defaultSettings $ run $ haskeline $ passwordW "rosebud"


passwordW2 :: (Password :<: b, OutputLn :<: b) => String -> Wizard b ()  
passwordW2 realPassword = (retryMsg "Incorrect password." 
                       $ validator (== realPassword) 
                       $ password "Enter password: " (Just '*'))
                      >> outputLn "The secret is 42"  

parseSticks :: String -> Maybe Int  
parseSticks [] = Just 0  
parseSticks ('|':r) = fmap (+1) $ parseSticks r  
parseSticks (_:_) = Nothing

sticksW = (do s <- parser parseSticks (line "Enter sticks!: ")
              outputLn $ "I found " ++ show s ++ " sticks!")
          <|> outputLn "I found something that wasn't a stick and got confused."

main3 :: IO ()
main3 = void $ runInputT defaultSettings $ run $ haskeline $ sticksW      

missilesW :: (ArbitraryIO :<: b, Character :<: b) => Wizard b ()  
missilesW = do retry $ validator (== 'x') $ character "Press 'X' to fire the missiles"
               liftIO $ fireTheMissiles
 where fireTheMissiles = putStrLn "FIRE!"              

specialHistory :: (WithSettings :<: b, Line :<: b, Output :<: b) => Wizard b ()
specialHistory = withSettings (defaultSettings {historyFile = Just "histfile"})
               $ line "Answers to this question are recorded in histfile" >>= output               
   
   