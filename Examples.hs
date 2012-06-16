import System.Console.Wizard
import System.Console.Wizard.Haskeline
import System.Console.Haskeline
import System.Console.Wizard.Pure
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Monoid

-- Examples from the readme. Used to see if they actually work
passwordW :: String -> Wizard b ()
passwordW realPassword = 
  let 
    w = do validator (== realPassword) $ password "Enter password: " (Just '*') 
           outputLn "The secret is 42"
  in w <|> w <|> w <|> (outputLn "Password rejected. Goodbye!")

passwordW2 :: String -> Wizard b ()
passwordW2 realPassword = (retryMsg "Incorrect password." 
                        $ validator (== realPassword) 
                        $ password "Enter password: " (Just '*'))
                       >> outputLn "The secret is 42"

type Name = String
type Class = Int 
data Student = Student Name Class deriving Show

nameWizard = retry $ nonEmpty $ line "Name: "
classWizard = retry 
            $ inRange (1,5) 
            $ parseRead 
            $ nonEmpty (line "Class[1]: ") `defaultTo` "1"

studentWizard = Student <$> nameWizard <*> classWizard

parseSticks :: String -> Maybe Int
parseSticks [] = Just 0
parseSticks ('|':r) = (+1) <$> parseSticks r
parseSticks (_:_) = Nothing

sticksW = (do s <- parser parseSticks (line "Enter sticks!: ")
              outputLn $ "I found " ++ show s ++ " sticks!")
         <|> outputLn "I found something that wasn't a stick and got confused."

missilesW :: Wizard Haskeline ()
missilesW = do retryMsg "" $ validator (== 'x') $ character "Press 'X' to fire the missiles"
               liftIO $ fireTheMissiles               
  where fireTheMissiles = putStrLn "MISSILES AWAY!"
  
specialHistory = withSettings (defaultSettings {historyFile = Just "histfile"})
               $ line "Answers to this question are recorded in histfile" >>= output  

menuExample :: Wizard b ()
menuExample = menu "Choose a choice: " $ choice "First choice"  (outputLn "You chose the first choice")
                                      <> choice "Second choice" (outputLn "You chose the second choice")

  
main = runInputT defaultSettings $ runHaskeline $ passwordW "rosebud"