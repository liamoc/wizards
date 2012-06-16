# Wizards

`wizards` is an Haskell library designed for the quick and painless development of *interrogative* programs, which revolve around a "dialogue" with the user, who is asked a series of questions in a sequence much like an installation wizard.

Everything from interactive system scripts, to installation wizards, to full-blown shells can be implemented with the support of `wizards`.

It is developed transparently on top of a `Prompt` monad, which separates out the semantics of the program from any particular interface. A variety of backends exist, including a full featured backend for Haskeline, a debug-friendly simpler implementation in terms of `System.IO`
primitives, and a completely pure implementation modelled as a function from an input string to output. It is also possible to write your 
own backends. 

While both built-in IO backends operate on a console, there is no reason why `wizards` cannot also be used for making GUI wizard interfaces.

Below are installation instructions and some educational examples.

Information on how to write backends, as well as structured API documentation is available on Hackage:

http://hackage.haskell.org/package/wizards

(Or, you can just run `cabal haddock` to generate the documentation from the source).

## Installing

To install with cabal from hackage, just go:

```
cabal install wizards
```

Otherwise, to install from source:

```
git clone https://github.com/liamoc/wizards.git
cd wizards
runhaskell Setup.hs configure
runhaskell Setup.hs build
runhaskell Setup.hs install
```

Or, if you have cabal, you can replace `runhaskell Setup.hs` with `cabal` there.

## Examples!

Below are a series of educational examples. If you want more structured documentation, please refer to the API documentation on Hackage (or generate it with `cabal haddock`).

```haskell
import System.Console.Haskeline
import System.Console.Wizard
import System.Console.Wizard.Haskeline --
import System.Console.Wizard.BasicIO   -- choose a backend, Haskeline recommended.
import System.Console.Wizard.Pure      --
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
```
### Student Records

This example demonstrates use of the `Applicative` instance to build up data structures, `retry`, `inRange`, `defaultTo`, and `parseRead`.


Suppose we have a `Student` data type, that contains a name and a class number (which we shall say must be in the interval [1,5]).

```haskell
type Name = String
type Class = Int 
data Student = Student Name Class
```

A `Name` must be a non-empty string. If the user enters an empty string, we will prompt them again:

```haskell
nameWizard = retry $ nonEmpty $ line "Name: "
```

A `Class` must be between 1 and 5. If the user enters nothing, we will default to 1. If they enter an invalid string, they will be prompted again:

```haskell
classWizard = retry 
            $ inRange (1,5) 
            $ parseRead 
            $ nonEmpty (line "Class[1]: ") `defaultTo` "1"
```

We can now populate a `Student` data type using the `Applicative` instance of `Wizard b`.

```haskell
studentWizard = Student <$> nameWizard <*> classWizard
```

And run our wizard with the Haskeline back-end:

```haskell
main = runInputT defaultSettings (runHaskeline studentWizard) 
   >>= print
```

### Passwords

This example demonstrates masked input, failure (using `Alternative`), and `retryMsg`, as well as simple use of `validator` and `outputLn`.

Ask for a password three times, then fail:

```haskell
passwordW :: String -> Wizard b ()
passwordW realPassword = 
  let 
    w = do validator (== realPassword) $ password "Enter password: " (Just '*') 
           outputLn "The secret is 42"
  in w <|> w <|> w <|> outputLn "Password rejected. Goodbye!"

main = runInputT defaultSettings $ runHaskeline $ passwordW "rosebud"

```

Here we use `validator` to check if the user has entered the correct password, and, if so, print out a secret message.

To run using the `BasicIO` backend instead, change `main` to:

```haskell
main = runBasicIO passwordW "rosebud"
```

Or, for unlimited tries, we can use the `retryMsg` function (or just `retry`):

```haskell

passwordW :: String -> Wizard b ()
passwordW realPassword = (retryMsg "Incorrect password." 
                       $ validator (== realPassword) 
                       $ password "Enter password: " (Just '*'))
                      >> outputLn "The secret is 42"

main = runInputT defaultSettings $ runHaskeline $ passwordW "rosebud"
          
```


### Counting sticks (custom parsers)

This example demonstrates using custom parse functions.

Suppose we have a parser that picks up sticks:

```haskell
parseSticks :: String -> Maybe Int
parseSticks [] = Just 0
parseSticks ('|':r) = fmap (+1) $ parseSticks r
parseSticks (_:_) = Nothing
```

We can equip a wizard with this parser using the `parser` modifier:

```haskell
sticksW = (do s <- parser parseSticks (line "Enter sticks!: ")
              outputLn $ "I found " ++ show s ++ " sticks!")
          <|> outputLn "I found something that wasn't a stick and got confused."
          
main = runInputT defaultSettings $ runHaskeline $ sticksW          
```

This will run the parseSticks parser on the user input, and, if it succeeds, output the number of sticks parsed. If it fails, it will output an error message.

### Menus 

Menus are constructed with `choice`, composed with `mappend` or `(<>)`, and made into a `Wizard` with `menu`.

```haskell
menuExample :: Wizard b ()
menuExample = menu "Choose a choice: " $ choice "First choice"  (outputLn "You chose the first choice")
                                      <> choice "Second choice" (outputLn "You chose the second choice")

main = runInputT defaultSettings $ runHaskeline $ menuExample                                      
```

## Backend-specific features

The Haskeline and BasicIO backend also supports embedding arbitrary IO actions inside wizards through a `MonadIO` instance. For example:

```haskell
missilesW :: Wizard Haskeline ()
missilesW = do retryMsg "" $ validator (== 'x') $ character "Press 'X' to fire the missiles"
               liftIO $ fireTheMissiles
```

Note that the type signature is necessary here. Otherwise GHC will infer `Wizard b ()` and (rightly) not be able to deduce `MonadIO (Wizard b)`. 
               
This is made backend-specific to allow a pure backend of some form to be developed in the future.

Another backend-specific feature unique to the Haskeline backend allows setting Haskeline settings through a wizard modifier, for example:

```haskell
specialHistory = withSettings (defaultSettings {historyFile = Just "histfile"})
               $ line "Answers to this question are recorded in histfile" >>= output
```
