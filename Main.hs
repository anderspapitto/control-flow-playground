{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Free
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Maybe
import Reflex
import Reflex.Dom hiding (attributes)
import Text.Read (readMaybe)

-- In the rest of the code we'll be making a modified version of this
-- function. Here it is in direct, IO-based style.
demoIO :: IO ()
demoIO = do
  i <- getUserInputIO "Please choose 0 or some other number."
  void $ if i == 0
    then do
      a <- getUserInputIO "You chose 0. I'm going to sum two numbers for you.\nPlease enter the first of two numbers."
      b <- getUserInputIO "Please enter the second of two numbers."
      getUserInputIO $ "Those numbers sum to " ++ show (a + b) ++ ". Enter some dummy number to finish."
    else do
      a <- getUserInputIO "You chose some other number. I'm going to square a number for you.\nPlease enter a number."
      getUserInputIO $ "The square of that number is " ++ show (a * a) ++ ". Enter some dummy number to finish."

-- used only in demoIO, above
getUserInputIO :: String -> IO Int
getUserInputIO prompt = do
  putStrLn prompt
  x <- getLine
  case readMaybe x of
   Just n -> return n
   Nothing -> putStrLn "That doesn't look like an int from here. Try again." >> getUserInputIO prompt


-- The first chunck of code here is based off of
-- http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html

-- First we define a monad with an action to retrieve user input by
-- providing a prompt.

data ArstF x
  = GetUserInput String (Int -> x)

instance Functor ArstF where
  fmap f (GetUserInput s k) = GetUserInput s (f . k)

type Arst = Free ArstF

getUserInput :: String -> Arst Int
getUserInput s = liftF $ GetUserInput s id

-- We can now write a demo program that uses this monad. Depending on
-- what the user supplies, it asks for different amounts of further
-- input. It looks the same as the function at the top of the file,
-- but using getUserInput in place of getUserInputIO, and in our new
-- monad.
demo :: Arst ()
demo = do
  i <- getUserInput "Please choose 0 or some other number."
  void $ if i == 0
    then do
      a <- getUserInput "You chose 0. I'm going to sum two numbers for you.\nPlease enter the first of two numbers."
      b <- getUserInput "Please enter the second of two numbers."
      getUserInput $ "Those numbers sum to " ++ show (a + b) ++ ". Enter some dummy number to finish."
    else do
      a <- getUserInput "You chose some other number. I'm going to square a number for you.\nPlease enter a number."
      getUserInput $ "The square of that number is " ++ show (a * a) ++ ". Enter some dummy number to finish."

-- Now, we provide a function that runs a program in Arst as far as it
-- can go with a single unit of input.  The type could be a little
-- cleaner, but it has to match (a -> b -> b) for Reflex's foldDyn.
step :: Int -> (String, Maybe (Int -> Arst r)) -> (String, Maybe (Int -> Arst r))
step _ (_, Nothing)                  = ("no more input needed", Nothing)
step n (_, Just f) = case f n of
  Pure r                            -> ("no more input needed", Nothing)
  Free (GetUserInput s f')          -> (s, Just f')

-- We have to manually unwrap the outermost GetUserInput before
-- handing our demo off to reflect
prepped_demo = case demo of Free (GetUserInput s f) -> (s, Just f)

-- Now we have some Reflex code to wrap our demo in a minimal web page
getUserSelections :: MonadWidget t m => Dynamic t String -> m (Event t Int)
getUserSelections prompts = do
  el "div" $ dynText prompts
  inp <- el "div" $ textInput $ def & textInputConfig_inputType .~ "number"
  return $ attachDynWithMaybe (\a b -> readMaybe a) (inp ^. textInput_value) (textInputGetEnter inp)

stepDemo :: (Reflex t, MonadHold t m, MonadFix m) => Event t Int -> m (Dynamic t (String, Maybe (Int -> Arst ())))
stepDemo selections = foldDyn step prepped_demo selections

reflexMain = mainWidget $ el "div" $ do
  el "p" $ text "Welcome to Demo"
  rec
    selections  <- getUserSelections prompts
    steppedDemo <- stepDemo selections
    prompts     <- mapDyn fst steppedDemo
  return ()

main = reflexMain
-- main = demoIO
