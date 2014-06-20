{-# LANGUAGE DeriveGeneric #-}

module Phonebook where

{--

Build a command line tool that manages phone books, via the interface below.

The goal isn't necessarily to finish (though finishing is ideal), it's
just to make progress and be able to talk about the choices you made
while building.


$ phonebook create hsphonebook.pb
or
$ python phonebook.py create hsphonebook.pb
created phonebook hsphonebook.pb in the current directory

$ phonebook lookup Sarah hsphonebook.pb # error message on no such phonebook
Sarah Ahmed 432 123 4321
Sarah Apple 509 123 4567
Sarah Orange 123 456 7890

$ phonebook add 'John Michael' '123 456 4323' hsphonebook.pb # error message on duplicate name

$ phonebook change 'John Michael' '234 521 2332' hsphonebook.pb # error message on not exist

$ phonebook remove 'John Michael' hsphonebook.pb # error message on not exist

$ phonebook reverse-lookup '312 432 5432' hsphonebook.pb

You should also write tests.

--}

import Control.Applicative

import Data.Aeson
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS

import GHC.Generics

import System.Directory
import System.Environment
import System.Exit
import System.IO

-- A contact
data Contact = Contact {
    name :: String,
    phone :: String
} deriving (Show, Generic)

instance FromJSON Contact
instance ToJSON Contact

-- A Phonebook
data Phonebook = Phonebook {
    contacts :: [Contact]
} deriving (Show, Generic)

instance FromJSON Phonebook
instance ToJSON Phonebook

-- fixme: They should really be a command interface, I think...
commandMap = M.fromList [("add", add), ("create", create), ("change", change), ("lookup", myLookup), ("remove", remove), ("reverseLookup", reverseLookup)]

add :: [String] -> IO ()
add (name:phone:dbPath:[]) = writeDB dbPath (addContact <$> (readDB dbPath) <*> (Contact name phone))
add _ = undefined

create :: [String] -> IO ()
create (x:[]) = writeDB x (Phonebook [])
create _ = undefined

change :: [String] -> IO ()
change _ = putStrLn "Not implemented!"

remove :: [String] -> IO ()
remove _ = putStrLn "Not implemented!"

myLookup :: [String] -> IO ()
myLookup _ = putStrLn "Not implemented!"

reverseLookup :: [String] -> IO ()
reverseLookup _ = putStrLn "Not implemented!"

-- Helper functions --

readDB :: FilePath ->  IO (Either String Phonebook)
readDB dbPath = fmap eitherDecode (BS.readFile dbPath) :: IO (Either String Phonebook)

writeDB :: FilePath -> Phonebook -> IO ()
writeDB dbPath phonebook = BS.writeFile dbPath $ encode $ phonebook

performCommand :: [String] -> IO ()
performCommand [] = putStrLn "Need a command argument."
performCommand (x:xs) = if M.member x commandMap then (y xs) else putStrLn "Unknown command!"
    where (Just y) = M.lookup x commandMap

addContact :: Phonebook -> Contact -> Phonebook
addContact (Phonebook pb) contact = Phonebook (contact:pb)

------------------------------------------------------------------------

main = do
    args <- getArgs

    if length args < 2
        then do
            putStrLn "Need atleast two arguments: <command-name> <phone-book>"
            exitFailure
        else performCommand args
