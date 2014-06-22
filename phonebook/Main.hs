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
import qualified Data.ByteString.Lazy as BS

import Phonebook

import System.Directory
import System.Environment (getArgs)
import System.Exit (exitFailure)

readDB :: FilePath -> IO (Either String Phonebook)
readDB dbPath = eitherDecode <$> BS.readFile dbPath :: IO (Either String Phonebook)

writeDB' :: FilePath -> (Either String Phonebook) -> IO ()
writeDB' dbPath pb = case pb of
                       Left msg -> putStrLn msg
                       Right pb' -> BS.writeFile dbPath $ encode pb'

main = do
    args <- getArgs

    case length args of
      2 -> if cmd == "create"
                then do
                  exists <- doesFileExist pbName
                  writeDB' pbName (if exists then Left "Phonebook already exists." else Right pbCreate)
                else putStrLn "Unknown command or missing/extra arguments"
                where (cmd:pbName:[]) = args

      3 -> case cmd of
             "lookup" -> do
                       pb <- readDB pbName
                       case pb of
                         Left err -> putStrLn err
                         Right pb -> mapM_ (putStrLn . show) (pbLookup arg pb)

             "reverse-lookup" -> do
                       pb <- readDB pbName
                       case pb of
                         Left err -> putStrLn err
                         Right pb -> mapM_ (putStrLn . show) (pbReverseLookup arg pb)

             "remove" -> do
                       pb <- readDB pbName
                       case pb of
                         Left err -> putStrLn err
                         Right pb -> writeDB' pbName (pbRemove arg pb)

             otherwise -> putStrLn "Unknown command or missing/extra arguments"
          where (cmd:arg:pbName:[]) = args

      4 -> case cmd of
             "add" -> do
                       pb <- readDB pbName
                       case pb of
                         Left err -> putStrLn err
                         Right pb -> writeDB' pbName (pbAdd (Contact name phone) pb)

             "change" -> do
                       pb <- readDB pbName
                       case pb of
                         Left err -> putStrLn err
                         Right pb -> writeDB' pbName (pbChange (Contact name phone) pb)

             otherwise -> putStrLn "Unknown command or missing/extra arguments"
           where (cmd:name:phone:pbName:[]) = args

      otherwise -> putStrLn "Unknown command or missing/extra arguments"
