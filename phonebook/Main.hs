{-# LANGUAGE DeriveGeneric #-}

import Control.Applicative

import Data.Aeson
import qualified Data.ByteString.Lazy as BS

import Phonebook

import System.Environment (getArgs)
import System.Exit (exitFailure)

readDB dbPath = eitherDecode <$> BS.readFile dbPath :: IO (Either String Phonebook)

writeDB dbPath pb = BS.writeFile dbPath $ encode pb

main = do
    args <- getArgs

    case length args of
      2 -> if cmd == "create"
                then writeDB pbName pbCreate
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
                         Right pb -> case (pbRemove arg pb) of
                                       Left err -> putStrLn err
                                       Right pb' -> writeDB pbName pb'

             otherwise -> putStrLn "Unknown command or missing/extra arguments"
          where (cmd:arg:pbName:[]) = args

      4 -> case cmd of
             "add" -> do
                       pb <- readDB pbName
                       case pb of
                         Left err -> putStrLn err
                         Right pb -> case (pbAdd (Contact name phone) pb) of
                                       Left err -> putStrLn err
                                       Right pb' -> writeDB pbName pb'

             "change" -> do
                       pb <- readDB pbName
                       case pb of
                         Left err -> putStrLn err
                         Right pb -> case (pbChange (Contact name phone) pb) of
                                       Left err -> putStrLn err
                                       Right pb' -> writeDB pbName pb'

             otherwise -> putStrLn "Unknown command or missing/extra arguments"
           where (cmd:name:phone:pbName:[]) = args

      otherwise -> putStrLn "Unknown command or missing/extra arguments"
