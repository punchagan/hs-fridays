{-# LANGUAGE DeriveGeneric #-}
module Phonebook where
--
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
import Data.List

import GHC.Generics

-- A contact
data Contact = Contact {
    name :: String,
    phone :: String
} deriving (Generic, Eq)

instance Show Contact where
  show (Contact name phone) = name ++ " " ++ phone

instance FromJSON Contact
instance ToJSON Contact

-- A Phonebook
data Phonebook = Phonebook {
    contacts :: [Contact]
} deriving (Show, Generic, Eq)

instance FromJSON Phonebook
instance ToJSON Phonebook

pbCreate :: Phonebook
-- fixme: check if pb already exists and raise error!
pbCreate = Phonebook []


pbAdd :: Contact -> Phonebook -> Either String Phonebook
pbAdd contact pb@(Phonebook contacts) = if M.notMember (name contact) phonemap
                                      then Right $ Phonebook $ contact:contacts
                                      else Left $ "Contact already exists: " ++ (name contact)
                                          where phonemap = toMap pb

pbChange :: Contact -> Phonebook -> Either String Phonebook
pbChange contact pb = case (pbRemove (name contact) pb) of
                        (Left msg) -> Left msg
                        (Right pb') -> pbAdd contact pb'


pbRemove :: String -> Phonebook -> Either String Phonebook
pbRemove name pb@(Phonebook contacts) = if M.notMember name phonemap
                                        then Left $ "Unknown contact: " ++ name
                                        else Right $ fromMap $ M.delete name phonemap
    where phonemap = toMap pb

pbLookup :: String -> Phonebook -> [Contact]
pbLookup query (Phonebook contacts) = foldr (startsWith query) [] contacts
    where startsWith query contact contacts' = if isInfixOf query (name contact)
                                               then contact:contacts'
                                               else contacts'

pbReverseLookup :: String -> Phonebook -> [Contact]
pbReverseLookup query (Phonebook contacts) = foldr (numberMatches query) [] contacts
    where numberMatches query contact contacts' = if query == (phone contact)
                                                  then contact:contacts'
                                                  else contacts'

-- Helper functions
-- fixme: use functors and applicatives?
toMap phonebook = M.fromList $ toList phonebook where
    toList (Phonebook []) = []
    toList (Phonebook (contact:contacts)) = (toPair contact):(toList $ Phonebook contacts) where
                                                toPair (Contact n p) = (n, p)

fromMap m = fromList $ M.toList m where
    fromList [] = Phonebook []
    fromList ((name, phone):xs) = Phonebook $ (Contact name phone):(map fromPair xs) where
                                  fromPair (n, p) = Contact n p
