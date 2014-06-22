{-# LANGUAGE DeriveGeneric #-}
module Phonebook where
--

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
