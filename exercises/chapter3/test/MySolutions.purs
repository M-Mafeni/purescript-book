module Test.MySolutions where

import Data.AddressBook
import Prelude

import Data.List (filter, head, null, nubByEq)
import Data.Maybe (Maybe)
import Prim.Boolean (True)

-- Note to reader: Add your solutions to this file
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street  = head <<< filter (\entry -> _.address.street entry == street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName  = not <<< null <<< filter (\entry -> entry.firstName == firstName && entry.lastName == lastName)

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq (\e1 e2 -> e1.firstName == e2.firstName 
                                      && e1.lastName == e2.lastName)