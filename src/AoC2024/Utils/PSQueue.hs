module AoC2024.Utils.PSQueue where

import Prelude hiding (null)
import qualified Prelude
import Data.Maybe (fromJust)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.PSQueue as PSQ

data PSQueue k p a
  = PSQueue (PSQ.PSQ k p) (Map k a)

null :: PSQueue k p a -> Bool
null (PSQueue _ table) = Prelude.null table

singleton :: k -> p -> a -> PSQueue k p a
singleton key prio info =
  PSQueue (PSQ.singleton key prio) (Map.singleton key info)

peek :: Ord k => PSQueue k p a -> (k, p, a)
peek (PSQueue psq table) = let
  key PSQ.:-> prio = fromJust (PSQ.findMin psq)
  info = table ! key
  in (key, prio, info)

delete :: (Ord k, Ord p) => k -> PSQueue k p a -> PSQueue k p a
delete key (PSQueue psq table) =
  PSQueue (PSQ.delete key psq) (Map.delete key table)

insertWith :: (Ord k, Ord p) => (a -> a -> a) -> k -> p -> a -> PSQueue k p a -> PSQueue k p a
insertWith f key prio info (PSQueue psq table) = 
  case PSQ.lookup key psq of
    Nothing -> PSQueue (PSQ.insert key prio psq) (Map.insert key info table)
    Just oldPrio ->
      case compare prio oldPrio of
        LT -> PSQueue (PSQ.insert key prio psq) (Map.insert key info table)
        EQ -> PSQueue psq (Map.insertWith f key info table)
        GT -> PSQueue psq table