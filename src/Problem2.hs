{-# LANGUAGE RecordWildCards #-}
module Problem2
  ( DuplicateCount(..)
  , updateDuplicateCount
  , checksum
  , neighbor
  , findFirstNeighbor
  )
where

import Data.Maybe
import qualified Data.Map as M

data DuplicateCount = DuplicateCount
  { exactlyTwo :: Int
  , exactlyThree :: Int
  }

data DuplicateUpdate = DuplicateUpdate
  { hasExcactlyTwo :: Bool
  , hasExactlyThree :: Bool
  }

type CountMap = M.Map Char Int

countDups :: String -> CountMap
countDups "" = M.empty
countDups (c:s) = M.insertWith (+) c 1 $ countDups s

makeUpdate :: CountMap -> DuplicateUpdate
makeUpdate = M.foldl updateWithCount (DuplicateUpdate False False) where
  updateWithCount DuplicateUpdate{..} c = DuplicateUpdate
    { hasExcactlyTwo = hasExcactlyTwo || c == 2
    , hasExactlyThree = hasExactlyThree || c == 3
    }


applyUpdate :: DuplicateUpdate -> DuplicateCount -> DuplicateCount
applyUpdate DuplicateUpdate{..} c@DuplicateCount{..} =
  c { exactlyTwo = if hasExcactlyTwo then exactlyTwo + 1 else exactlyTwo
    , exactlyThree = if hasExactlyThree then exactlyThree + 1 else exactlyThree}

updateDuplicateCount :: String -> DuplicateCount -> DuplicateCount
updateDuplicateCount s = applyUpdate (makeUpdate $ countDups s)

checksum :: DuplicateCount -> Int
checksum DuplicateCount{..} = exactlyTwo * exactlyThree

neighbor :: String -> String -> Maybe String
neighbor = go False id where
  go diff prefix (c1:s1) (c2:s2)
    | c1 == c2 = go diff (prefix . (c1:)) s1 s2
    | otherwise =
        if diff
        then Nothing
        else go True prefix s1 s2
  go True prefix "" "" = Just $ prefix ""
  go _    _      _  _  = Nothing

getNeighbor :: String -> [String] -> Maybe String
getNeighbor s = foldMap (neighbor s)

-- quadratic algorithm because I was too lazy to think of anything better
findFirstNeighbor :: [String] -> String
findFirstNeighbor = fromJust . go where
  go (s:ss) = getNeighbor s ss <> go ss
  go [] = Nothing
