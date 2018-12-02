{-# LANGUAGE RecordWildCards #-}
module Problem2 (DuplicateCount(..), updateDuplicateCount, checksum)
where

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
