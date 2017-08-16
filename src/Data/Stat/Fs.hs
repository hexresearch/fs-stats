-- | Saves the stats on file system in the YAML format.
module Data.Stat.Fs(
  -- * Key-value store
    Kv, newKv
  -- ** Store IO
  , writeKv
  , writeKvLoop
  -- ** Modify store
  , IsPrimVal
  , Ref, newRef, writeRef, readRef, modifyRef, rmRef
) where

import Data.Stat.Fs.Kv
import Data.Stat.Fs.Ref
import Data.Stat.Fs.PrimVal