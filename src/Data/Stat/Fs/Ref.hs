module Data.Stat.Fs.Ref(
  Ref, newRef, writeRef, readRef, modifyRef, rmRef
) where

import Data.Text (Text)

import Data.Stat.Fs.Kv
import Data.Stat.Fs.PrimVal

data Ref a = Ref Text Kv

-- | Creates a new field in the KV-store
newRef :: IsPrimVal a => Kv -> Text -> a -> IO (Ref a)
newRef kv name initVal = do
  writeRef ref initVal
  return ref
  where ref = Ref name kv

-- | Writes value to the store by reference.
writeRef :: IsPrimVal a => Ref a -> a -> IO ()
writeRef (Ref name kv) value = writeVal kv name (toPrimVal value)

-- | Reads the value by reference.
readRef :: IsPrimVal a => Ref a -> IO a
readRef (Ref name kv) = fmap fromPrimVal $ readVal kv name

-- | Modifies the value by reference
modifyRef :: IsPrimVal a => Ref a -> (a -> a) -> IO ()
modifyRef (Ref name kv) f = modifyVal kv name (toPrimVal . f . fromPrimVal)

-- | Clears the field from the store.
rmRef :: Ref a -> IO ()
rmRef (Ref name kv) = rmVal kv name