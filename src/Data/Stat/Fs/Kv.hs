module Data.Stat.Fs.Kv(
    Kv, newKv
  , writeVal, readVal, modifyVal, rmVal
  , writeKv
  , writeKvLoop
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Thread.Delay
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad

import Data.Map.Strict (Map)
import Data.String
import Data.Text (Text)
import Data.Time
import GHC.IO.Handle
import System.FilePath
import System.IO.Temp

import Data.Yaml(ToJSON(..), object, (.=))
import Data.Yaml.Pretty

import qualified Control.Immortal as Immortal
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Yaml as Y
import qualified Shelly as S


import Data.Stat.Fs.PrimVal

-- | Key-value list of metrics.
newtype Kv = Kv { unKv :: TVar (Map Text PrimVal) }

newKv :: IO Kv
newKv = fmap Kv $ newTVarIO M.empty

writeVal :: Kv -> Text -> PrimVal -> IO ()
writeVal (Kv tv) name value = atomically $ modifyTVar tv (M.insert name value)

readVal :: Kv -> Text -> IO PrimVal
readVal (Kv tv) name = fmap (M.! name) $ readTVarIO tv

modifyVal :: Kv -> Text -> (PrimVal -> PrimVal) -> IO ()
modifyVal (Kv tv) name f = atomically $ modifyTVar tv $ M.adjust f name

rmVal :: Kv -> Text -> IO ()
rmVal (Kv tv) name = atomically $ modifyTVar tv $ M.delete name

kvToJSON :: Kv -> IO Y.Value
kvToJSON (Kv tvValues) = fmap render $ readTVarIO tvValues
  where
    render = object . fmap (\(name, value) -> name .= toJSON value) . M.toList

-- | Saves the metrics to the given file.
--
-- Under the hood it first generates a temporary file
-- (it's the name of the original file plus .tmp at the end)
-- and then moves it to the specified file path.
writeKv :: FilePath -> Kv -> IO ()
writeKv file kv = withTempFile (takeDirectory file) (takeFileName file) $ \fileName handle -> do
  B.hPut handle =<< res
  hClose handle
  S.shelly $ S.mv (fromString fileName) (fromString file)
  where
    res = fmap (encodePretty defConfig) $ kvToJSON kv


-- | Creates a process that writes stats with given period.
writeKvLoop :: NominalDiffTime -> FilePath -> Kv -> IO ()
writeKvLoop dt file kv =
  void . Immortal.createWithLabel ("stat save: " ++ file) $ const $ do
    forever $ do
      writeKv file kv
      sleep dt

-- | Stop the thread for some time.
sleep :: NominalDiffTime -> IO ()
sleep dt = delay $ toMicroseconds dt

-- | Convert time to microseconds
toMicroseconds :: NominalDiffTime -> Integer
toMicroseconds t = ceiling $ toRational t * 1000000


