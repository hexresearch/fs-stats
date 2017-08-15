-- | Saves the stats on file system in the YAML format.
module Data.Stat.Fs(
    Kv(..)
  , Metric(..)
  , PrimVal(..)
  , writeStat
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Thread.Delay
import Control.Concurrent.STM.TVar
import Control.Monad

import Data.Boolean
import Data.Data
import Data.Scientific
import Data.String
import Data.Text (Text)
import Data.Time
import Data.Yaml(ToJSON(..), object, (.=))
import Data.Yaml.Pretty
import GHC.Generics

import qualified Control.Immortal as Immortal
import qualified Data.ByteString as B
import qualified Data.Yaml as Y
import qualified Shelly as S


-- | Key-value list of metrics.
newtype Kv = Kv { unKv :: [Metric] }
  deriving (Show, Eq, Generic, Data)

-- | A single metric:
--
-- > name: value
data Metric = Metric
  { metricName :: Text
  , metricData :: PrimVal
  } deriving (Show, Eq, Generic, Data)

-- | Primitive value: text, number or boolean
data PrimVal
  = TextVal !Text
  | NumberVal !Scientific
  | BoolVal !Bool
  deriving (Show, Eq, Generic, Data)

instance ToJSON Kv where
  toJSON (Kv metrics) = object $ fmap (\(Metric name value) -> name .= toJSON value) metrics

instance ToJSON PrimVal where
  toJSON x = case x of
    TextVal t -> Y.String t
    NumberVal n -> Y.Number n
    BoolVal b -> Y.Bool b

-- | Saves the metrics to the given file.
--
-- Under the hood it first generates a temporary file
-- (it's the name of the original file plus .tmp at the end)
-- and then moves it to the specified file path.
writeStat :: FilePath -> Kv -> IO ()
writeStat file kv = do
  B.writeFile tmpFile res
  S.shelly $ S.mv (fromString tmpFile) (fromString file)
  where
    res = encodePretty defConfig kv
    tmpFile = tmp file

-- | Creates a process that writes stats with given period.
writeStatLoop :: NominalDiffTime -> FilePath -> TVar Kv -> IO ()
writeStatLoop dt file tvStat =
  void . Immortal.createWithLabel ("stat save: " ++ file) $ const $ do
    forever $ do
      writeStat file =<< readTVarIO tvStat
      sleep dt

-- | Creates a temporary file name.
tmp :: FilePath -> FilePath
tmp x = x ++ ".tmp"

-- | Stop the thread for some time.
sleep :: NominalDiffTime -> IO ()
sleep dt = delay $ toMicroseconds dt

-- | Convert time to microseconds
toMicroseconds :: NominalDiffTime -> Integer
toMicroseconds t = ceiling $ toRational t * 1000000

------------------------------------
-- prim val instances (for convinience)
--
-- String, Numeric, Boolean

instance IsString PrimVal where
  fromString = TextVal . fromString

instance Num PrimVal where
  fromInteger = NumberVal . fromInteger
  (+) = onNum2 (+)
  (*) = onNum2 (*)
  negate = onNum1 negate
  abs = onNum1 abs
  signum = onNum1 signum

instance Fractional PrimVal where
  fromRational = NumberVal . fromRational
  (/) = onNum2 (/)
  recip = onNum1 recip

instance Boolean PrimVal where
  true = BoolVal True
  false = BoolVal False
  notB = onBool1 not
  (&&*) = onBool2 (&&)
  (||*) = onBool2 (||)

onNum1 :: (Scientific -> Scientific) -> PrimVal -> PrimVal
onNum1 f x = case x of
  NumberVal a -> NumberVal $ f a
  _ -> error "Not numeric primitive value"

onNum2 :: (Scientific -> Scientific -> Scientific) -> PrimVal -> PrimVal -> PrimVal
onNum2 f x y = case (x, y) of
  (NumberVal a, NumberVal b) -> NumberVal $ f a b
  _ -> error "Not numeric primitive value"

onBool1 :: (Bool -> Bool) -> PrimVal -> PrimVal
onBool1 f x = case x of
  BoolVal a -> BoolVal $ f a
  _ -> error "Not boolean primitive value"

onBool2 :: (Bool -> Bool -> Bool) -> PrimVal -> PrimVal -> PrimVal
onBool2 f x y = case (x, y) of
  (BoolVal a, BoolVal b) -> BoolVal $ f a b
  _ -> error "Not boolean primitive value"
