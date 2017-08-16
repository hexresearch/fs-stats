module Data.Stat.Fs.PrimVal(
    IsPrimVal(..)
  , PrimVal(..)
  , getText, getBool, getNumber
) where

import Data.Data
import Data.Scientific
import Data.String
import Data.Text (Text)
import Data.Yaml(ToJSON(..))
import GHC.Generics

import qualified Data.Text as T
import qualified Data.Yaml as Y

-- | Primitive value: text, number or boolean
data PrimVal
  = TextVal !Text
  | NumberVal !Scientific
  | BoolVal !Bool
  deriving (Show, Eq, Generic, Data)

class IsPrimVal a where
  toPrimVal :: a -> PrimVal
  fromPrimVal :: PrimVal -> a

instance ToJSON PrimVal where
  toJSON x = case x of
    TextVal t -> Y.String t
    NumberVal n -> Y.Number n
    BoolVal b -> Y.Bool b

-------------------------------------
-- prim val instances

getText :: PrimVal -> Text
getText x = case x of
  TextVal t -> t
  _ -> error "PrimVal is not a Text"

getBool :: PrimVal -> Bool
getBool x = case x of
  BoolVal t -> t
  _ -> error "PrimVal is not a Bool"

getNumber :: PrimVal -> Scientific
getNumber x = case x of
  NumberVal t -> t
  _ -> error "PrimVal is not a Number"

instance IsPrimVal Text where
  toPrimVal = TextVal
  fromPrimVal = getText

instance IsPrimVal Bool where
  toPrimVal = BoolVal
  fromPrimVal = getBool

instance IsPrimVal Scientific where
  toPrimVal = NumberVal
  fromPrimVal = getNumber

instance IsPrimVal String where
  toPrimVal = TextVal . fromString
  fromPrimVal = T.unpack . getText

instance IsPrimVal Int where
  toPrimVal = NumberVal . fromIntegral
  fromPrimVal = round . getNumber

instance IsPrimVal Float where
  toPrimVal = NumberVal . realToFrac
  fromPrimVal = realToFrac . getNumber

instance IsPrimVal Double where
  toPrimVal = NumberVal . realToFrac
  fromPrimVal = realToFrac . getNumber


