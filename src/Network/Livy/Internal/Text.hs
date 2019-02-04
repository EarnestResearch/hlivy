{-|
Module      :  Network.Livy.Internal.Text
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable
-}

module Network.Livy.Internal.Text where

import           Data.ByteString (ByteString)
import           Data.Maybe (maybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


class ToText a where
  toText :: a -> Text

instance ToText Text where toText = id
instance ToText Int where toText = T.pack . show
instance ToText ByteString where toText = T.decodeUtf8
instance ToText a => ToText (Maybe a) where toText = maybe "" toText


-- | Create a mapping of a value with its textual representation.
toTextLookup :: (Bounded a, Enum a, ToText a) => [(Text, a)]
toTextLookup = (\x -> (toText x, x)) <$> [minBound ..]
