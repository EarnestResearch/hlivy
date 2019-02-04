{-# LANGUAGE CPP #-}

{-|
Module      :  Network.Livy.Client.Internal.JSON
Copyright   :  (C) 2019 Earnest Research
License     :  MIT
Maintainer  :  Daniel Donohue <ddonohue@earnestresearch.com>
Stability   :  experimental
Portability :  non-portable
-}

module Network.Livy.Client.Internal.JSON
  ( recordPrefixOptions
  ) where

import           Data.Aeson
#if ! MIN_VERSION_base(4,8,0)
import           Data.Monoid (Monoid (..))
#endif
import qualified Data.Text as T


-- | Utility function to convert a standardized field accessor
-- to a field sent or receieved by Livy.
removeRecordPrefix
  :: Int -- ^ The length of the prefix.
  -> String -- ^ The field label to modify.
  -> String
removeRecordPrefix n = T.unpack . toLowerFirst . T.drop n . T.pack
  where toLowerFirst t = T.toLower (T.take 1 t) <> T.drop 1 t


-- | Encoding/decoding 'Options' for dropping common record prefixes.
recordPrefixOptions :: Int -> Options
recordPrefixOptions n = defaultOptions { fieldLabelModifier = removeRecordPrefix n }
