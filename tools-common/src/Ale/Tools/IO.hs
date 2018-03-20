-- | Input/output helpers.
module Ale.Tools.IO
       ( openFileOrStdin
       ) where

import Universum

import System.IO (Handle, IOMode (ReadMode), stdin)


-- | Open given file for reading unless the name is @-@,
-- in which case use 'stdin'
openFileOrStdin :: MonadIO m => FilePath -> m Handle
openFileOrStdin "-"  = pure stdin
openFileOrStdin name = openFile name ReadMode
