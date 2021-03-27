module TEPrelude (showT, errorT, module X) where

import Control.Applicative as X
import Control.Monad.Except as X
import Control.Monad.Reader as X
import Control.Monad.State as X
import Data.Foldable as X (for_, traverse_)
import Data.Map.Strict as X (Map)
import Data.Maybe as X (catMaybes, fromMaybe)
import Data.String as X (IsString)
import Data.Text as X (Text)
import Data.Text.IO as X (putStrLn, readFile, writeFile)
import Data.Void as X
import Prelude as X hiding (lex, putStrLn, readFile, writeFile)

{- ORMOLU_DISABLE -}
import qualified Data.Text as Text
{- ORMOLU_ENABLE -}

showT :: Show a => a -> Text
showT = Text.pack . show

errorT :: Text -> a
errorT = error . Text.unpack
