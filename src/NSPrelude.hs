module NSPrelude (module X, show) where

import Control.Applicative as X (Alternative (empty, (<|>)), liftA2)
import Control.Exception as X (throwIO)
import Control.Monad as X (MonadPlus (..), guard, unless, void, when)
import Control.Monad.Except as X
import Control.Monad.Reader as X
import Control.Monad.State as X
import Data.Foldable as X (for_)
import Data.Functor as X (($>), (<&>))
import Data.Set as X (Set)
import Data.String as X (IsString (..))
import Data.Text as X (Text, intercalate, pack, replace, unlines, unpack)
import Data.Text.IO as X (putStr, putStrLn, readFile, writeFile)
import Data.Traversable as X (for)
import Data.Void as X (Void)
import Prelude as X hiding (MonadFail (..), lookup, putStr, putStrLn, readFile, show, unlines, writeFile)

{- ORMOLU_DISABLE -}
import qualified Data.Text
import qualified Prelude
{- ORMOLU_ENABLE -}

show :: Prelude.Show a => a -> Data.Text.Text
show x = Data.Text.pack (Prelude.show x)
