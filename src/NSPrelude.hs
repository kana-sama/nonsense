module NSPrelude (module X, show) where

import Control.Applicative as X (Alternative (..), liftA2)
import Control.Exception as X (throwIO)
import Control.Monad as X (MonadPlus (..), guard, unless, void, when)
import Control.Monad.Reader as X hiding (MonadFail (..))
import Control.Monad.State as X hiding (MonadFail (..))
import Data.Data as X (Data)
import Data.Foldable as X (for_)
import Data.String as X (IsString (..))
import Data.Text as X (Text, intercalate, pack, replace, unlines)
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
