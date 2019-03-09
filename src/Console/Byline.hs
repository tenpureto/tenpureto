module Console.Byline where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Text                      ( Text )
import           System.Console.Byline         as B

data InterruptedException = InterruptedException
    deriving Show

instance Exception InterruptedException

runB :: (MonadIO m, MonadMask m) => Byline m a -> m a
runB a = B.runByline a >>= maybe (throwM InterruptedException) return

ask :: (MonadIO m, MonadMask m) => Stylized -> Maybe Text -> m Text
ask prompt defans = runB $ B.ask prompt defans

askUntil
    :: (MonadIO m, MonadMask m)
    => Stylized
    -> Maybe Text
    -> (Text -> m (Either Stylized Text))
    -> m Text
askUntil prompt defans confirm = runB $ B.askUntil prompt defans confirm

sayLn :: (MonadIO m, MonadMask m) => Stylized -> m ()
sayLn message = runB $ B.sayLn message
