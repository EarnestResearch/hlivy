module Examples.Batch where

import Control.Lens (view, (&), (?~), (^.))
import Control.Monad (void)
import Network.Livy

newEnv' :: IO Env
newEnv' = newEnv "localhost" 8998


-- | Create a new batch session.
--
-- The default Livy configuration expects that the file containing
-- the application is in HDFS.  To be able to upload a JAR on the local
-- filesystem, set the @livy.file.local-dir-whitelist@ value in livy.conf.
createNewBatch :: IO (Either LivyError CreateBatchResponse)
createNewBatch = do
  env <- newEnv'
  runLivy env (send req)
  where req = createBatch "/user/hadoop/my-app.jar"
            & cbClassName ?~ "com.company.project.my_app"
            & cbExecutorCores ?~ 4
            & cbName ?~ "my_app"


-- | Get the states of all batch sessions.
listBatchStates :: IO (Maybe [BatchState])
listBatchStates = do
  env <- newEnv'
  resp <- runLivy env (send getBatches)
  case resp of
    Left _     -> return Nothing
    Right body -> return . Just $
      view bState <$> body ^. gbsrSessions


-- | Terminate a given batch session.
killBatchSession :: BatchId -> IO ()
killBatchSession bid = newEnv' >>= void . flip runLivy (send $ killBatch bid)
