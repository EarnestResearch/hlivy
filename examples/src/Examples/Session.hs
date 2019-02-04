module Examples.Session where

import Control.Lens (view, (^.))
import Network.Livy


-- | Default environment.
newEnv' :: IO Env
newEnv' = newEnv "localhost" 8998


-- | Creates a new interactive session with Livy.
createNewSession :: IO (Either LivyError CreateSessionResponse)
createNewSession = newEnv' >>= flip runLivy (send createSession)


-- | List the active sessions.
listActiveSessions :: IO (Maybe [Session])
listActiveSessions = do
  env <- newEnv'
  resp <- runLivy env (send getSessions)
  case resp of
    -- There was an error interpreting the request.
    Left _     -> return Nothing
    Right body -> return . Just $ body ^. gssrSessions


-- | Run a statement in the given session, returning the id of the created statement if successful.
runStatementInSession :: SessionId -> IO (Either LivyError StatementId)
runStatementInSession sid = do
  env <- newEnv'
  resp <- runLivy env (send $ runStatement sid "val x = 1 + 1; println(x)" SparkSession)
  return $ view (rsrStatement . stId) <$> resp


-- | Get the output of a given statement ran in a given session.
getStatementOutput :: SessionId -> StatementId -> IO (Maybe StatementOutput)
getStatementOutput sid stid = do
  env <- newEnv'
  resp <- runLivy env (send $ getSessionStatement sid stid)
  case resp of
    Left _     -> return Nothing
    Right body -> return $ body ^. gsstmrStatement . stOutput
