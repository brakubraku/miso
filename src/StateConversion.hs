import Control.Monad.Trans.State.Strict (StateT, get, put, lift)

-- Example state types
data OldState = OldState { oldValue :: Int }
data NewState = NewState { newValue :: String }

-- Conversion functions
toNewState :: OldState -> NewState
toNewState (OldState v) = NewState (show v)

fromNewState :: NewState -> OldState
fromNewState (NewState v) = OldState (read v)

-- Convert one StateT to another
convertStateT :: Monad m => StateT OldState m a -> StateT NewState m a
convertStateT oldStateT = do
  newState <- get
  let oldState = fromNewState newState
  (result, updatedOldState) <- lift $ runStateT oldStateT oldState
  put (toNewState updatedOldState)
  return result
