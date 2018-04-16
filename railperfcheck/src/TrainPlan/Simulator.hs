module TrainPlan.Simulator (
  Event(..),
  withInfrastructureFile, 
  withRoutesFile,
  withDispatch,
  dispatchTiming
) where

import Foreign.C.String
import Foreign.Ptr

data StaticInfrastructure
data Routes 
data Dispatch

foreign import ccall unsafe "parse_infrastructure_file" parseInfrastructureFile  :: CString -> IO (Ptr StaticInfrastructure)
foreign import ccall unsafe "parse_routes_file" parseRoutesFile :: Ptr StaticInfrastructure -> CString -> IO (Ptr Routes)
foreign import ccall unsafe "parse_dispatch" parseDispatch :: CString -> IO (Ptr Dispatch)
foreign import ccall unsafe "eval_simplified" evalSimplified :: Ptr StaticInfrastructure -> Ptr Routes -> Ptr Dispatch  -> IO CString
foreign import ccall unsafe "free_infrastructure" freeInfrastructure :: Ptr StaticInfrastructure -> IO ()
foreign import ccall unsafe "free_routes" freeRoutes :: Ptr Routes -> IO ()
foreign import ccall unsafe "free_dispatch" freeDispatch :: Ptr Dispatch -> IO ()

withObject :: (CString -> IO (Ptr a)) -> (Ptr a -> IO ()) -> 
      String -> (Ptr a -> IO b) -> IO b
withObject create free input func = do
  obj <- withCString input create
  if obj == nullPtr then do
    error "Error: could not create simulator object"
  else do
    out <- func obj
    free obj
    return out

withInfrastructureFile = withObject parseInfrastructureFile freeInfrastructure
withRoutesFile inf = withObject (parseRoutesFile inf) freeRoutes
withDispatch = withObject parseDispatch freeDispatch

data Event
  = Event
  { train :: String
  , time :: Double
  , node :: String }
  deriving (Show, Eq, Ord)

dispatchTiming :: Ptr StaticInfrastructure -> Ptr Routes -> Ptr Dispatch -> IO [Event]
dispatchTiming inf routes d = do
  result <- evalSimplified inf routes d
  if result == nullPtr then do
    error "Error: Simulator failed"
  else do
    s <- peekCString result
    return (parseEvents s)

parseEvents :: String -> [Event]
parseEvents x = fmap (p.words) (lines x)
  where 
    p [x,y,z] = Event x (read y) z
    p _ = error "Error: parse error in event log from simulation"
