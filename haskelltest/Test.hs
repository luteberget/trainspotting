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
      String -> (Ptr a -> IO b) -> IO (Maybe b)
withObject create free input func = do
  obj <- withCString input create
  if obj == nullPtr then do
    putStrLn "Error parsing"
    return Nothing
  else do
    out <- func obj
    free obj
    return (Just out)

withInfrastructure = withObject parseInfrastructureFile freeInfrastructure
withRoutes inf = withObject (parseRoutesFile inf) freeRoutes
withDispatch = withObject parseDispatch freeDispatch

main = do
  putStrLn "hello from haskell"
  withInfrastructure "../examples/protostations/1-stopping.infrastructure" $ \inf -> do
  withRoutes inf "../examples/protostations/1-stopping.routes" $ \routes -> do
    putStrLn "yes"
