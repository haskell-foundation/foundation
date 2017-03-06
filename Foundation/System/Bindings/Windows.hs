{-# OPTIONS_HADDOCK hide #-}
module Foundation.System.Bindings.Windows
     where

type CFd = CInt

foreign import capi unsafe "_wsopen"
    sysWindowsWsopen :: Ptr CWChar -> CInt -> CInt -> CInt -> IO CInt
foreign import capi unsafe "_read"
    sysWindowsRead :: Ptr CFd -> CUInt -> IO CInt
