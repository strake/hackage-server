{-# LANGUAGE CPP #-}

#ifdef SAFE_HASKELL
{-# LANGUAGE Unsafe #-}
#endif

-- |
-- Maintainer: dag.odenhall@gmail.com
-- Stability: experimental
-- Portability: Haskell2010
module NLP.Snowball.IO.Unsafe.C
    ( -- * Low-level unsafe C interface in IO
      Stemmer
    , list
    , new
    , delete
    , finalize
    , stem
    , length
    ) where

import qualified Foreign
import qualified Foreign.C as Foreign
import qualified Prelude

-- | The foreign @sb_stemmer@ struct.
data Stemmer

-- | The foreign @sb_stemmer_list@ function.
foreign import ccall unsafe "sb_stemmer_list"
    list :: Prelude.IO (Foreign.Ptr Foreign.CString)
{- INLINABLE list -}

-- | The foreign @sb_stemmer_new@ function.
foreign import ccall unsafe "sb_stemmer_new"
    new
        :: Foreign.CString
        -> Foreign.CString
        -> Prelude.IO (Foreign.Ptr Stemmer)
{- INLINABLE new -}

-- | The foreign @sb_stemmer_delete@ function.
foreign import ccall unsafe "sb_stemmer_delete"
    delete :: Foreign.Ptr Stemmer -> Prelude.IO ()
{- INLINABLE delete -}

-- | Pointer to the foreign @sb_stemmer_delete@ function.
foreign import ccall unsafe "&sb_stemmer_delete"
    finalize :: Foreign.FinalizerPtr Stemmer
{- INLINABLE finalize -}

-- | The foreign @sb_stemmer_stem@ function.
foreign import ccall unsafe "sb_stemmer_stem"
    stem
        :: Foreign.Ptr Stemmer
        -> Foreign.CString
        -> Foreign.CInt
        -> Prelude.IO Foreign.CString
{- INLINABLE stem -}

-- | The foreign @sb_stemmer_length@ function.
foreign import ccall unsafe "sb_stemmer_length"
    length :: Foreign.Ptr Stemmer -> Prelude.IO Foreign.CInt
{- INLINABLE length -}
