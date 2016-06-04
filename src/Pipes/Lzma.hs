{-# LANGUAGE ScopedTypeVariables #-}

module Pipes.Lzma
    ( -- * Compression
      compress
    , compressWith
      -- * Decompression
    , decompress
    , decompressWith
    ) where

import Pipes
import qualified Codec.Compression.Lzma as Lzma
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Decompress a 'ByteString'
decompress :: forall m r. MonadIO m
           => Producer ByteString m r
           -> Producer ByteString m (Producer ByteString m r)
decompress = decompressWith Lzma.defaultDecompressParams

-- | Decompress a 'ByteString'.
decompressWith :: forall m r. MonadIO m
               => Lzma.DecompressParams
               -> Producer ByteString m r
               -> Producer ByteString m (Producer ByteString m r)
decompressWith params prod0 = liftIO (Lzma.decompressIO params) >>= go prod0
  where
    go :: Producer ByteString m r
       -> Lzma.DecompressStream IO
       -> Producer ByteString m (Producer ByteString m r)
    go prod s@(Lzma.DecompressInputRequired more) = do
        mx <- lift $ next prod
        case mx of
          Right (x, prod')
            | BS.null x    -> go prod' s
            | otherwise    -> liftIO (more x) >>= go prod'
          Left r           -> liftIO (more mempty) >>= go (return r)
    go prod (Lzma.DecompressOutputAvailable output cont) = do
        yield output
        liftIO cont >>= go prod
    go prod (Lzma.DecompressStreamEnd leftover) =
        return (yield leftover >> prod)
    go prod (Lzma.DecompressStreamError Lzma.LzmaRetOK) =
        return prod
    go _prod (Lzma.DecompressStreamError err) =
        fail $ "Pipes.Lzma.decompress: Error "++show err

-- | Compress a 'ByteString'
compress :: forall m r. MonadIO m
         => Producer ByteString m r
         -> Producer ByteString m r
compress = compressWith Lzma.defaultCompressParams

-- | Compress a 'ByteString'
compressWith :: forall m r. MonadIO m
             => Lzma.CompressParams
             -> Producer ByteString m r
             -> Producer ByteString m r
compressWith params prod0 = liftIO (Lzma.compressIO params) >>= go prod0
  where
    go :: Producer ByteString m r
       -> Lzma.CompressStream IO
       -> Producer ByteString m r
    go prod s@(Lzma.CompressInputRequired _flush more) = do
        mx <- lift $ next prod
        case mx of
          Right (x, prod')
            | BS.null x    -> go prod' s
            | otherwise    -> liftIO (more x) >>= go prod'
          Left r           -> liftIO (more mempty) >>= go (return r)
    go prod (Lzma.CompressOutputAvailable output cont) = do
        yield output
        liftIO cont >>= go prod
    go prod Lzma.CompressStreamEnd =
        prod
