-- This Source Code Form is subject to the terms of
-- the Mozilla Public License, v. 2.0. If a copy of
-- the MPL was not distributed with this file, You
-- can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.Gunzip (gunzip) where

import Control.Exception (throwIO)
import Network.HTTP.Types (hContentEncoding)
import Network.Wai (Middleware, Request, RequestBodyLength (ChunkedBody))

import qualified Data.ByteString      as S
import qualified Data.Streaming.Zlib  as Z
import qualified Data.ByteString.Lazy as L
import qualified Network.Wai          as Wai

gunzip :: Middleware
gunzip app rq k
    | isGzip rq = prepare >>= flip app k
    | otherwise = app rq k
  where
    prepare = do
        i <- Z.initInflate (Z.WindowBits 31)
        return $ rq { Wai.requestBody       = inflate i
                    , Wai.requestBodyLength = ChunkedBody -- FIXME
                    }

    inflate i = Wai.requestBody rq >>= \b ->
        if S.null b
            then return S.empty
            else L.toStrict `fmap` do
                f <- toBytes id =<< Z.feedInflate i b
                (L.fromChunks . f . (:[])) `fmap` Z.finishInflate i

    toBytes front p = p >>= \r -> case r of
        Z.PRDone    -> return front
        Z.PRNext  b -> toBytes (front . (:) b) p
        Z.PRError e -> throwIO e

isGzip :: Request -> Bool
isGzip = maybe False ("gzip" ==) . lookup hContentEncoding . Wai.requestHeaders
