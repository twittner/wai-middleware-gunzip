-- This Source Code Form is subject to the terms of
-- the Mozilla Public License, v. 2.0. If a copy of
-- the MPL was not distributed with this file, You
-- can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.Gunzip (gunzip) where

import Control.Applicative
import Control.Exception (throwIO)
import Data.IORef
import Network.HTTP.Types (Header, hContentEncoding)
import Network.Wai (Middleware, Request, RequestBodyLength (ChunkedBody))

import qualified Data.ByteString     as S
import qualified Data.Streaming.Zlib as Z
import qualified Network.Wai         as Wai

gunzip :: Middleware
gunzip app rq k
    | isGzip rq = prepare >>= flip app k
    | otherwise = app rq k
  where
    prepare = do
        r <- newIORef []
        i <- Z.initInflate (Z.WindowBits 31)
        return $ rq { Wai.requestBody       = inflate r i
                    , Wai.requestBodyLength = ChunkedBody -- FIXME
                    , Wai.requestHeaders    = noGzip (Wai.requestHeaders rq)
                    }

    inflate r i = do
        buffered <- readIORef r
        case buffered of
            []     -> Wai.requestBody rq >>= continue r i
            (x:xs) -> writeIORef r xs >> return x

    continue r i b =
        if S.null b then
            return S.empty
        else do
            f <- toBytes id =<< Z.feedInflate i b
            x <- f . (:[]) <$> Z.finishInflate i
            case x of
                []     -> return S.empty
                (y:ys) -> writeIORef r ys >> return y

    toBytes front p = p >>= \r -> case r of
        Z.PRDone    -> return front
        Z.PRNext  b -> toBytes (front . (:) b) p
        Z.PRError e -> throwIO e

isGzip :: Request -> Bool
isGzip = maybe False ("gzip" ==) . lookup hContentEncoding . Wai.requestHeaders

noGzip :: [Header] -> [Header]
noGzip = filter (\(k, v) -> k /= hContentEncoding || v /= "gzip")
