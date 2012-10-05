module Network.FTP.Socket
  ( startPasvServer
  , toPortString
  , fromPortString
  , module Network.Socket
  , module Network.BSD
  ) where

{-|
 - Network Utils.
 -}

import Data.List (intercalate)
import Data.Word
import Data.Bits
import Control.Exception (bracketOnError)
import Network.Socket
import Network.BSD (getProtocolNumber)

startPasvServer :: SockAddr -> IO Socket
startPasvServer (SockAddrInet _ host) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
      (socket AF_INET Stream proto)
      sClose
      (\sock -> do
          setSocketOption sock ReuseAddr 0
          bindSocket sock (SockAddrInet aNY_PORT host)
          listen sock 1
          return sock
      )
startPasvServer _ = fail "Require IPv4 sockets"

{- | Converts a socket address to a string suitable for a PORT command.

Example:

> toPortString (SockAddrInet (PortNum 0x1234) (0xaabbccdd)) ->
>                              "170,187,204,221,18,52"
-}
toPortString :: SockAddr -> IO String
toPortString (SockAddrInet port hostaddr) =
    let wport = fromIntegral port::Word16
        in do
           hn <- inet_ntoa hostaddr
           return (replace '.' ',' hn ++ "," ++ 
                   (intercalate "," . map show . getBytes $ wport))
toPortString _ = 
    error "toPortString only works on AF_INET addresses"

-- | Converts a port string to a socket address.  This is the inverse calculation of 'toPortString'.
fromPortString :: String -> IO SockAddr
fromPortString instr =
    let inbytes = split ',' instr
        (hostpart, portpart) = splitAt 4 inbytes
        hostname = intercalate "." hostpart
        portbytes = map read portpart
        in
        do
        addr <- inet_addr hostname
        return $ SockAddrInet (fromInteger $ fromBytes portbytes) addr

-- copied from cgi
--
-- * Utilities
--

-- | Replaces all instances of a value in a list by another value.
replace :: Eq a =>
           a   -- ^ Value to look for
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)

-- copied from MissingH
{- | Returns a list representing the bytes that comprise a data type.

Example:

> getBytes (0x12345678::Int) -> [0x12, 0x34, 0x56, 0x78]
-}
getBytes :: (Integral a, Bounded a, Bits a) => a -> [a]
getBytes input = 
    let getByte _ 0 = []
        getByte x remaining = (x .&. 0xff) : getByte (shiftR x 8) (remaining - 1)
        in
        if (bitSize input `mod` 8) /= 0
           then error "Input data bit size must be a multiple of 8"
           else reverse $ getByte input (bitSize input `div` 8)

{- | The opposite of 'getBytes', this function builds a number based on
its component bytes.

Results are undefined if any components of the input list are > 0xff!

-}

fromBytes :: (Bits a) => [a] -> a
fromBytes =
    foldl (\accum x -> shiftL accum 8 .|. x) 0

-- | split a list by sperator
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split sep xs = s : split sep rest'
  where (s, rest) = break (==sep) xs
        rest' = case rest of
                  (_:r) -> r
                  _      -> []
