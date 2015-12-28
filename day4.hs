import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as LB
import Text.Printf
import Data.List

secretKey = "abcdef609043"

myPrefix = "bgvyzdsv"

bytes :: LB.ByteString
bytes = LB.pack secretKey

goodDigest = md5 $ LB.pack secretKey

space :: [Int]
space = [0..(2^24 - 1)]

keys :: [String]
keys = map (\x -> myPrefix ++ (printf "%06d" x)) space

ks :: [LB.ByteString]
ks = map LB.pack keys

ms :: [MD5Digest]
ms = map md5 ks

good :: MD5Digest -> Bool
good d = isPrefixOf "00000" $ show d

theAnswer = find (\x -> good $ snd x) (zip keys ms)

main = do 
  putStrLn $ show theAnswer

