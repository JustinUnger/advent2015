import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as LB
import Text.Printf
import Data.List

secretKey = "bgvyzdsv1038736"
goodDigest = md5 $ LB.pack secretKey

test1Pass :: Bool
test1Pass = good goodDigest

space :: [Int]
space = [0..(2^24 - 1)]

myPrefix = "bgvyzdsv"

keys :: [String]
keys = map (\x -> myPrefix ++ (printf "%06d" x)) space

ks :: [LB.ByteString]
ks = map LB.pack keys

ms :: [MD5Digest]
ms = map md5 ks

good :: MD5Digest -> Bool
good d = isPrefixOf "000000" $ show d

theAnswer = find (\x -> good $ snd x) (zip keys ms)

main = do 
  putStrLn $ show theAnswer

