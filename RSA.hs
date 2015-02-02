module RSA (PublicKey, PrivateKey, generateRSAKey, encryptRSA, decryptRSA, loadPrimeNumbers) where

import System.Random
import System.IO
import NumberTheory

primeNumbersFile = "PrimeNumbers.txt"

-- Load a list of large prime numbers from primeNumbersFile. (This list is courtesy of
-- [Random Small Primes](https://primes.utm.edu/lists/small/small2.html))
loadPrimeNumbers :: IO [Integer]
loadPrimeNumbers =
    do
      file <- readFile primeNumbersFile
      let lines = splitLines file
      return $ map (\line -> read line :: Integer) lines
  where splitLines str = let (line, rest) = break (=='\n') str
                         in if (null rest) then [line] else line:(splitLines (tail rest))

-- RSA Encryption
-- ==============

-- Types representing public and private keys
data PublicKey  = PublicKey  Integer Integer         -- (n k)
data PrivateKey = PrivateKey Integer Integer Integer -- (p1 p2 k)

-- A RSA key is two primes p1 and p2, as well as a number k. Let n = p1*p2
--  * to encrypt: m -> m^k (mod n)
--  * to decrypt: c -> c^d (mod n)  where d = k^{-1} (mod phi(n))
-- Generates private key (p1,p2,k) and public key (n,k).
generateRSAKey :: [Integer] -> IO (PrivateKey, PublicKey)
generateRSAKey primesList =
    do
      i <- randomRIO (0,(length primesList)-1)
      j <- randomRIO (0,(length primesList)-1)
      -- Choose p1 and p2 from primesList
      let p1 = primesList!!i
          p2 = primesList!!j

      -- Choose a random k
      a <- randomIO :: IO Integer
      b <- randomIO :: IO Integer
      let k = (a^^^b |% (p1-1)*(p2-1))

      -- Check our choice of k works. It should almost always (in the mathematical sense) work.
      if checkPublicKey (p1,p2,k)
        then return ((PrivateKey p1 p2 k), (PublicKey (p1*p2) k))
        else generateRSAKey primesList
  where
    -- make sure k and phi(n) are relatively prime
    checkPublicKey (p1,p2,k) = ((p1-1)*(p2-1)) `gcd` k == 1

-- To encrypt, all you need is "a" (what to encrypt), "m" (the mod), and "k" (the power).
-- The encryption map is (a -> a^k) (everything mod m).
encryptRSA :: Integer -> PublicKey -> Integer
encryptRSA a (PublicKey m k) = a^^^k |% m

-- To decrypt, you need "b" (what to decrypt), "p1" and "p2" (the prime factors of "m"), "m" (the mod), and "k" (the power).
decryptRSA :: Integer -> PrivateKey -> Integer
decryptRSA b (PrivateKey p1 p2 k) = b^^^k' |% (p1*p2)
  where phi = (p1 - 1)*(p2 - 1)
        k'  = invert k |% phi