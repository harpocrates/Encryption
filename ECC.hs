module ECC (EllipticCurve, PrivateKey, PublicGeneralKey, PublicSpecificKey, PublicKey(PublicKey),
            loadEllipticCurve, generateRandomPoint,
            generateECCGeneralKey, generateECCSpecificKey,
            encryptECC, decryptECC) where

import System.Random
import System.IO
import Data.Maybe
import NumberTheory

ellipticCurvesFile = "EllipticCurves.txt"

-- Load a list of common elliptic curves. (These are recommended by
-- [NIST](http://csrc.nist.gov/groups/ST/toolkit/documents/dss/NISTReCur.pdf))
loadEllipticCurves :: IO [(String,EllipticCurve)]
loadEllipticCurves =
    do
      file <- readFile ellipticCurvesFile
      return $ map (\line -> read line :: (String, EllipticCurve)) (lines file)

loadEllipticCurve :: String -> IO EllipticCurve
loadEllipticCurve str = loadEllipticCurves >>= (return . (\ecs -> fromJust $ lookup str ecs))

-- For an elliptic curve to form a group, we require that
checkValidEllipticCurve :: (Integer, Integer, Integer) -> Bool
checkValidEllipticCurve (a,b,p) = (4 * (a^^^3 |% p) + 27 * (b^^^2 |% p)) % p == 0

-- Discussion of [group theory](https://www.certicom.com/10-introduction)

data Point = Point Integer Integer | O deriving (Eq, Show, Read)
-- p    - base field prime
-- a, b - define the elliptic curve in E/F_p (the elliptic curve is also referred to as E_p(a,b))
-- g    - base point (hopefully a generator) on the elliptic curve
-- n    - |g| (order of g)
-- h    - |E/F_p|/|g| (ideally, if g is a generator, this is 1)
-- In order: p a b g n h
data EllipticCurve = EllipticCurve Integer Integer Integer Point Integer Integer deriving(Show, Read)

-- Elliptic curve operations over prime fields
(<+>) :: Point -> Point -> EllipticCurve -> Point
(<+>) O p _ = p
(<+>) p O _ = p
(<+>) p@(Point x_p y_p) q@(Point x_q y_q) (EllipticCurve m a b g n h)
  | p /= q && x_p == x_q = O
  | otherwise            = do
                             let s = if p == q
                                       then (3*x_p^2 + a) *** (invert (2*y_p) |% m)     |% m
                                       else (y_p - y_q)   *** (invert (x_p - x_q) |% m) |% m
                                 x_r = s^2 - x_p - x_q
                                 y_r = -y_p + s*(x_p - x_r)
                             Point (x_r % m) (y_r % m)

negative :: Point -> Point
negative (Point x_p y_p) = Point x_p (-y_p)

(<*>) :: Integer -> Point -> EllipticCurve -> Point
(<*>) a p ec = helper a O p
  where
    helper :: Integer -> Point -> Point -> Point
    helper p x a | p==0  = x
                 | p>0   = helper (p `quot` 2)
                                  (if (even p) then x else (a <+> x $ ec))
                                  (a <+> a $ ec)
------------------------------------------------------

-- ECC Encryption
-- ==============

-- Types representing public and private keys
data PrivateKey        = PrivateKey Integer Point            -- (alpha a)
data PublicGeneralKey  = PublicGeneralKey Point Point Point  -- (a_1 a_2 c)
data PublicSpecificKey = PublicSpecificKey Point             -- (a_b)
data PublicKey         = PublicKey PublicGeneralKey PublicSpecificKey

generateRandomPoint :: EllipticCurve -> IO Point
generateRandomPoint ec@(EllipticCurve p a b g n h)
  = randomRIO (1, n-1) >>= return . (\d -> d <*> g $ ec)

-- Given an elliptic curve and a random point c, generate a private key (a alpha) and a general
-- public key (a_1 a_2).
generateECCGeneralKey :: EllipticCurve -> Point -> IO (PrivateKey, PublicGeneralKey)
generateECCGeneralKey ec@(EllipticCurve _ _ _ _ n _) c
  = do
      alpha <- randomRIO (1, n-1)     -- 'alpha' is a random number less than the order of E_p(a,b)
      a     <- generateRandomPoint ec -- 'a' is therefore a random element of the curve
      let a_1 = alpha <*> (c <+> a $ ec) $ ec
          a_2 = alpha <*> a              $ ec
      return (PrivateKey alpha a, PublicGeneralKey a_1 a_2 c)

-- Given an elliptic curve, one's private key, and someone else's public general key, generate a
-- public specific key for that party.
generateECCSpecificKey :: EllipticCurve -> PrivateKey -> PublicGeneralKey -> PublicSpecificKey
generateECCSpecificKey ec (PrivateKey alpha _) (PublicGeneralKey _ b_2 _)
  = PublicSpecificKey (alpha <*> b_2 $ ec)

-- Encrypt a list of points using one's private key and another party's public key
encryptECC :: [Point] -> EllipticCurve -> PrivateKey -> PublicKey -> IO [(Point,Point)]
encryptECC points ec@(EllipticCurve _ _ _ _ n _) private public
  = mapM encryptPoint points
    where
      encryptPoint :: Point -> IO (Point, Point)
      encryptPoint point
        = do
            gamma <-  randomRIO (1, n-1)
            let (PublicKey (PublicGeneralKey a_1 a_2 c) (PublicSpecificKey a_b)) = public
                (PrivateKey beta b)                                              = private
                e_1 = gamma <*> c $ ec
                e_2 = (point <+> ((beta + gamma) <*> a_1 $ ec) $ ec)
                        <+> ((negative (gamma <*> a_2 $ ec)) <+> a_b $ ec) $ ec
            return (e_1, e_2)

-- Decrypt a list of pairs of points using one's private key and another party's public key
decryptECC :: [(Point,Point)] -> EllipticCurve -> PrivateKey -> PublicKey -> [Point]
decryptECC points ec private public
  = let (PublicKey (PublicGeneralKey b_1 b_2 _) (PublicSpecificKey b_a)) = public
        (PrivateKey alpha a)                                             = private
    in map (\(e_1, e_2) ->
                   (e_2 <+> (negative ((alpha <*> (e_1 <+> b_1 $ ec) $ ec) <+> b_a $ ec)) $ ec)) points

-- Brute Force Decryption
-- ======================

-- Find n such that n <*> p = q
discreteLog :: Point -> Point -> EllipticCurve -> Integer
discreteLog p q ec = let (_,n) = until (\(x,n) -> q==x) (\(x,n) -> (x <+> p $ ec, n+1)) (p,1) in n

