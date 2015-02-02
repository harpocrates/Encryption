module ECC where

import System.Random
import System.IO
import NumberTheory

ellipticCurvesFile = "ellipticCurves.txt"

checkValidEllipticCurve :: (Integer, Integer, Integer) -> Bool
checkValidEllipticCurve (a,b,p) = (4 * (a^^^3 |% p) + 27 * (b^^^2 |% p)) % p == 0

data Point = Point Integer Integer | O deriving (Eq, Show)
-- p    - base field prime
-- a, b - define the elliptic curve in E/F_p (the elliptic curve is also referred to as E_p(a,b))
-- g    - base point (hopefully a generator) on the elliptic curve
-- n    - |g| (order of g)
-- h    - |E/F_p|/|g| (ideally, if g is a generator, this is 1)
data EllipticCurve = EllipticCurve Integer Integer Integer Point Integer Integer -- p a b g n h

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

-------------------------------------------------------

-- Find n such that n <*> p = q
discreteLog :: Point -> Point -> EllipticCurve -> Integer
discreteLog p q ec = let (_,n) = until (\(x,n) -> q==x) (\(x,n) -> (x <+> p $ ec, n+1)) (p,1) in n

main = do
          --putStrLn $ show $ (Point 2 0) <+> (Point 1 3) $ (EllipticCurve 17 1 7 (Point 1 2) 0 0)
          --putStrLn $ show $ (Point 1 3) <+> (Point 1 3) $ (EllipticCurve 17 1 7 (Point 1 2) 0 0)
          --putStrLn $ show $ 2 <*> (Point 1 3)           $ (EllipticCurve 17 1 7 (Point 1 2) 0 0)
          --putStrLn $ show $ discreteLog (Point 16 5) (Point 4 5) (EllipticCurve 23 9 17 (Point 1 2) 0 0)
          --putStrLn $ show $ 9 <*> (Point 16 5)          $ (EllipticCurve 23 9 17 (Point 1 2) 0 0)
          --putStrLn $ show $ (negative (Point 16 5)) <+> (Point 16 5) $ (EllipticCurve 23 9 17 (Point 1 2) 0 0)

          let p = 6277101735386680763835789423207666416083908700390324961279
              a = -3
              b = 0x64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
              g = Point 0x188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
                        0x07192b95ffc8da78631011ed6b24cdd573f977a11e794811
              n = 6277101735386680763835789423176059013767194773182842284081
              h = 1
              ec = EllipticCurve p a b g n h

          c <- generateRandomPoint ec

          (private_a, public_general_a) <- generateECCGeneralKey ec c
          (private_b, public_general_b) <- generateECCGeneralKey ec c

          let public_a_for_b = generateECCSpecificKey ec private_a public_general_b
              public_b_for_a = generateECCSpecificKey ec private_b public_general_a

              public_a = PublicKey public_general_a public_a_for_b
              public_b = PublicKey public_general_b public_b_for_a

          m1 <- generateRandomPoint ec
          m2 <- generateRandomPoint ec
          m3 <- generateRandomPoint ec
          m4 <- generateRandomPoint ec

          let plaintext = [m1,m2,m3,m4]
          putStrLn $ show $ plaintext

          encrypted <-    encryptECC plaintext ec private_a public_b
          let decrypted = decryptECC encrypted ec private_b public_a

          putStrLn $ show $ decrypted