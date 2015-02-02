module NumberTheory where

-- NOTES: permits us to define infix functions with signature 'a -> a -> a' and call them
-- using a more suggestive notation 'x ^^^ y |% m' (for function (^^^) :: a -> a -> a)
(|%) = ($)
infixr 1 |%

-- Yields the remainder from the division of the first argument by the second.
-- NOTES: The result has the same sign as the second argument.
(%) :: (Integral a) => a -> a -> a
x % p = if y >= 0 then y else y + p
  where y = x `mod` p
infix 6 %

-- Calculates the bezout coefficients of a given pair of numbers, and their gcd.
-- (Given a and b, it returns [x,y,g] where a*x + b*y = g and g is the gcd.)
bezout :: (Integral a) => a -> a -> [a]
bezout x y = bezout [1,0,x] [0,1,y]
  where bezout u v
          | v!!2==0   = u
          | otherwise = let q = (u!!2) `div` (v!!2)
                        in bezout v [u!!k - q * v!!k | k <- [0..2]]

-- Finds the modular multiplicative inverse
-- NOTES: The number and the mod must be relatively prime for an answer to exist. If this is not
-- the case, no inverse exists, and "invert" returns 0.
-- (Given a and m, it returns x where a*m = 1 (mod m).)
invert :: (Integral a) => a -> a -> a
invert x y | b==1      = a % y
           | otherwise = 0
  where [a,_,b] = bezout x y


-- Finds the modular product
-- NOTES: uses the fact that a*b (mod m) is equal to c*d (mod m) provided a = c (mod m) and
-- b = d (mod m). Exploits the fact '%' is much faster than '*'.
-- (Given a, b, and m, it returns x where x = a*b (mod m))
(***) :: (Integral a) => a -> a -> a -> a
(***) a b m = ((a % m) * (b % m)) % m

-- Finds the modular addition
-- NOTES: uses the fact that a+b (mod m) is equal to c+d (mod m) provided a = c (mod m) and
-- b = d (mod m). Exploits the fact '%' is much faster than '+'.
-- (Given a, b, and m, it returns x where x = a+b (mod m))
(+++) :: (Integral a) => a -> a -> a -> a
(+++) a b m = ((a % m) + (b % m)) % m

-- Finds the modular exponent
-- NOTES: Usual exponentiation by squaring, but for negative exponent take a^(-k) = (1/a)^k where
-- (1/a) is found through inverting in the given modulo.
-- (Given a, k, and m, it returns x where x = a^k (mod m))
(^^^) :: (Integral a) => a -> a -> a -> a
(^^^) a k m = helper k 1 a
  where
    helper k x a
      | k==0  = x % m
      | k>0   = helper (k `quot` 2) (if (even k) then x else (a*x % m)) ((a*a) % m)
      | k<0   = (invert a m) ^^^ (negate k) |%  m