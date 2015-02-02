# Disclaimer

THIS IS NOT MEANT TO BE USED FOR CRYPTOGRAPHIC PURPOSES. This implementation is naive and only for educational purposes. 
It does not consider defend against [these](http://en.wikipedia.org/wiki/RSA_%28cryptosystem%29#Attacks_against_plain_RSA)
attacks. Note that RSA scales badly, and ECC should be used instead.

## General

This is a toy implementation of RSA. Keys are generated using a file containing large prime numbers. Then, 

To use, import `RSA`. Then, encrypting the number `0123456789` and decrypting it again can be done using the following.

```haskell
import RSA

main = do
         let plainText = 0123456789
         primesList <- loadPrimeNumbers                 -- load list of primes
         (private, public) <- generateRSAKey primesList -- generate keys

         let cipherText     = encryptRSA plainText public  -- encrypt
             decipheredText = decryptRSA cipherText private -- decrypt

         putStrLn $ show plainText
         putStrLn $ show cipherText
         putStrLn $ show decipheredText
```

One possible output is

```
123456789
28716360466573212816140507227160896071254936916916992565701437712147840827172086840295534503452291831112126708177551905202700662825376899309838765076207820678592014757269191144865356746643576321071055950922765550493050235392521781774700826
123456789
```

## Choice of Haskell

  * Arbitrarily large integers supported natively
  * Very safe system, pure and impure functions very clearly seperated
