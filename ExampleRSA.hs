import RSA

-- Sample of RSA encryption/decryption
-- ===================================
main = do
         primesList <- loadPrimeNumbers

         (private, public) <- generateRSAKey primesList

         putStrLn $ show $ decryptRSA (encryptRSA 666000666 public) private
         putStrLn $ show $ decryptRSA (encryptRSA 696969696 public) private
         putStrLn $ show $ decryptRSA (encryptRSA 123456789 public) private
         putStrLn $ show $ decryptRSA (encryptRSA 000000000 public) private
         putStrLn $ show $ decryptRSA (encryptRSA 101010101 public) private