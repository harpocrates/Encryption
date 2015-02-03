import ECC

-- Sample of ECC encryption/decryption
-- ===================================
main = do
        ec <- loadEllipticCurve "P-192"
        c  <- generateRandomPoint ec

        (private_a, public_general_a) <- generateECCGeneralKey ec c
        (private_b, public_general_b) <- generateECCGeneralKey ec c

        let public_a_for_b = generateECCSpecificKey ec private_a public_general_b
            public_b_for_a = generateECCSpecificKey ec private_b public_general_a

            public_a = PublicKey public_general_a public_a_for_b
            public_b = PublicKey public_general_b public_b_for_a

        -- generate a random 4 point long message
        plaintext <- sequence (replicate 4 (generateRandomPoint ec))


        encrypted <-    encryptECC plaintext ec private_a public_b
        let decrypted = decryptECC encrypted ec private_b public_a

        putStrLn $ show $ plaintext
        putStrLn $ show $ decrypted