module Main

import Data.String

getInteger : IO Integer
getInteger = do
    putStr "guess a number: "
    n <- getLine
    case parsePositive n of
        Just n => pure n
        Nothing => do
            putStrLn (n ++ " is not a number.")
            getInteger


game : Integer -> IO ()
game secret = do
    n <- getInteger
    case compare n secret of
        LT => do
            putStrLn "Too small"
            game secret
        EQ => putStrLn "You got it. Congrats!"
        GT => do
            putStrLn "Too big"
            game secret


tryParse : String -> Either String Integer
tryParse input =
    case parsePositive input of
        Maybe => Left input
        Just n => Right n


getRandom : IO (Either FileError Int)
getRandom = do
    efile <- openFile "/dev/random" Read
    case efile of
        Left e => pure (Left e)
        Right file => do
            echars <- fGetChars file 4
            case echars of
                Left e => pure (Left e)
                Right chars => pure (Right (foldl (\acc, i => (shiftL acc 8) + i) 0 (map ord (unpack chars))))


main : IO ()
main = do
    eint <- getRandom
    case eint of
        Left _ => pure()
        Right int => game (cast (1 + (mod int 10)))



