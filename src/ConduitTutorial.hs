module ConduitTutorial where

import           Conduit
import           Data.String.Utils

sumList :: (Num a) => [a] -> a
sumList xs = runConduitPure $ yieldMany xs .| sumC

dummyPath :: String
dummyPath = "/Users/akhong/Downloads/input.txt"

makeDummyFile :: IO ()
makeDummyFile = writeFile dummyPath "this is a test."

sinkDummyFile :: MonadUnliftIO m => m ()
sinkDummyFile = runConduitRes $ sourceFileBS dummyPath .| sinkFile (replace "input" "output" dummyPath)

printDummyFile :: IO ()
printDummyFile = readFile dummyPath >>= putStrLn

sink :: Monad m => ConduitT Int o m (String, Int)
{-sink = do-}
    {-x <- takeC 5 .| mapC show .| foldC-}
    {-y <- sumC-}
    {-return (x, y)-}
sink = (\x y -> (x, y)) <$> (takeC 5 .| mapC show .| foldC) <*> sumC

monadicCompositionExample :: IO ()
monadicCompositionExample = do
    let res = runConduitPure $ yieldMany [1..10] .| sink
    print res

trans :: Monad m => ConduitT Int Int m ()
trans = do
    takeC 3 .| mapC (+ 1)
    takeC 3 .| mapC (+ 10)
    takeC 3 .| mapC (+ 100)

monadicCompositionExample' :: IO ()
monadicCompositionExample' = runConduit $ yieldMany [1..10] .| trans .| mapM_C print

yieldMany' :: (Num a, Monad m) => [a] -> ConduitT i a m ()
yieldMany' = mconcat . map yield

evalOrderExample :: IO ()
evalOrderExample =
    runConduit
        $ yieldMany [1..10]
        .| iterMC print
        .| liftIO (putStrLn "I was called")
        .| sinkNull
