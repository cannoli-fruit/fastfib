import Data.Bits
import Data.Ratio
import Data.Time.Clock
import Control.DeepSeq
import System.IO (hFlush, stdout)

data Qadsqrt5 = Qadsqrt5 {
  phi :: Integer, -- Phi:  1.618
  phb :: Integer  -- PhiBar: -0.618
}

square :: Qadsqrt5 -> Qadsqrt5
square x =
  let ab2 = 2 * phi x * phb x
      a2 = phi x * phi x
      b2 = phb x * phb x
      s = a2 + b2 - ab2
  in
  Qadsqrt5 
    (s + a2)
    (s + b2)

phix :: Qadsqrt5 -> Qadsqrt5
phix x =
  let amb = phi x - phb x in
  Qadsqrt5 (phi x + amb) (amb)

phipow :: Integer -> Qadsqrt5
phipow n
  | n == 0 = Qadsqrt5 1 1
  | n == 1 = Qadsqrt5 1 0
  | even n = square q
  | otherwise = phix $ square q
  where
    q = phipow (shift n (-1))

main :: IO()
main = do
  line <- getLine
  let idx = read line :: Integer
  startTime <- getCurrentTime
  let y = phipow (idx - 1)
  let phiy = phi y
  phiy `deepseq` return ()
  endTime <- getCurrentTime

  -- putStrLn $ show phiy ++ "*phi + " ++ show phby ++ "*phb"
  putStrLn "Calculated!"
  putStrLn $ show (diffUTCTime endTime startTime)
  hFlush stdout

  let label = "Fib_" ++ show idx ++ ""
  writeFile (label ++ ".txt") $ label ++ " = " ++ show phiy
