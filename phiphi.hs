import Data.Bits
import Data.Ratio
import Data.Time.Clock
import Control.DeepSeq

data Qadsqrt5 = Qadsqrt5 {
  phi :: Integer, -- Phi: 1.618
  phb :: Integer  -- PhiBar: -0.618
}

instance Num Qadsqrt5 where
  a + b = Qadsqrt5 (phi a + phi b) (phb a + phb b)
  a * b = undefined
  negate x = Qadsqrt5 (-phi x) (-phb x)
  abs x = undefined
  signum x = undefined
  fromInteger x = Qadsqrt5 (fromInteger x) (fromInteger x)

square :: Qadsqrt5 -> Qadsqrt5
square x =
  let ab2 = 2 * phi x * phb x
      a2 = phi x * phi x
      b2 = phb x * phb x
  in
  Qadsqrt5 
    (2 * a2 + b2 - ab2)
    (2 * b2 + a2 - ab2)

phix :: Qadsqrt5 -> Qadsqrt5
phix x =
  let amb = phi x - phb x in
  Qadsqrt5 (phi x + amb) (amb)

phbx :: Qadsqrt5 -> Qadsqrt5
phbx x =
  let bma = phb x - phi x in
  Qadsqrt5 (bma)  (phb x + bma)

phipow :: Integer -> Qadsqrt5
phipow n
  | n == 0 = 1
  | n == 1 = Qadsqrt5 1 0
  | even n = square q
  | otherwise = phix $ square q
  where
    q = phipow (shift n (-1))

phbpow :: Integer -> Qadsqrt5
phbpow n
  | n == 0 = 1
  | n == 1 = Qadsqrt5 0 1
  | even n = square q
  | otherwise = phbx $ square q
  where
    q = phbpow (shift n (-1))

fibonacci :: Integer -> Qadsqrt5
fibonacci x =
  phipow x - phbpow x

main :: IO()
main = do
  line <- getLine
  let idx = read line :: Integer
  startTime <- getCurrentTime
  let y = fibonacci idx
  let phiy = phi y
  let phby = phb y
  phiy `deepseq` return ()
  endTime <- getCurrentTime

  -- putStrLn $ show phiy ++ "*phi + " ++ show phby ++ "*phb"
  
  let label = "Fib_" ++ show idx ++ ""
  writeFile (label ++ ".txt") $ label ++ " = " ++ show phiy
  putStrLn $ show (diffUTCTime endTime startTime)
