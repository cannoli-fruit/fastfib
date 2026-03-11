import Data.Bits
import Data.Ratio
import Data.Time.Clock
import Control.DeepSeq

data Qadsqrt5 = Qadsqrt5 {
  rat :: Rational,
  irr :: Rational
}

instance Num Qadsqrt5 where
  a + b = Qadsqrt5 (rat a + rat b) (irr a + irr b)
  a * b = Qadsqrt5
    (rat a * rat b + 5 * irr a * irr b)
    (irr a * rat b + rat a * irr b)
 
  negate x = Qadsqrt5 (-rat x) (-irr x)
  abs x = undefined
  signum x = undefined
  fromInteger x = Qadsqrt5 (fromInteger x) 0

instance Fractional Qadsqrt5 where
  recip (Qadsqrt5 c d) =
    let denom = c*c - 5*d*d
    in Qadsqrt5 (c / denom) (-d / denom)
  fromRational r =
    Qadsqrt5 (fromRational r) 0

pow :: Qadsqrt5 -> Integer -> Qadsqrt5
pow x n
  | n == 0 = 1
  | n == 1 = x
  | even n = q*q
  | otherwise = x*q*q
  where
    q = pow x (shift n (-1))

fibonacci :: Integer -> Qadsqrt5
fibonacci x =
  let power = x
      r5 = Qadsqrt5 0 1
      phi = (1 + r5) / 2
      phi2 = (1 - r5) / 2
  in
    ( (pow phi power) - (pow phi2 power) ) / r5

main :: IO()
main = do
  line <- getLine
  let idx = read line :: Integer
  startTime <- getCurrentTime
  let y = fibonacci idx
  let num = numerator $ rat y
  num `deepseq` return ()     -- force computation
  endTime <- getCurrentTime
  -- let den = denominator $ rat y
  --
  -- let numi = numerator $ irr y
  -- let deni = denominator $ irr y
  -- putStrLn $ "Fib[" ++ show idx ++ "] = " ++ show num ++ "/" ++ show den ++ " + sqrt(5)*(" ++ show numi ++ "/" ++ show deni ++ ")"
  let label = "Fib_" ++ show idx ++ ""
  writeFile (label ++ ".txt") $ label ++ " = " ++ show num
  putStrLn $ show (diffUTCTime endTime startTime)
