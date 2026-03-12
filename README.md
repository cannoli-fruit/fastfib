# Fibonacci, But fast
I watched the video series by (g+)+ [https://www.youtube.com/watch?v=KzT9I1d-LlQ&list=PL3JI_Wj02ehVoNlvqVbCrxdwE2sWmDO8O] and it looked like fun, considering i'm "learning" haskell at the moment i thought i'd try to do something in haskell, i decided to use binet's formula because it seemed easiest and i beat him by a pretty large margin, based on what i know it seems like gmp is just really fast although i think it's using the same fft multiplication algorithm that he was at some point using

with my "tests" i ran 50M on my script which ran about 1.6s on my i5-6600 while (g+)+'s 5M ran about 2.5s and i was too impatient to check 50M on his
EDIT: Added karatsuba-type multiplication and mine runs in around 1.3s now
EDIT: Changed the way numbers were represented, about 0.75s now

## Quickstart?
run:
ghc fastfib.hs
echo "\[NUM\]" | ./fastfib
