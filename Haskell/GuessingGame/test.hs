import Data.Complex
main = print $ sum $ (^2) . (:+ 10) <$> [1..10000000]
