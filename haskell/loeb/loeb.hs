
import Data.List

loeb :: Functor f => f ( f w -> w) -> f w
loeb fs = let xs = fmap ($ xs) fs in xs

exemples :: [[[Integer] -> Integer]]
exemples = [ [genericLength, const 2]
           , (sum . tail) : (take 40 $ map const [1,3..])
           , const 1 : map (\i -> \xs -> i * foldr (*) 1 (genericTake i xs)) [1..]
           ]

maxLength = 10
main = mapM (print . take maxLength. loeb) exemples
