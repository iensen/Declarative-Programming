import Data.Ratio 

--probabilities of failures for each component
probFail comp 
    | comp <= 2  = 1 % 1000000  -- batteries  
    | comp <= 5  = 5 % 10000000 -- processors
    | comp <= 7  = 1 % 10000000 -- motor controllers  
    | comp <= 9  = 1 % 10000000 -- voters              
    | comp <= 12 = 1 % 10000000 -- wheel sensor  
    | comp <= 15 = 2 % 1000000  -- steer sensor

--probability that the set of components comps fails
probFailSet comps = foldr (*) 1 (map probFail comps) 
  
computePA4 = 
  let a4 = filter (\x ->  elem ( length x) [1..4] )  (subsets [1..15]) 
      in
   foldr (+) 0 (map probFailSet a4) 

--generate all subsets of a given set
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

