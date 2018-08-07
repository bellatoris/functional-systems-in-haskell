x = 2
y = 3

main = let z = x + y
       in print z

add arg1 arg2 = arg1 + arg2
five = add 2 3

main2 = print (add 2 3)

safeDiv x y = 
    let q = div x y
    in if y == 0 then 0 else q

main3 = print (safeDiv 3 0)

-- main4 = 
--     let x = x + 1
--     in print x
--

factorial :: (Ord p, Num p) => p -> p
factorial n = 
    if n > 1
    then n * factorial (n - 1)
    else 1

factorial2 n =
    let loop acc n' = if n' > 1
                      then loop (acc * n') (n' - 1)
                      else acc
    in loop 1 n

factorial3 n = let loop acc n' | n' > 1 = loop (acc * n') (n' - 1)
                               | otherwise = acc
               in loop 1 n
