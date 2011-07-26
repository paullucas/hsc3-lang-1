zip :: [a] -> [b] -> [(a, b)]

> take 5 (zip (repeat 3) (repeat 4))

Stops on shortest pattern.

> zip [0 ..] [0,-1 .. -3]
