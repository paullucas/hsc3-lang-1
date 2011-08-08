slide :: [a] -> Int -> Int -> Int -> Int -> Bool -> [a]
pslide :: [a] -> Int -> Int -> Int -> Int -> Bool -> P a

      data - list of elements
   repeats - number of segments
    length - length of each segment
      step - increment to shift each segment, can be negative
     start - index to start at
      wrap - must be True

> import Sound.SC3.Lang.Pattern.List as P

> slide [1,2,3,4,5] 6 3 1 0 True
> slide [1,2,3,4,5] 6 3 (-1) 0 True

> pslide [1,2,3,4,5] 6 3 1 0 True
> pslide [1,2,3,4,5] 6 3 (-1) 0 True
