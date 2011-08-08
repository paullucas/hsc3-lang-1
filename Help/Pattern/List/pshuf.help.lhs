shuf :: Enum e => e -> [a] -> Int -> [a]
pshuf :: Enum e => e -> [a] -> Int -> P a

Sequentially embed values in a list in constant, but random order.
Returns a shuffled version of the list item by item, with n repeats.

> shuf 'a' [1,2,3,4,5] 3
> pshuf 'a' [1,2,3,4,5] 3


