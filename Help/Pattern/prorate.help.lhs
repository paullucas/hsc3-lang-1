prorate

Divide stream proportionally

 proportions - a pattern that returns either numbers (divides the
               pattern into pairs) or arrays of size n which are used
               to split up the input into n parts.
     pattern - a numerical pattern

> let p = prorate (pseq [0.35, 0.5, 0.8] 1) 1
> in pureP p
