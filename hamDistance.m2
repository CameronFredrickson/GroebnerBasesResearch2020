
-- data set, D, our variety

-- D = {{1,0},{1,1},{0,0}}

-- Compute Hamming distance between 2 points in ZZ/2[x1..xn]

hamDis = pairPts -> (p = pairPts#0;
                     q = pairPts#1;
                     distance = 0;
                     for i from 0 to (n = length p) when i < n do (
                                if xor(p#i,q#i) then distance = distance + 1;
                     ); 
                     distance;)
