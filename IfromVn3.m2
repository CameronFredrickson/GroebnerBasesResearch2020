-- load "IfromVn3.m2"

-- Computes all "relevant" V in F_p, such that |V| <= p and V can be traced from the origin (0,0,0)

optionals = {numPts => p};

getVRepsN3 = optionals >> o -> p -> (if isPrime p then "" else print "p, the first argument, must be a prime number";
                                 if numPts <= p then "" else (print "numPts, the second argument, must be a positive integer less than or equal to p";
                                V := {};
                                return V;));

-- IfromVn3 computes I(V) from V where polyRing is a ring you can pass in 

optionals = {polyRing => R};

IfromVn3 = optionals >> o -> V -> (I := R;
        (for i from 0 to #V when i < #V 
            do  (ptIdeal = ideal(x - V#i#0, y - V#i#1, z - V#i#2);
                 I = intersect(ptIdeal,I);));
         return I;);
