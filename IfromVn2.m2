-- load "IfromVn2.m2"

-- Computes all "relevant" V in F_p, such that |V| <= p and V can be traced from the origin (0,0)

optionals = {numPts => p};

getVRepsN2 = optionals >> o -> p -> (if isPrime p then "" else print "p, the first argument, must be a prime number";
                                 if numPts <= p then "" else (print "numPts, the second argument, must be a positive integer less than or equal to p";
                                V := {};
                                return V;));

-- IfromVn2 computes I(V) from V where polyRing is a ring you can pass in, where n is the dimension of the points in V

optionals = {polyRing => R};

IfromVn2 = optionals >> o -> V -> (I := R;
        (for i from 0 to #V when i < #V 
            do (ptIdeal = ideal(x_1 - V#i#0, x_2 - V#i#1);
                 I = intersect(ptIdeal,I);));
         return I;);

createPtSymbol = n -> (symbolStr := "x1 - V#i";
--
                       (for i from 0 to n when i < n 
                            do (symbolStr = symbolStr | ", x" | toString i | " - V#i";)
                       )return "" | symbolStr;);

IfromV = optionals >> o -> (V,n) -> (I := R;
        symbolStr := createPtSymbol n;
--
        (for i from 0 to #V when i < #V 
            do (I = intersect(ptIdeal,I);));
         return I;)

-- dim R will get you n

-- write the point in V to the file above the line "ptIdeal = ideal(symbolStr);"
-- this will create the global symbol "ptIdeal" which you can reference in "I = intersect(ptIdeal,I);"