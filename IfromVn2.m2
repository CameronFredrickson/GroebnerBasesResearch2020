-- Computes all "relevant" V in F_p, such that |V| <= p and V can be traced from the origin (0,0)

optionals = {numPts => p};

getVRepsN2 = optionals >> o -> p -> (if isPrime p then "" else print "p, the first argument, must be a prime number";
                                 if numPts <= p then "" else (print "numPts, the second argument, must be a positive integer less than or equal to p";
                                V := {};
                                return V;));

-- IfromV computes I(V) from V where polyRing is a ring you can pass in
-- a global variable idealName is created from the variety V where idealName is a string
-- the code used to create idealName is written in the file "idealName.m2" where idealName is an argument to the function

IfromV = (V,idealName) -> (
--
                symbolStr := "x_1 - tempV#i";
                n := dim R; -- the dimension of the points in tempV
                file := idealName | ".m2";
--
                file << idealName << " = R;" << endl;
                file << "tempV = " << (toString V) << ";" << endl;
                file << "(for i from 0 to #tempV when i < #tempV" << endl << "    d" << "o"; -- do in string does not compile :/
--
                (for i from 1 to n when i < n 
                     do (symbolStr = symbolStr | ", x_" | toString i | " - tempV#i";));
--
                file << "(idealName = ideal(" << symbolStr << ");" << endl;
                file << idealName << " = intersect(idealName," << idealName << ");));" << close;
                load file;);