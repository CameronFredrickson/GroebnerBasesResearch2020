G = groebnerBasis I

###################   S T A N D A R D   M O N O M I A L S   ###################

initialIdealGens = GB -> leadTerm GB

LTs = initialIdealGens G

for i from 0 to (m = numgens source LTs - 1) when i <= m list LTs_(0,i)

###############################################################################

initialIdealGens G








##################   M A X   L I S T   F I N D E R  2 D   #####################

monomialExpList = LTs -> for i from 0 to (m = numgens source LTs - 1) when i <= m list flatten exponents LTs_(0,i)

maxVarsExp = expsList ->   (max_x:=0;
                            max_y:=0; 
                            for i from 0 to (n = length expsList - 1) when i <= n do (
                                if expsList#i#0 > max_x then max_x := expsList#i#0; 
                                if expsList#i#1 > max_y then max_y := expsList#i#1;)
                            (max_x,max_y));
