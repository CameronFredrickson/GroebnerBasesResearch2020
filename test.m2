maxVarsExp = expsList ->   (maxX:=0;
                            maxY:=0; 
                            for i from 0 to (n = length expsList - 1) when i <= n do (
                                if expsList#i#0 > maxX then maxX = expsList#i#0; 
                                if expsList#i#1 > maxY then maxY = expsList#i#1;);
                            (maxX,maxY));
