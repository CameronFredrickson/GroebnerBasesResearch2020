-- Code to yield a print out similar to Anyu's DoEMS website for any prime power p and any dimension n


-- IfromV computes I(V) from V assuming R is defined as the polynomial ring your ideal is a subset of 
-- Also this function assumes your variables in R are defined in the form "x_i" when you declare R := k[x_1..x_5];
-- V is an affine variety and idealName is a string
-- in the form of a string, code is written to the file "idealName.m2" to create the global variable idealName

IfromV = (V,idealName) -> (
--
                symbolStr  := "x_1 - tempV#i#0";  -- string needed to create the ideal when calling intersect
                initialStr := "x_1";              -- string needed to create the ideal prior to calling intersect
                n          := dim R;              -- the dimension of the points in tempV
                file       := idealName | ".m2";
--
                (for i from 1 to n when i < n 
                     do (initialStr = initialStr | ", x_" | toString(i+1);));
                file << idealName << " = ideal(" << initialStr << ");" << endl; -- for the initial intersection performed below
                file << "tempV = " << (toString V) << ";" << endl;
                file << "(for i from 0 to #tempV when i < #tempV" << endl << "    d" << "o ("; -- do in string does not compile :/
--
                (for i from 1 to n when i < n 
                     do (symbolStr = symbolStr | ", x_" | toString(i+1) | " - tempV#i#" | toString i;));
--
                file << idealName << " = intersect(" << idealName << ", ideal(" << symbolStr << "));));" << close;
                load file;);
--              removeFile file;);

-- createMBs computes model bases from Gröbner bases for a given V
-- lenLT corresponds to the number Gröbner bases for a given V
-- LT is a list of leading terms for each of the Gröbner bases corresponding to V

createMBs = (LT,lenLT) -> (
--
             allMB := {};
             MB := {};
             T := {};
--
             (for i from 0 to lenLT when i < lenLT
                  do (staircase = ideal LT#i;
                      T = R/staircase;
                      MB = flatten entries basis T; -- infinite basis error?!
                      allMB = append(allMB, MB);
                      use R;)
             ) return allMB;);

-- createTableElements generates ideals from a collection of sets V and their corresponding Gröbner and Model Bases
-- IfromV is the function used to create I(V)

-- call like this: "(allGB, allMB) = createTableElements allV"

createTableElements = allV -> (
--
                      allGB := {};
                      allMB := {};
                      allLT := {};
                      staircase := {};
                      GB := {};
                      MB := {};
                      LT := {};
                      lenAllV := #allV;
--                               
                      (for i from 0 to lenAllV when i < lenAllV
                          do (IfromV(allV#i,"I");
                              GB = gfan I;
                              allGB = append(allGB, GB);
                              LT = gfanLeadingTerms(GB, "m" => true); --computes the LTs for each set, returning a set of sets
                              allLT = append(allLT, LT);
                              MB = createMBs(LT, #LT);
                              allMB = append(allMB, MB);)
                      ) return (allGB, allMB, allLT););

-- findMaxVstrLen converts each set V to a string and find the "max" V by the number of characters contained in its string representation

findMaxVstrLen = allV -> (
--                  
                 maxLen := 0; Vlen := 0;
--
                 (for i from 0 to #allV when i < #allV
                    do (Vlen = #(toString allV#i);
                        if Vlen > maxLen then maxLen = Vlen;));
                 return maxLen;);

-- createRepearStr creates a string of n char

createRepeatStr = (char,n) -> (
--
                  str := "";
                  (for i from 0 to n when i < n
                    do (str = str | char;));
                  return str;);

-- displayTable prints/(pipes to file) elements from getTableElements in a table format

optionals = {toFile => 0, fileName => "table.txt"};

displayTable = optionals >> o -> allV -> (
--              
               TableNet := "";
               maxVlen := findMaxVstrLen allV;
               colVMB := (createRepeatStr(" ", 10)) | "V" | (createRepeatStr(" ", (maxVlen + 2))) | "MBs";
               colLT := "LTs";
               colGB := "GBs";
               (allGB, allMB, allLT) := createTableElements allV;
               fileDescriptor := 0;
--
               (for i from 0 to #allV when i < #allV
                     do (currentV := allV#i;
                         currentVstr := toString currentV;
                         PtsStr := currentVstr | (createRepeatStr(" ", (maxVlen - #currentVstr)));
--
                         currentMBs := allMB#i;
                         MBsStr := toString currentMBs#0;
--
                         currentGBs := allGB#i;
                         GBsStr := toString currentGBs#0;

                         currentLTs := allLT#i;
                         LTsStr := toString currentLTs#0;   
--
                         (for k from 1 to #currentGBs when k < #currentGBs
                             do (MBsStr = MBsStr || toString currentMBs#k;
                                 GBsStr = GBsStr || toString currentGBs#k;
                                 LTsStr = LTsStr || toString currentLTs#k;));
--
                         colVMB = colVMB || "\n" || ((toString (i+1)) | ".    " | toString (#currentMBs) | "   " | PtsStr | "   " | MBsStr);
                         colLT = colLT || "\n" || LTsStr;
                         colGB = colGB || "\n" || GBsStr;)
                         ); TableNet = colVMB | "   " | colLT | "   " | colGB; -- first ';' on line 141 must be there or 'null SPACE null' error
                         (if o.toFile == 1 then
                              (fileDescriptor = openOut o.fileName;
                               fileDescriptor << TableNet;
                               close fileDescriptor;)
                            else print TableNet;))

-- How to retrieve and remove files:
-- get "table.txt"
-- remove "table.txt"

-- Below is a test set for R = ZZ/3[x_1,x_2] where allV contains ALMOST all distinct varieties of ZZ/3^2 with respect to linear shifts and complement rules on the size of V  

-- To run the test, first start M2 in the directory containing this file

-- Then copy and paste the lines below one at a time without the "-- " at the beginning of each line 

-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

-- loadPackage "gfanInterface";

-- load "AnyuTable.m2";

-- R = ZZ/3[x_1,x_2];

-- allV = {{{0, 0}},{{0, 0}, {0, 1}},{{0, 0}, {1, 0}},{{0, 0}, {1, 1}},{{0, 0}, {0, 1}, {0, 2}},{{0, 0}, {0, 1}, {1, 0}},{{0, 0}, {1, 0}, {2, 0}},{{0, 0}, {0, 1}, {1, 2}},{{0, 0}, {1, 0}, {2, 1}},{{0, 0}, {1, 1}, {2, 2}},{{0, 0}, {0, 1}, {0, 2}, {1, 0}},{{0, 0}, {0, 1}, {1, 0}, {1, 1}},{{0, 0}, {0, 1}, {1, 0}, {2, 0}},{{0, 0}, {0, 1}, {1, 0}, {1, 2}},{{0, 0}, {0, 1}, {1, 0}, {2, 1}},{{0, 0}, {0, 1}, {1, 0}, {2, 2}},{{0, 0}, {0, 2}, {1, 1}, {2, 1}}};

-- displayTable(allV);

-- OR 

-- displayTable(allV, toFile => 1, fileName => "yourChoice.txt");

-- get "yourChoice.txt" 

-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

-- "yourChoice.txt" can be found in the directory you started M2 in