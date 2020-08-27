-- Code to yield a print out similar to Anyu's DoEMS website for any prime power p and any dimension n

-- IfromVn2 computes I(V) from V where polyRing is a ring you can pass in, where n is the dimension of the points in V

optionals = {polyRing => R};

IfromVn2 = optionals >> o -> V -> (I := R;
        (for i from 0 to #V when i < #V 
            do (ptIdeal = ideal(x_1 - V#i#0, x_2 - V#i#1);
                 I = intersect(ptIdeal,I);));
         return I;);

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
-- IfromV is the function used to create I(V) this currently varies based on n, i.e. IfromVn2 and IfromVn3

-- call like this: "(allGB, allMB) = createTableElements allV"

createTableElements = (allV,IfromV) -> (
--
                       allGB := {};
                       allMB := {};
                       staircase := {};
                       GB := {};
                       MB := {};
                       LT := {};
                       lenAllV := #allV;
--                               
                       (for i from 0 to lenAllV when i < lenAllV
                           do (I = IfromV allV#i;
                               GB = gfan I;
                               allGB = append(allGB, GB);
                               LT = gfanLeadingTerms(GB, "m" => true); --computes the LTs for each set, returning a set of sets
                               MB = createMBs(LT, #LT);
                               allMB = append(allMB, MB);)
                       ) return (allGB, allMB););

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

displayTable = optionals >> o -> (allV,IfromV) -> (
--              
               TableNet := "";
               maxVlen := findMaxVstrLen allV;
               colVMB := (createRepeatStr(" ", 10)) | "V" | (createRepeatStr(" ", (maxVlen + 2))) | "MBs";
               colLT := "LTs";
               colGB := "GBs";
               (allGB, allMB) := createTableElements(allV,IfromV);
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
                         (for k from 1 to #currentMBs when k < #currentMBs
                            do (MBsStr = MBsStr || toString currentMBs#k;));
--
                         currentGBs := allGB#i;
                         markedGBstr := toString currentGBs#0;
--                         
                         strToSeparate := substring(21, (#markedGBstr - 22), markedGBstr); -- removes 'MarkedPolynomialList{' and '}' at end
-- 
                         separatedStrs := separate("},", strToSeparate); -- separates strToSeparate into LTs and GB
--                         
                         LTsStr := separatedStrs#0 | "}";
                         GBsStr := separatedStrs#1;
--
                         (for l from 1 to #currentGBs when l < #currentGBs
                             do (markedGBstr = toString currentGBs#l;
                                 strToSeparate = substring(21,(#markedGBstr - 22), markedGBstr);
                                 separatedStrs = separate("},", strToSeparate);
                                 LTsStr = LTsStr || (separatedStrs#0 | "}");
                                 GBsStr = GBsStr || separatedStrs#1;));
--
                         colVMB = colVMB || "\n" || ((toString (i+1)) | ".    " | toString (#currentMBs) | "   " | PtsStr | "   " | MBsStr);
                         colLT = colLT || "\n" || LTsStr;
                         colGB = colGB || "\n" || GBsStr;)
                         ); TableNet = colVMB | "   " | colLT | "   " | colGB; -- first ';' on line must be there or 'null SPACE null' error
                         (if o.toFile == 1 then
                              (fileDescriptor = openOut o.fileName;
                               fileDescriptor << TableNet;
                               close fileDescriptor;)
                            else print TableNet;))

-- How to retrieve and remove files:
-- get "table.txt"
-- remove "table.txt"

-- Below is a test set for R = ZZ/3[x_1,x_2]; 

-- allV = {{{0, 0}},{{0, 0}, {0, 1}},{{0, 0}, {1, 0}},{{0, 0}, {1, 1}},{{0, 0}, {0, 1}, {0, 2}},{{0, 0}, {0, 1}, {1, 0}},{{0, 0}, {1, 0}, {2, 0}},{{0, 0}, {0, 1}, {1, 2}},{{0, 0}, {1, 0}, {2, 1}},{{0, 0}, {1, 1}, {2, 2}},{{0, 0}, {0, 1}, {0, 2}, {1, 0}},{{0, 0}, {0, 1}, {1, 0}, {1, 1}},{{0, 0}, {0, 1}, {1, 0}, {2, 0}},{{0, 0}, {0, 1}, {1, 0}, {1, 2}},{{0, 0}, {0, 1}, {1, 0}, {2, 1}},{{0, 0}, {0, 1}, {1, 0}, {2, 2}},{{0, 0}, {0, 2}, {1, 1}, {2, 1}}};