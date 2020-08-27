-- Code to yield a print out similar to Anyu's DoEMS website for any prime power p and any dimension n

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
                       --allLT := {};
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
                               --allLT = append(allLT, LT);
                               MB = createMBs(LT, #LT);
                               allMB = append(allMB, MB);)
                       ) return (allGB, allMB););

-- displayTable prints/(pipes to file) elements from getTableElements in a table format

optionals = {toFile => 0, fileName => "table.txt"};

displayTable = optionals >> o -> (allV,IfromV) -> (
--              
               TableNet := "";
               Vnumber := "1.";
               (allGB, allMB) := createTableElements(allV,IfromV);
               f := 0;
--
               (for i from 0 to #allV when i < #allV
                     do (currentV := allV#i;
                         PtsStr := toString currentV#0;
--
                         (for j from 1 to #currentV when j < #currentV
                            do (PtsStr = PtsStr || toString currentV#j;));
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
                         (for m from 1 to #currentGBs when m < #currentGBs
                             do (markedGBstr = toString currentGBs#m;
                                 strToSeparate = substring(21,(#markedGBstr - 22), markedGBstr);
                                 separatedStrs = separate("},", strToSeparate);
                                 LTsStr = LTsStr || (separatedStrs#0 | "}");
                                 GBsStr = GBsStr || separatedStrs#1;));
--
                         Vnumber = (toString (i+1)) | ".    ";
                         TableNet = TableNet || "\n" || Vnumber | toString (#currentMBs) | "   " | PtsStr | "   " | MBsStr | "   " | GBsStr;) 
                         );(if o.toFile == 1 then        -- That ');(' semicolon matters :/
                              (f = openOut o.fileName;
                               f << TableNet;
                               close f;)
                            else print TableNet;))

-- How to retrieve and remove files:
-- get "table.txt"
-- remove "table.txt"