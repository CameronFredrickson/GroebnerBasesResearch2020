-- Code to yield a print out similar to Anyu's DoEMS website for any prime power p and any dimension n


-- createLTs computes model bases from Gröbner bases for a given V
-- lenGB corresponds to the number Gröbner bases for a given V
-- GB is a list containing all Gröbner bases for a set V

-- createLTs = (GB,lenGB) -> (allLT := {};
--                            LT := {};
--
--                            (for i from 0 to lenGB when i < lenGB
--                                 do (LT = gfanLeadingTerms GB#i;
--                                     allLT = append(allLT, LT);)
--                            ) return allLT;);

-- createMBs computes model bases from Gröbner bases for a given V
-- lenLT corresponds to the number Gröbner bases for a given V
-- LT is a list of leading terms for each of the Gröbner bases corresponding to V

createMBs = (LT,lenLT) -> (allMB := {};
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

createTableElements = (allV,IfromV) -> (allGB := {};
                               allMB := {};
                               allLT := {};
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
                                       allLT = append(allLT, LT);
                                       MB = createMBs(LT, #LT);
                                       allMB = append(allMB, MB);)
                               ) return (allGB, allMB, allLT););

-- displayTable prints/(pipes to file) elements from getTableElements in a table format

optionals = {toFile => 0, fileName => "table.txt"};

displayTable = optionals >> o -> (allV,IfromV) -> (TableNet := "";
                                                   Vnumber := "0.";
                                                   (allGB, allMB, allLT) := createTableElements(allV,IfromV);
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
                                                             currentLTs := allLT#i;
                                                             LTsStr := toString currentLTs#0;
--
                                                             (for l from 1 to #currentLTs when l < #currentLTs
                                                                 do (LTsStr = LTsStr || toString currentLTs#l;));
--
                                                             currentGBs := allGB#i;
                                                             GBsStr := toString currentGBs#0;
--
                                                             (for m from 1 to #currentGBs when m < #currentGBs
                                                                 do (GBsStr = GBsStr || toString currentGBs#m;));
                                                             Vnumber = (toString i) | ".    ";
                                                             TableNet = TableNet || "\n" || Vnumber | toString (#currentMBs) | "   " | PtsStr | "   " | MBsStr | "   " | LTsStr | "   " | GBsStr;) 
                                                   )(if o.toFile == 1 then 
                                                        (f = openOut o.fileName;
                                                         f << TableNet;
                                                         close f;)
                                                     else print TableNet;))

-- How to retrieve and remove files:
-- get "table.txt"
-- remove "table.txt"

-- TableStr = 1; if toFile then (f = openOut "fileName"; f << TableStr; close f;) else print TableStr;