-- Code to create the LaTeX needed to create a visual representation of any 2D variety from a finite field and label it as a variety that is diagonal-free w/o a UGB, 
-- a linear shift of a staircase, or not a linear shift of a staircase but has a UGB


loadPackage("gfanInterface", Reload => true);


-- createRepearStr creates a string of n char

createRepeatStr = (char,n) -> (
--
                  str := "";
                  (for i from 0 to n when i < n
                    do (str = str | char;));
                  return str;);


-- createPoints creates all points in the finite vector space over a finite field of characteristic q and dimension n by writing the necessary code to a file and then
--              storing the result in the global variable allPoints

createPoints = (n, q) -> (
--              
                file := "tmpCreatePointsCode.m2";
                symbolStr := "{i0}";
--
                file << "allPoints = ((n, q) -> (" << endl; -- storing all points created in the global variable allPoints
                file << createRepeatStr(" ", 12) << "allPts := {};" << endl; -- storing all points created in the local variable allPts to be returned
                file << "--" << endl << createRepeatStr(" ", 12) << "(";
--                
                (for i from 0 to n when i < n
                    do (file << "fo" << "r i" << i << " from 0 to q when i" << i << " < q" << endl;
                        file << createRepeatStr(" ", (12 + ((i + 1) * 4))) << "d" << "o (";););
--
                (for i from 1 to n when i < n
                    do (symbolStr = symbolStr | " | " | "{i" | i | "}"));
                file << "allPts = append(allPts, (" << symbolStr << ")));" << endl;
                file << createRepeatStr(" ", 12) << createRepeatStr(")", n) << " return allPts;)) (" << n << ", " << q << ");" << close;
                load file;);
--              removeFile file;);


-- createAllVs creates all affine varieties up to complement, meaning for a finite vector space over a finite field of characteristic q and dimension n
-- this function computes all V shifted to the origin, meaning each V contains 0, such that |V| <= q^n/2 
-- assuming R is defined as the polynomial ring, call as "createAllVs(dim R, char R)"
-- m  is an optional parameter to force all computed Vs to have m points
-- Note 3 <= |V| because a general formula is known for the number of RGBs of I(V) where V has exactly 2 points in n dimensions

createAllVs = {m => 0} >> o -> (n , q) -> (
--
                allVs := {};
--
                createPoints(n, q);
--
                if n == 2 and q == 2 then return subsets(allPoints, 2);
--
                allPoints = delete(toList (n:0), allPoints);
--
                if o.m > 0 then return apply(subsets(allPoints, o.m - 1), (L -> prepend(toList (n:0), L)));
--
                upperBound = floor(q^n/2);
                (for i from 2 to upperBound when i < upperBound
                  do (allVs = join(allVs, apply(subsets(allPoints, i), (L -> prepend(toList (n:0), L))));)
                ) return allVs;);


-- HammingDistance computes the Hamming Distance between the points v and w i.e. returns the number of differing coordinates between the 2 points
-- assuming R is defined as the polynomial ring, call like HammingDistance(v, w, dim R);

HammingDistance = (v, w, n) -> (
--
                Hdist := 0;
--
                if #v != #w then return "Error: The arguments v and w need to be lists of the same length.\n";
                (for i from 0 to n when i < n
                  do (if v#i != w#i then Hdist = Hdist + 1;)
                ) return Hdist;);


-- DiagonalFreeCheck uses Hamming Distance to check if a variety is diagonal-free where n is the dimension of the points in the variety

DiagonalFreeCheck = (V, n) -> (
--
                m := #V;
                diag := false;
--
                (for i from 0 to n when i < m
                  do (for j from 0 to n when j < m
                        do (if i != j then (if HammingDistance(V#i, V#j, n) <= 1 then (diag = false; break;) else diag = true));
                        if diag then return "N";);
                ) return "Y";);


-- StaircaseCheck2D determines whether or not the 2 dimensional affine variety V is a staircase or not
--                  starting with the point in V with the largest x value and then largest y value for points with that x value the function interates
--                  through the points "underneath" it by subtracting 1 from the y value with each iteration and checking if this point is in V.

StaircaseCheck2D = (V, n) -> (
--
                toCheck := rsort(V);
                m       := 0;
                minY    := -1;  -- forces minY to be properly intialized and run through the loop in all subsequent interations without initializing outside the loop
                lastX   := 0;
                flag    := 0;
--
                (while m < (#toCheck - 1)
                  do (currentPt := toCheck#m; 
                      if currentPt#1 < minY then return false else minY = currentPt#1;
                      lastX = currentPt#0;
--
                      for j from 1 to currentPt#1 when j <= currentPt#1
                        do (flag = 1;
                            if toCheck#(m + 1) != currentPt - {0, j} then return false else m = m + 1;
                            );
                      if flag == 0 then m = m + 1 else flag = 0;
                      if lastX > 0 and toCheck#m#0 != lastX - 1 then return false;);
                ); return true;);


-- If V is a linear shift of a staircase LinearShift2D returns a tuple containing 3 elements, true or false if V is a linear shift of a staircase, the linear shift as a 
-- string if true, and the unshifted verision of V if true

LinearShift2D = (V, n, q) -> (
                shiftedV := {};
                b        := {};
--
                (for m from 0 to #V when m < #V
                  do (for a from 1 to q when a < q
                        do (for i from 0 to n when i < n
                              do (b = append(b, ((q - a*V#m#i) % q))); -- creates the additive inverse of the current point V#m to shift it to the orgin
--
                            shiftedV = entries (matrix apply(V, j -> a*j + b)); -- applies the shift to all points in V
                            shiftedV = applyTable(shiftedV, i -> i % q);        -- makes all entries nonnegative mod q, using mod(i, q) returns the class 
                                                                                -- ZZ/q where 2 < 0 is true, however i % q returns class ZZ, where 2 < 0 is false
--
                            if StaircaseCheck2D(shiftedV, n) then return (true, "Φ(v) = " | a | "v + " | (toString b), shiftedV);
                            b = {};))); 
                 return (false, "not a linear shift of a staircase", {}););


-- create2DLatexEntry returns the LaTeX to be copy pasted into a LaTeX file to label a variety as diagonal-free w/o a UGB, a linear shift of a staircase, 
--                    or not a linear shift of a staircase but has a UGB and creates a Tikz picture of the variety
--                    Note: /// allows "\" to be treated as is and not as an ecsape character

create2DLatexEntry = method(Options => {SMexponents => 0});
 
create2DLatexEntry(List, ZZ, ZZ, String) := (V, p, i, label) -> (  -- something is fishy with 'o -> ', 'o.SMexponents' does not work
--
                entry   := ///\begin{tabular}{ c c @ {\hskip 0.75 cm} c }/// | "\n\t" | (toString i) | ". & " | (toString label) | " & " | (toString V#0#0) | " " | (toString V#0#1) | " ";  -- /// allows "\" to be treated as is and not as an ecsape character
                Vcolor  := if odd(i) then "tetradicBlue" else "tetradicRed";
                SMcolor := if even(i) then "tetradicOrange" else "green";
--
                (for m from 1 to #V when m < #V
                  do (entry = entry | ///\\\n &    & /// | (toString V#m#0) | " " | (toString V#m#1) | " ";));
--
                entry = entry | "\n" | ///\end{tabular} \hspace{0.25cm} \begin{tikzpicture}[baseline=(current bounding box.center), roundnode/.style={circle, draw=/// | Vcolor | "!60, fill=" | Vcolor | "!5, very thick, minimum size=5mm}, scale=1.2]" | "\n % Lines \n" | ///\draw (0,0) grid (/// | (toString p) | ", " | (toString p) | ");\n\n % Nodes\n";
--
                (for m from 0 to #V when m < #V
                  do (entry = entry | ///\node[roundnode] at (/// | (toString V#m#0) | ", " | (toString V#m#1) | ") {};\n"));
--
                entry = (toString entry) | ///\end{tikzpicture}///;  -- did not compile with "entry | ///\end{tikzpicture}///" needed "(toString entry) | ///\end{tikzpicture}///";
                if (options create2DLatexEntry).SMexponents == 0 then (return entry | ///\n\n\vspace{10mm}\n\n///;) else (return "still need to write code for the SMs";);  -- if SMexponents == 1 then create entry for SMs
);


-- create2DLatexFile writes the LaTeX to a file so that each variety in allVs has a visual representation and each variety is labeled as diagonal-free w/o a UGB, 
--                   a linear shift of a staircase, or n`ot a linear shift of a staircase w/ a UGB. The .tex file will be titled "p(char R)m(value of m).tex" if m is nonzero
--                   and no file name is given as an argument where m corresponds to the number of points in the varieties to be looked at if m is nonzero. 
--                   If m is equal to zero and no file name is given the file will be titled "p(char R).tex" i.e. so if char R returns 5, the corresponding .tex file 
--                   would be titled "p5.tex". The filter will allow the caller to only return the varieties with a specified property and the filter will be added to 
--                   the end of the file name if no file name is given. If filter == 0 then the LaTeX will not be filtered. If filter == 1
--                   then only the LaTeX corresponding to DF varieties w/o a UGB will be returned. If filter == 2 then only the LaTeX corresponding to varieties w/ UGBs
--                   are not linear shifts of staircases will be returned. 

create2DLatexFile = {FileName => 0, a => 0, filter => 0} >> o -> (allVs, p, n) -> (
--
                label       := "   ";
                entry       := "";
--
                if FileName == 0 then (FileName = "p" | p; 
                                       if m > 0 then FileName = FileName | "m" | m;
                                       if filter == 0 then FileName = FileName | ".tex"
                                       else if filter == 1 then FileName = FileName | "-DFnotUGB.tex"
                                       else if filter == 2 then FileName = FileName | "-UGBnotStaircase.tex";);
--
                fileString  := ///\documentclass[12 pt]{article} 
  
                          \usepackage[utf8]{inputenc}
                          \usepackage{amsfonts,amssymb,amsmath}
                          \usepackage{graphicx,tikz}
                          \usepackage{adjustbox}
                          \usepackage{geometry}
                              \geometry{
                                  a4paper,
                                  total={170mm,257mm},
                                  left=20mm,
                                  top=15mm,
                              }
                          \usepackage{tabularx}
                          \usepackage{xcolor}
                          \definecolor{tetradicRed}{RGB}{255, 102, 102}
                          \definecolor{tetradicOrange}{RGB}{255, 179, 102}
                          \definecolor{tetradicBlue}{RGB}{102, 102, 255}
                          \newcommand{\DF}{{\color{red}\(\mathbb{DF}\)}}
                          \newcommand{\St}{\fcolorbox{blue}{white}{{\color{blue}\S}}}
  
                          \title{Varieties in \(\mathbb{F}^/// | (toString n) | "_" | (toString p) | ///\)}
                          \date{}
                          \author{}
  
  
                          \begin{document}
  
                          \maketitle
  
                          \noindent
  
                          \vspace{-10mm}
  
                          \begin{tabular}{ l c l }
                               UGB & - & unique reduced Gr{\"o}bner Basis \\
                                DF & - & diagonal-free variety \\
                               \DF & - & diagonal-free variety yielding more than one GB \\
                                 S & - & linear shift of a staircase variety \\
                              \St & - & a variety that is not a linear shift of a staircase and yields a UGB
                          \end{tabular}
  
                          \vspace{10mm}
  
                          * Recall UGB \(\implies\) DF and S \(\implies\) UGB
  
                          \vspace{10mm}

                          ///;
-- need to compute GBs to determine label
                (for num from 0 to #allVs when num < #allVs
                   do (create2DLatexEntry(allVs, p, n, num, label)););
);




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
                              LT = gfanLeadingTerms(GB, "m" => true); -- computes the LTs for each set, returning a set of sets
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


-- displayTable prints/(pipes to file) elements from getTableElements in a table format

optionals = {toFile => 0, fileName => "table.txt"};

displayTable = optionals >> o -> (allV, n, q) -> (
--              
               TableNet := "";
               maxVlen := findMaxVstrLen allV;
               colHeaders := "      Type    "| (createRepeatStr(" ", 10)) | "V" | (createRepeatStr(" ", (maxVlen + 2))) | "MBs";
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


                         type := "NA "; 
                         (isStaircase, message, unshifted) := LinearShift2D(currentV, n, q);

                         print (isStaircase, message, unshifted);

                         (if isStaircase == true then type = "S  "
                          else if #currentGBs == 1 then type = "UGB"
                          else if DiagonalFreeCheck(currentV, n) == "Y" then type = "DF ");
--
                         (for k from 1 to #currentGBs when k < #currentGBs
                             do (MBsStr = MBsStr || toString currentMBs#k;
                                 GBsStr = GBsStr || toString currentGBs#k;
                                 LTsStr = LTsStr || toString currentLTs#k;));
--
                         colHeaders = colHeaders || "\n" || ((toString (i+1)) | "." | createRepeatStr(" ", (4 - floor(log_10(i + 1)))) | type | "   " | PtsStr | "   " | MBsStr);                     -- log_10(i + 1) is to remove the number of spaces corresponding to the number of the digits of the row number
                         )
                ); TableNet = colHeaders; -- first ';' on line 141 must be there or 'null SPACE null' error
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