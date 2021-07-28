-- Functions to find a basic staircase for I that is not an initial staircase over ZZ/2 and display the search results

-- Returns 0 if the monomial m is present in the list and 1 if it is not

monomialSearch = (m, monomialList) -> (
--
    (for i from 0 to #monomialList when i < #monomialList
        do ((if m == monomialList#i then (return 0;)))
    );
    return 1;
);

-- queries whether or not the evaluation matrix evaluated on the monomials in potentialCandidate and the points in tempV is invertible

evalMatrixInvQ = (tempV, potentialCandidate) -> (
--
    ptsMatrix = matrix tempV;
    evalMatrix = sub(potentialCandidate, ptsMatrix);
--
    for i from 1 to numgens(target(ptsMatrix))-1 do(evalMatrix = evalMatrix || sub(potentialCandidate,ptsMatrix^{i}));
    return determinant(evalMatrix);
);

-- recursive function to pair the remaining points in V with more than a single 1 with monomials generating all basic staircases for I(tempV)

genBasicCandidates = (staircases, candidate, tempV, possibleMonomials) -> (
--
    if #tempV == 0 then (return append(staircases,candidate);)
    (while #tempV != 0 -- change this condition to searching through possible m
-- how are you going to generate m?
        do ((if monomialSearch(candidate,m) and sub(m, matrix {tempV#0}) != 0 and evalMatrix(tempV, append(candidate,m)) != 0 
             then (staircases = genBasicCandidates(staircases, append(candidate,m), drop(tempV, 1), delete(possibleMonomials, m));)))
    );
    return staircases;
);

-- generates all elements in a staircase of monomials that are nonzero on at least one point in V capped in degree by degreeLimit

genStaircaseElements = (degreeLimit, tempV) -> (
--
    possibleMonomials := {};
    (for i to #tempV

    )
    return possibleMonomials;
);

-- generates all basic staircases for a given ideal generated from a variety

genBasicStaircases = (V) -> (
--
    staircases := {};
    candidate := {1};
    degreeLimit := 2^#V;
    tempV := sort(V);    -- sorts V lexicographically (i.e. each point by number of 1s)
    i := 0;
--  
    tempV = drop(tempV, 1);     -- removes the point 0 from V
    (while i < #tempV
        do ((if sum(tempV#0) == 1 then (append(candidate,x_(i+1)); tempV = drop(tempV, 1))
             else if sum(tempV#0) > 1 then (break;)); 
             i = i + 1;)
    );
    possibleMonomials := genStaircaseElements(degreeLimit, tempV);
    staircases = genBasicCandidates(staircases, candidate, tempV, possibleMonomials);
    return staircases;
);

-- displayResults = (V) -> (
--
-- )