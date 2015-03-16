
(* HavelHakimi*)

(* 
reduceDegrees[degreeLst_]
degreeLst \[Rule] type: List
degreeLst must be in non-increasing order
returns a new list where each degree is one less than the previous list
*)
reduceDegrees[degreeLst_] := Map[# - 1 &,  degreeLst]

(*
degreeSumEvenQ[degreeLst_]
degreeLst \[Rule] type: List
degreeLst must be in non-increasing order
returns true if the sum of the degrees in degreeLst is even 
*)
degreeSumEvenQ[degreeLst_] := EvenQ[Total[degreeLst] ]

(*
positiveDegQ[degreeLst_]
degreeLst \[Rule] type: List
degreeLst must be in non-increasing order
returns true if the last degree is positive or 0
*)
positiveDegQ[degreeLst_] := Not[Negative[Last[degreeLst] ] ]

(*
validDegreeQ[degreeLst_]
degreeLst \[Rule] type: List
degreeLst must be in non-increasing order
returns false if the first degree \[GreaterEqual] the number of items \
in the list, this would mean that there are not enough vertices in \
the graph for the first vertex to connect to
*)
validDegreeQ[degreeLst_] := 
 If[
  First[degreeLst] >= Length[degreeLst],
  False,
  True
  ]

(*
graphicQ[degreeLst_]
degreeLst \[Rule] type: List
degreeLst must be in non-increasing order
returns true if degreeSumEvenQ returns true and negativeDegQ returns \
false
*)
graphicQ[degreeLst_] := 
 degreeSumEvenQ[degreeLst] && positiveDegQ[degreeLst]  && 
  validDegreeQ[degreeLst] 

(*
havelHakimiReduce[degreeLst_]
degreeLst \[Rule] type: List
degreeLst must be in non-increasing order
returns a new list constructed according to the Havel-Hakimi \
algorithm in non-increasing order, returned list is not guaranteed to \
be graphic
*)
havelHakimiReduce[degreeLst_] := 
 Module[
  {degreeRemoved = First[degreeLst], newsequence = Rest[degreeLst]},
  reduced = 
   Join[ reduceDegrees[ newsequence[[1 ;; degreeRemoved]] ], 
    newsequence[[1 + degreeRemoved ;; Length[newsequence] ]] ]; 
  Sort[reduced, Greater]
  ]

(*
havelHakimiAlg[degreeLst_]
degreeLst \[Rule] type: List
degreeLst must be in non-increasing order
returns true if the list can be reduced to a list of all zeroes
*)
havelHakimiAlg[graphlist_] := 
 Module[{degreeLst = graphlist[[-1]]},
  If[
   Total[degreeLst] == 0,
   Return[graphlist],
   If[
    graphicQ[degreeLst],
    havelHakimiAlg[Append[graphlist, havelHakimiReduce[degreeLst]] ],
    False
    ]
   ]
  ]
  

