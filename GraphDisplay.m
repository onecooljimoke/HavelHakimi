(* Mathematica package *)
(*
buildEmptyTable[size_]
size \[Rule] integer 
returns an n x n matrix where n = size and all values initialized to 0
*)

buildEmptyTable[size_] := Table[Table[0, {size}], {size}]

(*
Plan: 
Use the GraphPlot function to display the degree sequence.  Graph plot requires an adjacency matrix, so need to generate an adjacency matrix from the degree sequence.
Thoughts?
	Have to make sure the adjacency matrix accurately reflects the edge endpoints (ie. If you add a 1 at 1,2, you need to add a 1 at 2,1)
	Need to tie each degree sequence entry to an actual row,column in the matrix (best way to do this is probably let index of degree sequence == index in matrix)
	Need to make sure the sum of each row == the correct degree sequence
 Steps:
 	Build zero filled matrix with number of rows == length of degree sequence list
 	For any vertex degree added, also add the degree of the opposite vertex
 	Use the total degree of a vertex as a loop control, decrement the degree, when you reach zero, assign the next vertex degree
 	track the current vertex, when current vertex > list length, end the process 	
 	*)
 	