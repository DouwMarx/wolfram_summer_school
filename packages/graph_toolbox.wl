(* ::Package:: *)

(* ::Subsubsection:: *)
(*Constructing graphs*)


FindNeighbours::usage = "Used to detect collisions between rods";
FindNeighbours[state_]:=Table[FlipLength[state,i],{i,Length[state]}];


StateToNeighbours::usage = "Gives a mapping between a certain state and its neighbours for graphing";
StateToNeighbours[state_,style_]:=Style[state->#,style]&/@FindNeighbours[state];


MakeState[statenumber_,nrods_]:=IntegerDigits[statenumber-1,2,nrods]/.{0->shortlength,0->longlength}


PlotCoordTransition::usage = "For input to tootip of graphs";
PlotCoordTransition[transition_]:= (
MeshAllTriag[MakeTriangleWorm[fixed1,fixed2,First@First@transition],{{0,3},{0,3}}]->
MeshAllTriag[MakeTriangleWorm[fixed1,fixed2,Last@First@transition],{{0,3},{0,3}}]
)


FlipLength::usage = "Change the length of an index";
FlipLength[state_,index_]:=(
neighbour=state;
neighbour[[index]] =state[[index]]/.{0.6->1,1->0.6};
neighbour
)


(* ::Subsubsection:: *)
(*Optimal transition between states*)


SmallCoordChangeMetric[pathbetweenstates_]:=(
allcoords = (MakeTriangleWorm[fixed1,fixed2,#]&/@pathbetweenstates)[[All,-1,All]];
distancebetween = Total@Transpose@(allcoords[[2;;-1,All]]-allcoords[[1;;-2,All]])^2;
Total[distancebetween]/Length[pathbetweenstates]
)


AllCoordsForStatePath::usage = "Find all end effector ";
AllCoordsForStatePath[path_]:=(MakeTriangleWorm[fixed1,fixed2,#]&/@path)[[All,-1,All]];
