(* ::Package:: *)

ThirdCoord::usage = "Finds the third coordinate of a triangle when given two coordinates and two side lengths";
ThirdCoord[a_,b_,l1_,l2_]:=(
l3 = Sqrt[(a[[1]]-b[[1]])^2+(a[[2]]-b[[2]])^2];
phi1 = ArcTan[b[[1]]-a[[1]],b[[2]]-a[[2]]];
phi2 =ArcCos[(l1^2+l3^2-l2^2)/(2*l1*l3)];
{a[[1]]+l1*Cos[phi1+phi2],a[[2]]+l1*Sin[phi1+phi2]}
);


MeshTriag::usage = "Used to plot a triangle, given 3 points";
MeshTriag[c1_,c2_,c3_,pltrange_]:=(
MeshRegion[{c1,c2,c3},{Line[{1,2}],Line[{2,3}],Line[{1,3}]},PlotRange->pltrange,Axes->False]
)


MeshAllTriag::usage = "Used to plot triangles for all coordinates as returned by a TriangleWorm";
MeshAllTriag[coordlist_,pltrange_]:=(
Show[Table[MeshTriag[coordlist[[i]],coordlist[[i+1]],coordlist[[i+2]],pltrange],{i,1,Length[coordlist]-2}]]
);


(* ::Text:: *)
(*Functions for creating a general robotics lattice*)


MakeTriangleWorm::usage = "Creates a lattice of triangles in a pre defined way";
MakeTriangleWorm[fixedpoint1_,fixedpoint2_,lengths_]:=(
coords = {fixedpoint1,fixedpoint2};
Do[
If[OddQ[i]==True,  (*Enforce that new point should lie on the "right side"*)
coords =Append[coords,ThirdCoord[coords[[-2]],coords[[-1]],lengths[[2*i-1]],lengths[[2*i]]]],
coords =Append[coords,ThirdCoord[coords[[-1]],coords[[-2]],lengths[[2*i-1]],lengths[[2*i]]]]
],{i,1,Length[lengths]/2}
];
coords
);


MakeSymbolicTriangleWorm::usage = "Generates the triangleworm architechture symbolically for later finding derivatives used for optimisation";
MakeSymbolicTriangleWorm[fixedpoint1_,fixedpoint2_,nfreepoints_]:=(
coords = {fixedpoint1,fixedpoint2};
Do[
If[OddQ[i]==True,  (*Enforce that new point should lie on the "right side"*)
coords =Append[coords,ThirdCoord[coords[[-2]],coords[[-1]],Symbol[StringJoin["len",ToString[2*i-1]]],
Symbol[StringJoin["len",ToString[2*i]]]]],
coords=Append[coords,ThirdCoord[coords[[-1]],coords[[-2]],
Symbol[StringJoin["len",ToString[2*i-1]]],
Symbol[StringJoin["len",ToString[2*i]]]]]
],{i,1,nfreepoints}
];
coords
);


MakeSymbolicTriangleLattice[fixedpoint1_,fixedpoint2_,nfreepoints_]:=(
r=Reap[Flatten[NestList[NextSymbolicTrianglePair,{fixedpoint1,fixedpoint2},nfreepoints/2],1]];
structure = First@r;
symbols = First@Last@r;
{structure,symbols}
)


MakeSymbolicTriangleLattice2[fixedpoint1_,fixedpoint2_,nfreepoints_]:=(
r=Reap[Flatten[NestList[NextSymbolicTrianglePair2,{1,fixedpoint1,fixedpoint2},nfreepoints],1]];
structure = First@r;
symbols = First@Last@r;
{structure,symbols}
)



NextSymbolicTrianglePair[coords_]:=(
With[{left1=Sow@Unique["L"],right1=Sow@Unique["L"],left2=Sow@Unique["L"],right2=Sow@Unique["L"]},
{ThirdCoord[coords[[-2]],coords[[-1]],left1,right1],ThirdCoord[coords[[-2]],coords[[-1]],left2,right2]}]
)



NextSymbolicTrianglePair2[coordsandoe_]:=(
With[{left1=Sow@Unique["L"],right1=Sow@Unique["L"]},
{coordsandoe[[-3]]*-1,
coordsandoe[[-1]],
If[coordsandoe[[-3]]>0,
ThirdCoord[coordsandoe[[-1]],coordsandoe[[-2]],left1,right1],
ThirdCoord[coordsandoe[[-2]],coordsandoe[[-1]],left1,right1]]}]
)




(* ::Text:: *)
(*Functions for making symbols and symbolic constraints*)


MakeUnknowns::usage = "Generate a set of unknowns named len1,len2,....lenn";
MakeUnknowns[nunknown_]:=Table[Symbol[StringJoin["len",ToString[i]]],{i,nunknown}];


MakeOptStartPoint::usage = "Generate a starting point for optimsation stating that all lengths should be 1";
MakeOptStartPoint[nunknown_]:=Table[{Symbol[StringJoin["len",ToString[i]]],1},{i,nunknown}];


SolAsStartPoint::usage = "Used to use the solution of the previous timestep as initialization for the next timestep";
SolAsStartPoint[optparam_]:=Thread[{Keys[optparam],Values[optparam]}];


MakeLengthConstraints::usage = "Used to create the constraints on the module lengths";
MakeLengthConstraints[nunknown_,minlen_,maxlen_]:=Table[minlen<Symbol[StringJoin["len",ToString[i]]]<maxlen,{i,nunknown}]


MakeLengthConstraintsFromSymbols[symbols_,short_,long_]:=(
{short<#<long}&/@symbols
)


(* ::Text:: *)
(*Functions for performing minimisation*)


OptForDesired::usage = "Optimize for a given desired end effector location";
OptForDesired[constraints_,startpoint_]:=(
sol = FindMinimum[constraints,startpoint];
costval = sol[[1]]; If[costval>0.001,Echo[StringJoin["Optimisation Unsuccessfull, cost=",ToString[costval]]]];
optlens =sol[[2]]
)


OptForDesiredWithGrad::usage = "Optimize for a given desired end effector location using gradient information";
OptForDesired[constraints_,startpoint_]:=(
sol = FindMinimum[constraints,startpoint,Gradient->Automatic];
costval = sol[[1]]; If[costval>0.001,Echo[StringJoin["Optimisation Unsuccessfull, cost=",ToString[costval]]]];
optlens =sol[[2]]
)


SolveForPath[path_,constraints_,nunknowns_]:=(
optfordes = OptForDesired[constraints,MakeOptStartPoint[nunknowns]];
requiredlens = {optfordes};
Do[
optfordes =OptForDesired[constraints,SolAsStartPoint[optfordes]];
 AppendTo[requiredlens,optfordes],
{i,Length[path]-1}];
requiredlens
)


(* ::Subsubsection:: *)
(*Check for collisions *)


CollistionQ::usage = "Used to detect collisions between rods";
CollisionQ[coords_,shortlength_]:=(
distance = DistanceMatrix[coords] + IdentityMatrix[Length[coords]]*shortlength;
AnyTrue[distance,#<shortlength&,2]
)


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


FlipLength::usage = "Change the length of an index;
FlipLength[state_,index_]:=(
neighbour=state;
neighbour[[index]] =state[[index]]/.{0.6\[Rule]1,1\[Rule]0.6};
neighbour
)
