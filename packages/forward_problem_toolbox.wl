(* ::Package:: *)

ThirdCoord::usage = "Finds the third coordinate of a triangle when given two coordinates and two side lengths";
ThirdCoord[a_,b_,l1_,l2_]:=(
l3 = Sqrt[(a[[1]]-b[[1]])^2+(a[[2]]-b[[2]])^2];
phi1 = ArcTan[b[[1]]-a[[1]],b[[2]]-a[[2]]];
phi2 =ArcCos[(l1^2+l3^2-l2^2)/(2*l1*l3)];
{a[[1]]+l1*Cos[phi1+phi2],a[[2]]+l1*Sin[phi1+phi2]}
);


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


MeshTriag::usage = "Used to plot a triangle, given 3 points";
MeshTriag[c1_,c2_,c3_,pltrange_]:=(
MeshRegion[{c1,c2,c3},{Line[{1,2}],Line[{2,3}],Line[{1,3}]},PlotRange->pltrange,Axes->True]
)


MeshAllTriag::usage = "Used to plot triangles for all coordinates as returned by a TriangleWorm";
MeshAllTriag[coordlist_,pltrange_]:=(
Show[Table[MeshTriag[coordlist[[i]],coordlist[[i+1]],coordlist[[i+2]],pltrange],{i,1,Length[coordlist]-2}]]
);
