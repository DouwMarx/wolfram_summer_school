(* ::Package:: *)

ThirdCoord::usage = "Finds the third coordinate of a triangle when given two coordinates and two side lengths";
ThirdCoord[a_,b_,l1_,l2_]:=(
l3 = Sqrt[(a[[1]]-b[[1]])^2+(a[[2]]-b[[2]])^2];
phi1 = ArcTan[b[[1]]-a[[1]],b[[2]]-a[[2]]];
phi2 =ArcCos[(l1^2+l3^2-l2^2)/(2*l1*l3)];
{a[[1]]+l1*Cos[phi1+phi2],a[[2]]+l1*Sin[phi1+phi2]}
);


MakeRow[bottomcoords_,rodlens_]:=(
c = Length[bottomcoords]-1;
topleftrodid1 = 2*c+2*(c-1) +1;
toprightrodid1 = topleftrodid1+2;
midlevel = Table[ThirdCoord[bottomcoords[[i]],bottomcoords[[i+1]],rodlens[[2*i-1]],rodlens[[2*i]]],{i,1,c}];
topcenter =Table[ThirdCoord[midlevel[[i]],midlevel[[i+1]],rodlens[[2*c+2*i-1]],rodlens[[2*c+2*i]]],{i,1,c-1}];
topleft =ThirdCoord[midlevel[[1]],topcenter[[1]],rodlens[[topleftrodid1]],rodlens[[topleftrodid1+1]]];
topright =ThirdCoord[topcenter[[-1]],midlevel[[-1]],rodlens[[toprightrodid1]],rodlens[[toprightrodid1+1]]];
{bottomcoords,midlevel,topcenter,topleft,topright}
)


LengthToColor[length_]:=If[length>0.98,Darker@Green,Red];


PlotRow[rowcoords_,rodlengths_]:=(
bot = rowcoords[[1]];
mid = rowcoords[[2]];
top = rowcoords[[3]];
left = rowcoords[[4]];
right = rowcoords[[5]];
c = Length[bot]-1;
topleftrodid1 = 2*c+2*(c-1) +1;
toprightrodid1 = topleftrodid1+2;

botposdiag = Table[Style[Line[{bot[[i]],mid[[i]]}],LengthToColor[rodlengths[[2*i-1]]]],{i,1,c}];
botnegdiag = Table[Style[Line[{bot[[i+1]],mid[[i]]}],LengthToColor[rodlengths[[2*i-1+1]]]],{i,1,c}];

topposdiag = Table[Style[Line[{mid[[i]],top[[i]]}],LengthToColor[rodlengths[[2*i-1 + c*2]]]],{i,1,c-1}];
topnegdiag = Table[Style[Line[{mid[[i+1]],top[[i]]}],LengthToColor[rodlengths[[2*i-1+1 + c*2]]]],{i,1,c-1}];

topleft = {Style[Line[{mid[[1]],left}],LengthToColor[rodlengths[[topleftrodid1]]]],
Style[Line[{top[[1]],left}],LengthToColor[rodlengths[[topleftrodid1+1]]]]};
topright = {Style[Line[{mid[[-1]],right}],LengthToColor[rodlengths[[toprightrodid1+1]]]],
Style[Line[{top[[-1]],right}],LengthToColor[rodlengths[[toprightrodid1]]]]};
Join[botposdiag,botnegdiag,topposdiag,topnegdiag, topleft,topright]
)


GetRowTopCoords[makerowoutput_]:=Join[{makerowoutput[[4]]},makerowoutput[[3]],{makerowoutput[[5]]}]


BuildStructure[c_,r_,rodlengths_]:=(
fixed = With[{fixedspacing=1},
Table[{x*fixedspacing,0},{x,0,c}]];

top = fixed;
coordlist = {};
graphicslist={};

Do[rowrodlengths =rodlengths[[i*nr+1;;(i+1)*nr]];
 row = MakeRow[top,rowrodlengths]; 
AppendTo[coordlist,row];
AppendTo[graphicslist,PlotRow[row,rowrodlengths]];
top=GetRowTopCoords[row]
,{i,0,r-1}];
{coordlist,graphicslist})
