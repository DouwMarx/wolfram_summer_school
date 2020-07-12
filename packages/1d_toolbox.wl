(* ::Package:: *)

MaxRodStates[n_]:=4+2^(n-2)+4


ToRodRule[rule_]:=Join[MapThread[Rule,{Join[Prepend[2]/@Tuples[{0,1},2],Tuples[{0,1},3],Append[2]/@Tuples[{0,1},2]],IntegerDigits[rule-1,2,16]}],{{2,2,0}->2,{2,2,1}->2,{0,2,2}->2,{1,2,2}->2}]


RodEvolution[rule_,n_,t_,init_:Automatic]:=CellularAutomaton[ToRodRule[rule],Replace[init,Automatic->Join[{2},ConstantArray[0,n],{2}]],t]


GraphSingleRod[startloc_,onof_]:={CapForm["Butt"],AbsoluteThickness[50],Line[{{startloc,1},{If[onof==1,long,short]+startloc,1}},VertexColors->If[onof==1,{Green, Black},{Red, Black}]]};
