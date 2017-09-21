(* ::Package:: *)

(* ::Title:: *)
(*Talk2Data*)


(* ::Chapter:: *)
(*Starting package*)


BeginPackage["Talk2Data`"];

Talk2Data::usage =
    "Talk2Data[text, data] executes the function corresponding to the command 
    specified by the string text on the dataset specified by data. 
    
    The text can be in any language supported by Google Translate. 
    
    If a matching command is not found then the text will be evaluated with WolframAlpha.";

Begin["`Private`"];


(* ::Chapter:: *)
(*Adding commands*)


(* ::Subchapter::Bold:: *)
(*Initializing commands*)


(* ::Subsection:: *)
(*Initialize/reset association for commands*)


commands=<||>; (*Creating an empty association*)


(* ::Subsection:: *)
(*Function to add a command*)


(* ::Text:: *)
(*Adds natural language queries and the corresponding function name*)


addCommand[s_String, f_]:=AppendTo[commands, StringSplit@s -> f];
addCommand[s_List, f_]:=AppendTo[commands, StringSplit@# -> f]& /@ s;

addCommand[s_List, f_, "C->R"]:=(AppendTo[commands, StringSplit@# -> f["Column"]]& /@ s;
  AppendTo[commands, StringSplit@# -> f["Row"]]& /@ StringReplace[s,"column"->"row"]);
  
addCommand[s_List, {f1_,f2_}, "C->R"]:=(AppendTo[commands, StringSplit@# -> f1]& /@ s;
  AppendTo[commands, StringSplit@# -> f2]& /@ StringReplace[s,"column"->"row"]);


(* ::Subsection:: *)
(*Loading test dataset*)


(* ::Input:: *)
(*data = SemanticImport["http://tiny.cc/crimedata"]*)


(* ::Subchapter::Bold:: *)
(*Data manipulation*)


(* ::Section:: *)
(*Viewing/selecting data*)


(* ::Subsection:: *)
(*Show data*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


show["All"][][data_Dataset]:=data

show["Column"][n1_Integer][data_Dataset]:=data[All,n1]

show["Row"][n1_Integer][data_Dataset]:=data[n1]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"show","show data","show table","data","table"}
,show["All"]];

addCommand[{"column @Integer","show column @Integer","column @Integer","column @Integer show"}
, show, "C->R"];


(* ::Subsection:: *)
(*Show/select multiple columns/rows*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


show["Column"][n1_Integer,n2_Integer][data_Dataset]:=data[All,Range[n1,n2]]

show["Row"][n1_Integer,n2_Integer][data_Dataset]:=data[Range[n1,n2]]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"column @Integer to @Integer","column @Integer to column @Integer","column @Integer @Integer","column @Integer column @Integer"
,"show column @Integer to @Integer","show column @Integer @Integer","show column @Integer column @Integer","show column @Integer to column @Integer"
,"show column between @Integer and @Integer","show column from @Integer to @Integer"
,"column @Integer to @Integer show","column @Integer to column @Integer show"}
, show, "C->R"];


(* ::Subsection:: *)
(*Delete columns/rows*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


delete["Column"][n1_Integer][data_Dataset]:=data[All,Drop[#,{n1}]&]

delete["Row"][n1_Integer][data_Dataset]:=data[Drop[#,{n1}]&]

delete["Column"][n1_Integer,n2_Integer][data_Dataset]:=data[All,Drop[#,{n1,n2}]&]

delete["Row"][n1_Integer,n2_Integer][data_Dataset]:=data[Drop[#,{n1,n2}]&]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"show column except @Integer","column except @Integer to @Integer","delete column @Integer","except column @Integer","column @Integer delete"}
, delete, "C->R"];

addCommand[{"show column except @Integer to @Integer","show column except @Integer to column @Integer","column except @Integer to @Integer",
"column except @Integer to column @Integer","delete column @Integer to @Integer","delete column between @Integer and @Integer",
"delete column from @Integer to @Integer","column between @Integer and @Integer delete","column from @Integer to @Integer delete"
,"column @Integer to column @Integer delete"}
, delete, "C->R"];


(* ::Section:: *)
(*Operations on entire data*)


(* ::Subsection:: *)
(*Dimensions*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


dimensions[]:=Dimensions


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"dimension","dimension data","dimension table","size","size data","size table"}
, dimensions];


(* ::Subsection:: *)
(*Transpose*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


transpose["All"][]:=Transpose
transpose["Column"][n1_Integer,n2_Integer][data_Dataset]:=Transpose@data[All,n1;;n2]
transpose["Row"][n1_Integer,n2_Integer][data_Dataset]:=Transpose@data[n1;;n2]
(*Note: Returns original data for some datasets*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"transpose", "transpose data","data transpose", "transpose table","rotate","rotate table","rotate data"}
, transpose["All"]]; 

addCommand[{"transpose column @Integer","column @Integer transpose","rotate column @Integer data","rotate column @Integer table"
,"column @Integer rotate","column @Integer data rotate","column @Integer data transpose","column @Integer transpose data"},
transpose, "C->R"];


(* ::Subsection:: *)
(*Reverse*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


reverse["All"][][data_Dataset]:=Reverse@data
reverse["Column"][n1_Integer][data_Dataset]:=Reverse@data[All,n1]
reverse["Row"][n1_Integer][data_Dataset]:=Reverse@data[n1]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"reverse","reverse data","data reverse","data invert","invert"}
, reverse["All"]];

addCommand[{"reverse column @Integer","invert column @Integer","column @Integer invert","column @Integer reverse"}
, reverse, "C->R"];


(* ::Subsection:: *)
(*MinMax*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


minmax["All"][][data_Dataset]:=MinMax@Select[NumberQ]@Flatten@Normal@data
min["All"][][data_Dataset]:=Min@Select[NumberQ]@Flatten@Normal@data
max["All"][][data_Dataset]:=Max@Select[NumberQ]@Flatten@Normal@data


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"minimum and maximum","minimum and maximum data","minimum maximum","minmax","minmax data", "minmax table","minimum and maximum table"
,"data minimum and maximum"}
,minmax["All"]];

addCommand[{"maximum","maximum data","maximum element","maximum element data"}
, minmax["All"]];

addCommand[{"minimum","minimum data","minimum element","minimum element data"}
,min["All"]];


(* ::Subchapter::Bold:: *)
(*Mathematical operations*)


(* ::Section:: *)
(*Arithmetic with columns/rows*************************)


(* ::Subsection:: *)
(*Add********************************)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


add["Column"][n1_Integer,n2_Integer][data_Dataset]:=data[Plus@@@#&,{n1,n2}]
add["Row"][n1_Integer,n2_Integer][data_Dataset]:=data[{n1,n2},Plus@@@#&]
(*Note: Row operations not working*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"add column @Integer to column @Integer","add column @Integer column @Integer","sum column @Integer column @Integer",
"column @Integer plus column @Integer","column @Integer + column @Integer","sum column @Integer and column @Integer"
,"sum column @Integer column @Integer"}
, add, "C->R"];


(* ::Subsection:: *)
(*Subtract**************************)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


subtract["Column"][n1_Integer,n2_Integer][data_Dataset]:=data[Subtract@@@#&,{n2,n1}]
subtract["Row"][n1_Integer,n2_Integer][data_Dataset]:=data[n2]-data[n1]

minus["Column"][n1_Integer,n2_Integer][data_Dataset]:=data[Subtract@@@#&,{n1,n2}]
minus["Row"][n1_Integer,n2_Integer][data_Dataset]:=data[{n1,n2},Subtract@@@#&]

(*Note: Row operations not working*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"subtract column @Integer from column @Integer","subtract column @Integer column @Integer"}
, subtract, "C->R"];

addCommand[{"column @Integer minus column @Integer","column @Integer - column @Integer"}
, minus, "C->R"];


(* ::Subsection:: *)
(*Multiply**********************************)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


multiply["Column"][n1_Integer,n2_Integer][data_Dataset]:=data[Times@@@#&,{n1,n2}]
multiply["Row"][n1_Integer,n2_Integer][data_Dataset]:=data[{n1,n2},Times@@@#&]
(*Note: Row operations not working*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"multiply column @Integer with column @Integer","multiply column @Integer column @Integer","column @Integer times column @Integer"
,"column @Integer * column @Integer","column @Integer x column @Integer"}
, multiply, "C->R"];


(* ::Subsection:: *)
(*Divide***************************************)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


divide["Column"][n1_Integer,n2_Integer][data_Dataset]:=data[Divide@@@#&,{n1,n2}]
divide["Row"][n1_Integer,n2_Integer][data_Dataset]:=data[{n1,n2},Divide@@@#&]
(*Note: Row operations not working; Infinities not displayed*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"divide column @Integer with column @Integer","divide column @Integer column @Integer",
"divide column @Integer by column @Integer","column @Integer divided by column @Integer","column @Integer / column @Integer"}
, divide, "C->R"];


(* ::Section:: *)
(*Scalar operations on rows/columns*)


(* ::Subsection:: *)
(*Add*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


addC["Column",1][n1_Integer,c_][data_Dataset]:=data[#+c&,n1]
addC["Column",-1][c_,n1_Integer][data_Dataset]:=data[#+c&,n1]

addC["Row",1][n1_Integer,c_][data_Dataset]:=data[n1,#+c&]
addC["Row",-1][c_,n1_Integer][data_Dataset]:=data[n1,#+c&]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"add @Input to column @Integer","add @Input column @Integer","@Input + column @Integer"}
,{addC["Column",-1],addC["Row",-1]}
, "C->R"];

addCommand[{"column @Integer plus @Input","column @Integer + @Input"}
,{addC["Column",1],addC["Row",1]}
, "C->R"];


(* ::Subsection:: *)
(*Subtract*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


subtractC["Column",1][c_, n1_Integer][data_Dataset]:=data[#-c&,n1]
subtractC["Column",-1][n1_Integer,c_][data_Dataset]:=data[c-#&,n1]

minusC["Column",-1][c_,n1_Integer][data_Dataset]:=data[c-#&,n1]
minusC["Column",1][n1_Integer,c_][data_Dataset]:=data[#-c&,n1]

subtractC["Row",1][c_, n1_Integer][data_Dataset]:=data[n1,#-c&]
subtractC["Row",-1][n1_Integer,c_][data_Dataset]:=data[n1,c-#&]

minusC["Row",-1][c_,n1_Integer][data_Dataset]:=data[n1,c-#&]
minusC["Row",1][n1_Integer,c_][data_Dataset]:=data[n1,#-c&]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"subtract @Input from column @Integer","subtract @Input column @Integer","@Input - column @Integer"
,"@Input minus column @Integer"}
,{minusC["Column",-1],minusC["Row",-1]}
, "C->R"];

addCommand[{"subtract column @Integer from @Input","subtract column @Integer @Input",
"column @Integer minus @Input","column @Integer - @Input"}
,{minusC["Column",1],minusC["Row",1]}
, "C->R"];


(* ::Subsection:: *)
(*Multiply*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


multiplyC["Column",1][n1_Integer,c_][data_Dataset]:=data[# c&,n1]
multiplyC["Column",-1][c_,n1_Integer][data_Dataset]:=data[# c&,n1]

multiplyC["Row",1][n1_Integer,c_][data_Dataset]:=data[n1, # c&]
multiplyC["Row",-1][c_,n1_Integer][data_Dataset]:=data[n1, # c&]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"multiply @Input with column @Integer","multiply @Input column @Integer"
,"@Input times column @Integer","@Input * column @Integer"}
,{multiplyC["Column",-1],multiplyC["Row",-1]}
, "C->R"];

addCommand[{"column @Integer times @Input","column @Integer * @Input","multiply column @Integer by @Input"
,"multiply column @Integer with @Input","multiply column @Integer @Input"}
,{multiplyC["Column",1],multiplyC["Row",1]}
, "C->R"];


(* ::Subsection:: *)
(*Divide*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


divideC["Column",1][n1_Integer,c_][data_Dataset]:=data[#+c&,n1]
divideC["Column",-1][c_,n1_Integer][data_Dataset]:=data[#+c&,n1]

divideC["Row",1][n1_Integer,c_][data_Dataset]:=data[n1,#+c&]
divideC["Row",-1][c_,n1_Integer][data_Dataset]:=data[n1,#+c&]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"divide @Input by column @Integer","divide @Input column @Integer","@Input divided by column @Integer"
,"@Input / column @Integer"}
,{divideC["Column",-1],divideC["Row",-1]}
, "C->R"];

addCommand[{"column @Integer divided by @Input","column @Integer / @Input","divide column @Integer by @Input","divide column @Integer @Input"}
,{divideC["Column",1],divideC["Row",1]}
, "C->R"];


(* ::Subchapter::Bold:: *)
(*Statistics*)


(* ::Section:: *)
(*Single column/row operations*)


(* ::Subsection:: *)
(*Length*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


length["Column"][n1_Integer][data_Dataset]:=data[Length,n1]

length["Column"][][data_Dataset]:=data[Length,1]

length["Row"][n1_Integer][data_Dataset]:=data[1,Length]

length["Row"][][data_Dataset]:=data[1,Length]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"length column @Integer","column @Integer length","column @Integer total length"
,"total length column @Integer","number element column @Integer","total number element column @Integer"
,"count column @Integer","column @Integer count","count element column @Integer","column @Integer count element"}
, length, "C->R"];

addCommand[{"height table","height data","length table","length data","table height","data height","table length","data length"}
,length["Column"]];

addCommand[{"breadth table","breadth data","width table","width data","table breadth","data breadth","table width","data width"}
,length["Row"]];


(* ::Subsection:: *)
(*Mean*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


mean["Column"][n1_Integer][data_Dataset]:=N@data[Mean(*@*Select[NumberQ]*),n1]

mean["Row"][n1_Integer][data_Dataset]:=N@data[n1,Mean(*@*Select[NumberQ]*)]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"mean column @Integer","average column @Integer","arithmetic mean column @Integer"
,"column @Integer mean","column @Integer average","column @Integer arithmetic mean"}
, mean, "C->R"];


(* ::Subsection:: *)
(*Median*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


median["Column"][n1_Integer][data_Dataset]:=N@data[Median@*Select[NumberQ],n1]

median["Row"][n1_Integer][data_Dataset]:=N@data[n1,Median@*Select[NumberQ]]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"median column @Integer","column @Integer median"}
, median, "C->R"];


(* ::Subsection:: *)
(*Mode*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


mode["Column"][n1_Integer][data_Dataset]:=N@Normal@data[Commonest,n1]

mode["Row"][n1_Integer][data_Dataset]:=N@Normal@data[n1,Commonest]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"most common element column @Integer","mode column @Integer","column @Integer mode"
,"most common element column @Integer","most common column @Integer"}
, mode, "C->R"];


(* ::Subsection:: *)
(*Sum*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


sum["Column"][n1_Integer][data_Dataset]:=N@data[Total@*Select[NumberQ],n1]

sum["Row"][n1_Integer][data_Dataset]:=N@data[n1,Total@*Select[NumberQ]]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"sum column @Integer","total column @Integer","sum element column @Integer","total element column @Integer","column @Integer sum"
,"column @Integer total","column @Integer sum element","column @Integer total element"}
, sum, "C->R"];


(* ::Subsection:: *)
(*Standard deviation*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


standardDeviation["Column"][n1_Integer][data_Dataset]:=N@data[StandardDeviation@*Select[NumberQ],n1]

standardDeviation["Row"][n1_Integer][data_Dataset]:=N@data[n1,StandardDeviation@*Select[NumberQ]]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"standard deviation column @Integer","error column @Integer"
,"column @Integer standard deviation ","column @Integer error"}
, standardDeviation, "C->R"];

(*
addCommand["variance column @Integer",#^2&@*standardDeviation["Column"]];
addCommand["column @Integer variance",#^2&@*standardDeviation["Column"]];
*)

(*
addCommand["variance row @Integer",#^2&@*standardDeviation["Row"]];
addCommand["row @Integer variance",#^2&@*standardDeviation["Row"]];
*)


(* ::Subsection:: *)
(*Minimum and maximum*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


minmax["Column"][n1_Integer][data_Dataset]:=MinMax@data[All,n1]
minmax["Row"][n1_Integer][data_Dataset]:=MinMax@data[All,n1]

minmax["Column"][n1_Integer][data_Dataset]:=N@Normal@data[MinMax@*Select[NumberQ],n1]
minmax["Row"][n1_Integer][data_Dataset]:=N@Normal@data[n1,MinMax@*Select[NumberQ]]

min["Column"][n1_Integer][data_Dataset]:=N@Normal@data[Min@*Select[NumberQ],n1]
max["Column"][n1_Integer][data_Dataset]:=N@Normal@data[Max@*Select[NumberQ],n1]

min["Column"][n1_Integer][data_Dataset]:=N@Normal@data[Min@*Select[NumberQ],n1]
max["Column"][n1_Integer][data_Dataset]:=N@Normal@data[Max@*Select[NumberQ],n1]

min["Row"][n1_Integer][data_Dataset]:=N@Normal@data[n1,Min@*Select[NumberQ]]
max["Row"][n1_Integer][data_Dataset]:=N@Normal@data[n1,Max@*Select[NumberQ]]

min["Row"][n1_Integer][data_Dataset]:=N@Normal@data[n1,Min@*Select[NumberQ]]
max["Row"][n1_Integer][data_Dataset]:=N@Normal@data[n1,Max@*Select[NumberQ]]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"maximum and minimum column @Integer","minimum and maximum column @Integer"
,"column @Integer maximum and minimum","column @Integer minimum and maximum"}
, minmax, "C->R"];

addCommand[{"minimum column @Integer","minimum element column @Integer","column @Integer minimum"}
, min, "C->R"];

addCommand[{"maximum column @Integer","maximum element column @Integer","column @Integer maximum"}
, max, "C->R"];


(* ::Subsection:: *)
(*Plot column/row*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


plot["Column"][n1_Integer][data_Dataset]:=data[ListPlot[#,Joined->True,Mesh->All]&,n1]
logPlot["Column"][n1_Integer][data_Dataset]:=data[ListLogPlot[#,Joined->True,Mesh->All]&,n1]
logLogPlot["Column"][n1_Integer][data_Dataset]:=data[ListLogLogPlot[#,Joined->True,Mesh->All]&,n1]

plot["Row"][n1_Integer][data_Dataset]:=data[n1,ListPlot[#,Joined->True,Mesh->All]&]
logPlot["Row"][n1_Integer][data_Dataset]:=data[n1,ListLogPlot[#,Joined->True,Mesh->All]&]
logLogPlot["Row"][n1_Integer][data_Dataset]:=data[n1,ListLogLogPlot[#,Joined->True,Mesh->All]&]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"plot column @Integer","graph column @Integer"}
, plot, "C->R"];

addCommand[{"logplot column @Integer","log plot column @Integer"}
, logPlot, "C->R"];

addCommand[{"loglogplot column @Integer","log log plot column @Integer"}
, logLogPlot, "C->R"];


(* ::Subsection:: *)
(*SortBy*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


sortBy["Column"][n1_Integer][data_Dataset]:=SortBy[data,#[[n1]]&]

sortBy["Row"][n1_Integer][data_Dataset]:=Transpose@SortBy[Transpose@data,#[[n1]]&]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"sort column @Integer","sort by column @Integer","column @Integer sort"}
, sortBy, "C->R"];


(* ::Subsection:: *)
(*Find formula*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


findFormula["Column"][n1_Integer][data_Dataset]:=data[FindFormula[Select[#,NumberQ],x]&,n1]

findFormula["Row"][n1_Integer][data_Dataset]:=data[n1,FindFormula[Select[#,NumberQ],x]&]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"equation column @Integer","formula column @Integer","fit formula column @Integer","fit column @Integer"}
, findFormula, "C->R"];


(* ::Subsection:: *)
(*Find sequence*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


findSequence["Column"][n1_Integer][data_Dataset]:=data[FindSequenceFunction[Select[#,NumberQ],n]&,n1]

findSequence["Row"][n1_Integer][data_Dataset]:=data[n1,FindSequenceFunction[Select[#,NumberQ],n]&]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"sequence column @Integer","series column @Integer","next column @Integer"
,"next element column @Integer","next number column @Integer", "next column @Integer"}
, findSequence, "C->R"];


(* ::Subsection:: *)
(*Find distribution*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


findDistribution["Column"][n1_Integer][data_Dataset]:=data[FindDistribution@Select[#,NumberQ]&,n1]

findDistribution["Row"][n1_Integer][data_Dataset]:=data[n1,FindDistribution@Select[#,NumberQ]&]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"distribution column @Integer","probability distribution column @Integer","probability column @Integer"}
, findDistribution, "C->R"];


(* ::Subsection:: *)
(*Histogram*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


histogram["Column"][n1_Integer][data_Dataset]:=data[Histogram,n1]

histogram["Row"][n1_Integer][data_Dataset]:=data[n1,Histogram]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"histogram column @Integer", "bar chart column @Integer", "column @Integer histogram",  "column @Integer bar chart"
,"column @Integer barchart","barchart column @Integer"}
, histogram, "C->R"];


(* ::Subsection:: *)
(*Pie chart*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


pieChart["Column"][n1_Integer][data_Dataset]:=data[PieChart,n1]

pieChart["Row"][n1_Integer][data_Dataset]:=data[n1,PieChart]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"pie chart column @Integer","piechart column @Integer"}
, pieChart, "C->R"];


(* ::Section:: *)
(*Double column/row operations*)


(* ::Subsection:: *)
(*Linear fit*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


linearFit["Column"][n1_Integer,n2_Integer][data_Dataset]:=LinearModelFit[Values@Select[And@@(NumberQ/@#)&]@Normal@data[All,{n2,n1}],{1,x},x]@"x"

linearFit["Row"][n1_Integer,n2_Integer][data_Dataset]:=LinearModelFit[Select[And@@(NumberQ/@#)&]@Normal@Transpose[data][All,{n2,n1}],{1,x},x]@"x"


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"linear fit column @Integer versus column @Integer","fit column @Integer versus column @Integer"
,"linear fit column @Integer column @Integer","fit column @Integer column @Integer","fit line column @Integer column @Integer"
,"fit line to column @Integer column @Integer","fit line to column @Integer versus column @Integer"
,"fit line column @Integer versus column @Integer"}
, linearFit, "C->R"];


(* ::Subsection:: *)
(*Find formula*)


(* ::Subsubsection:: *)
(*Functions*)


findFormula["Column"][n1_Integer,n2_Integer][data_Dataset]:=FindFormula[Select[And@@(NumberQ/@#)&]@Normal@data[All,{n1,n2}],"x",3]

findFormula["Row"][n1_Integer,n2_Integer][data_Dataset]:=FindFormula[Select[And@@(NumberQ/@#)&]@Normal@Transpose[data][All,{n2,n1}],"x",3]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"formula column @Integer versus column @Integer","formula column @Integer column @Integer"}
, findFormula, "C->R"];


(* ::Subsection:: *)
(*Correlation*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


correlate["Column"][n1_Integer,n2_Integer][data_Dataset]:=Correlation@@Select[NumberQ]/@Transpose@Normal@data[All,{n1,n2}]//N
correlate["Row"][n1_Integer,n2_Integer][data_Dataset]:=Correlation@@Select[NumberQ]/@Transpose@Normal@Transpose[data][All,{n1,n2}]//N


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"correlation column @Integer with column @Integer","correlation column @Integer versus column @Integer"}
, correlate, "C->R"];


(* ::Subsection:: *)
(*Scatter plot*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


scatterPlot["Column"][n1_Integer,n2_Integer][data_Dataset]:=ListPlot[data[All,{n2,n1}]]
scatterPlot["Row"][n1_Integer,n2_Integer][data_Dataset]:=ListPlot[Transpose[data][All,{n2,n1}]]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"plot column @Integer versus @Integer","plot column @Integer versus column @Integer"
,"plot column @Integer against column @Integer","plot column @Integer column @Integer",
"scatter plot column @Integer versus @Integer","scatter plot column @Integer versus column @Integer"
,"scatter plot column @Integer against column @Integer","scatterplot column @Integer versus @Integer",
"scatterplot column @Integer versus column @Integer","scatterplot column @Integer against column @Integer"}
, scatterPlot, "C->R"];


(* ::Section:: *)
(*Multiple column/row operations*****************)


(* ::Subchapter::Bold:: *)
(*Linguistics*)


(* ::Subsection:: *)
(*Word cloud*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Functions*)


wordCloud["Column"][n1_Integer][data_Dataset]:=data[WordCloud@*DeleteStopwords@*Flatten@*StringSplit,n1]
wordCloud["Row"][n1_Integer][data_Dataset]:=data[n1,WordCloud@*DeleteStopwords]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand[{"word cloud column @Integer","wordcloud column @Integer"}
, wordCloud, "C->R"];


(* ::Subchapter::Bold:: *)
(*Geography********************)


(* ::Subsection:: *)
(*Plot coordinates*********************************)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Commands*)


addCommand["show column @Integer on map",func["All"]];
addCommand["show map of column @Integer",fun["All"]];


(* ::Chapter:: *)
(*Parsing Input*)


(* ::Subsection:: *)
(*Check if the input is a URL*)


checkURL:= If[URLParse[#]["Scheme"]=!=None,True,False]&;
(*Note: Need to make this more robust*)


(* ::Subsection:: *)
(*Lists of things to process*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*These characters are deleted from anywhere in the input string*)


skipChars=","|":"|";"|"\""|"'"|"`"|"!"|"?"  ;


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*These characters are replaced in the input string*)


replaceString={
"."..->".","first column"->"column 1","second column"->"column 2","third column"->"column 3","fourth column"->"column 4"
,"fifth column"->"column 5","sixth column"->"column 6","seventh column"->"column 7","eighth column"->"column 8"
,"first row"->"row 1","second row"->"row 2","third row"->"row 3","fourth row"->"row 4","fifth row"->"row 5"
,"sixth row"->"row 6","seventh row"->"row 7","eighth row"->"row 8"
};


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*These phrases are deleted from anywhere in the input string*)


skipBody="the"|"a"|"an"|"of"|"is"|"please"|"hello"|"hi"|"in"|"um"|
"umm"|"ummm"|"hey"|"for"|"err"|"huh"|"well"|"so"|"now"|"me"|"for"  ;


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Then these are deleted recursively if they occur in the beginning**************************)


skipStart={"draw","find","make","what","compute","evaluate","can","you","ask","tell","how","are","there","do","think","would"
};


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*These are replaced in the input string*)


replaceWords={
"vs"|"vs."|"v"|"v."->"versus", "&"->"and", "view"|"display"|"select"|"choose"|"pick"->"show", "rows"->"row",
"columns"->"column", "dimensions"->"dimension", "elements"|"things"|"thing"->"element", "counts"->"count",
"max"|"highest"|"largest"->"maximum", "min"|"lowest"|"smallest"->"minimum",
"remove"|"eliminate"|"annihilate"|"destroy"->"delete", "dataset"|"table"->"data"
};


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*These are scanned in the beginning for wrapper functions**************************************)


startCommands = <|
Sequence@@Thread[{"speak","say","listen to","listen","hear"}->Speak]
|>;
(*Note: Need to implement this. See StringStartsQ*)


(* ::Subsection:: *)
(*Functions to process the input text*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Preprocessor*)


preprocess[text_String]:=
DeleteCases[
    StringSplit
     @StringReplace[
        ToLowerCase
        @StringDelete[text
         , skipChars]
      , replaceString]
, skipBody] /. replaceWords
(*Note: Need to use startCommands*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Spell-checker*)


spellingList[words_List]:=
Join[{words},
  Take[
    Map[(*Flatten@StringSplit@*)
      Take[SpellingCorrectionList[#,Language->"English"]
      , UpTo[2]]&
    , words]//Tuples
  , UpTo[10^4]]]
(*Note: The ordering could be made more optimal by rearranging the list 
  such that the closest spelling combinations are near the beginning*)
  
(*Need to process spell-corrected lists*)


spellingList[StringSplit@"findd columnn threee to fourr"]


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Synonym-checker***********************************************)


(* ::Input:: *)
(*WordData[#,"Synonyms"]&/@{"plot","select"}*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]:: *)
(*Combined function*)


processedList[text_String]:=spellingList@FixedPoint[If[MemberQ[skipStart,#[[1]]],Drop[#,1],#]&,preprocess@text]


(* ::Chapter:: *)
(*Processing commands*******************)


(* ::Subsection:: *)
(*Sorting commands by length*)


commands=KeySortBy[commands,Length];
(*Note: Need to preprocess commands to match input preprocessor*)


(* ::Chapter:: *)
(*Executing input*)


(* ::Subsection:: *)
(*List of input types*)


inputType=<| "@Integer"->Interpreter["SemanticInteger"], "@Input"->SemanticInterpretation,
"@Number"->Interpreter["SemanticNumber"]
|>;


(* ::Subsection:: *)
(*Function to interpret inputs*)


interpretInput:= Function[input,
  If[Head@inputType@#1=!=Missing
  ,inputType[#1]@input[[#2[[1]]]]
  ,Nothing]&]


(* ::Subsection:: *)
(*Finding the function from text*)


findFunction[text_String]:=
With[{inputs=processedList@text,commandWords=Keys@commands,types=Alternatives@@Keys@inputType},
  Scan[
   Function[words,
    If[Missing=!=Head@#
    , Return@{#, MapIndexed[interpretInput[words],#]}]&
       @SelectFirst[commandWords
        , Function[key,Quiet@Delete[words,#]===Delete[key,#]&@Position[key,types]]]]
  , inputs]]
(*Note: Need to process words after spell-checking*)


(* ::Subsection:: *)
(*Checking for previous output query*)


prevOut[words_,data_]:= 
If[Length@SequenceCases[words,{"previous"|"last","output"|"result"},1]==1
, {words/.{x___,PatternSequence["previous"|"last","output"|"result"],y___}:>{x,y},%}
, If[Head@#=!=Missing
  , {words/.{x___,PatternSequence["output"|"result",_],y___}:>{x,y},Out@#}
  , {words,data}]&
     @Interpreter["SemanticNumber",Positive]@SelectFirst[
       SequenceCases[words,{"output"|"result",x_}:>x],IntegerQ@*Interpreter["SemanticNumber",Positive]]];

SetAttributes[prevOut,HoldRest]


(* ::Subsection:: *)
(*Main text evaluation function*)


Talk2Data[text_String, data_]:=
Module[{words,iData},
If[checkURL[text]
, data=SemanticImport@text
, If[StringContainsQ[text,"wolframalpha"|"wolfram alpha"]
  , WolframAlpha@StringDelete[text, "wolframalpha"|"wolfram"|"alpha", IgnoreCase->True]
  , {words,iData}=prevOut[preprocess@text,data];
    If[Length@#>0
    , (commands[#[[1]]]@@#[[2]])[iData]
    , WolframAlpha[text,"Result"]
    ]&@findFunction[StringRiffle@words]
  ]
]];

SetAttributes[Talk2Data,HoldRest];


(* ::Chapter:: *)
(*Ending package*)


End[ ];

EndPackage[ ];

(*DumpSave[NotebookDirectory[]<>"Talk2Data.mx","Talk2Data`"];*)
