
libname a "D:\";

/*ICD CODE, 주상병, 질환별 1 or 0 코딩 심혈관예시*/
/*I00이면 I00 변수는  1, 그 외에 ICD 코드는 0*/
/*I99이면 I99 변수는  1, 그 외에 ICD 코드는 0*/
/*example data set*/
data z; set a.a;

array index{100} I00-I99;
do i= 1 to 100;

/*배열 함수로 지정한 변수명과 자료 명이 동일한 경우 1, 아닌 경우 0*/
if vlabel(index{i})=substr(ICD,1,3) THEN index{i}=1; else index{i}=0;
end;

drop i ;

run;

/*자료 순서 뒤집혀서 적용되는지? 검토*/
proc sort data=a out=a2; by descending OBS; run;

data z3; set a2;

array index{100} I00-I99;
do i= 1 to 100;
if vlabel(index{i})=substr(ICD,1,3) THEN index{i}=1; else index{i}=0;
end;

drop i ;

run;
proc sort data=z3; by obs; run;
