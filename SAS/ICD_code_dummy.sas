
libname a "D:\";

/*ICD CODE, �ֻ�, ��ȯ�� 1 or 0 �ڵ� ����������*/
/*I00�̸� I00 ������  1, �� �ܿ� ICD �ڵ�� 0*/
/*I99�̸� I99 ������  1, �� �ܿ� ICD �ڵ�� 0*/
/*example data set*/
data z; set a.a;

array index{100} I00-I99;
do i= 1 to 100;

/*�迭 �Լ��� ������ ������� �ڷ� ���� ������ ��� 1, �ƴ� ��� 0*/
if vlabel(index{i})=substr(ICD,1,3) THEN index{i}=1; else index{i}=0;
end;

drop i ;

run;

/*�ڷ� ���� �������� ����Ǵ���? ����*/
proc sort data=a out=a2; by descending OBS; run;

data z3; set a2;

array index{100} I00-I99;
do i= 1 to 100;
if vlabel(index{i})=substr(ICD,1,3) THEN index{i}=1; else index{i}=0;
end;

drop i ;

run;
proc sort data=z3; by obs; run;
