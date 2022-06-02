data _null_;
%let url = //support.sas.com/documentation/onlinedoc/stat/ex_code/131;
infile "http:&url/templft.html" device=url;
file 'macros.tmp';
input;
if index(_infile_, '</pre>') then stop;
if pre then put _infile_;
if index(_infile_, '<pre>') then pre + 1;
run;
%inc 'macros.tmp' / nosource;
/***********************************************************************/
/*옵션 적용 */
%ProvideSurvivalMacros
%let yOptions = label="Survival"
linearopts=(viewmin=0.1 viewmax=1 tickvaluelist=(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)); 
%let xOptions = label="X";
/* linearopts=(viewmin=0.2 viewmax=1 tickvaluelist=(0 .2 .4 .6 .8 1.0)); */

%CompileSurvivalTemplates

/*plotting*/
proc lifetest data=sashelp.BMT plots=survival(cb=hw test);
time T * Status(0);
strata Group;
run;
/***********************************************************************/

/*위 매크로 설정내용 지우고 plotting*/
proc template;
   delete Stat.Lifetest.Graphics.ProductLimitSurvival  / store=sasuser.templat;
   delete Stat.Lifetest.Graphics.ProductLimitSurvival2 /  store=sasuser.templat;
run;


proc lifetest data=sashelp.BMT plots=survival(cb=hw test);
time T * Status(0);
strata Group;
run;
