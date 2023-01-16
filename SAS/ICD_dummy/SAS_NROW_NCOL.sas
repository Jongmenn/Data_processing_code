/*SAS 라이브러리 */
libname A 'D:\' ;

/*현재 라이브러리 내에서 테이블명, 행, 열 수 찾기 */
proc sql;CREATE TABLE table_str AS
select MemName as table_name, nobs as nrow ,NVar as ncol
from sashelp.vtable
where Libname = 'A'; /*라이브러리 지정*/
quit;
