/*SAS ���̺귯�� */
libname A 'D:\' ;

/*���� ���̺귯�� ������ ���̺��, ��, �� �� ã�� */
proc sql;CREATE TABLE table_str AS
select MemName as table_name, nobs as nrow ,NVar as ncol
from sashelp.vtable
where Libname = 'A'; /*���̺귯�� ����*/
quit;
