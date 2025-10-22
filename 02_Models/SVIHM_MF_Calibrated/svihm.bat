:: Run SWBM
cd SWBM
call ..\..\Bin\SWBM.exe svihm.swbm

:: Copy over new SWBM-generated MODFLOW files
cd ..\
xcopy SWBM\SVIHM.* MODFLOW /Y /I
xcopy SWBM\SVIHM_tabfile_seg*.tab MODFLOW /Y /I

:: Run MODFLOW
cd MODFLOW
call ..\..\Bin\MODFLOW-NWT.exe SVIHM.nam

cd ..\