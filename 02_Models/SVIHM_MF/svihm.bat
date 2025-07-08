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

:: Run gage2vol
call conda activate SV_AEM_T2P
python ..\..\Bin\GAGE2VOL.py Streamflow_FJ_SVIHM.dat 1990-09-30 5
python ..\..\Bin\HOB2HDIFF.py HobData_SVIHM.dat
conda deactivate

cd ..\