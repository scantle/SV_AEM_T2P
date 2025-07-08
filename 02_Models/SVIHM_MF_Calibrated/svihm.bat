:: Run par2par
call ..\Bin\par2par.exe t2p_par2par.in

:: Run T2P
cd preproc
call ..\..\Bin\AEM2Texture.exe
call ..\..\Bin\Texture2Par.exe svihm.t2p

:: Run SWBM
cd ..\SWBM
call ..\..\Bin\SWBM.exe svihm.swbm

:: Copy over new SWBM-generated MODFLOW files
cd ..\
xcopy SWBM\SVIHM.* MODFLOW /Y /I
xcopy SWBM\SVIHM_tabfile_seg*.tab MODFLOW /Y /I

:: Run SFR2Par
copy MODFLOW\SVIHM.sfr MODFLOW\SVIHM_intermediate.sfr /Y
call ..\Bin\SFR2PAR sfr2par.in

:: Run MODFLOW
cd MODFLOW
call ..\..\Bin\MODFLOW-NWT.exe SVIHM.nam

:: Run gage2vol
call conda activate SV_AEM_T2P
python ..\..\Bin\LOGSTRSIM.py Streamflow_FJ_SVIHM.dat MidptFlow
python ..\..\Bin\LOGSTRSIM.py Streamflow_AS_SVIHM.dat MidptFlow
python ..\..\Bin\LOGSTRSIM.py Streamflow_BY_SVIHM.dat MidptFlow
python ..\..\Bin\GAGE2VOL.py Streamflow_FJ_SVIHM.dat 1990-09-30 5
python ..\..\Bin\HOB2HDIFF.py HobData_SVIHM.dat
conda deactivate

cd ..\