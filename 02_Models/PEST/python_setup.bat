@echo off
REM Check if conda is available
where conda >nul 2>nul
IF %ERRORLEVEL% NEQ 0 (
    echo Conda is not found in PATH. Please ensure Anaconda or Miniconda is installed and available.
    exit /b 1
)

REM Setup shell if necessary
::conda init

REM Check if the environment already exists
conda env list | findstr /C:"SV_AEM_T2P" >nul
IF %ERRORLEVEL% EQU 0 (
    echo Conda environment "SV_AEM_T2P" already exists. Skipping creation.
) ELSE (
    echo Creating Conda environment SV_AEM_T2P...
    conda create --name SV_AEM_T2P python=3.11.11 pandas=2.2.3 -y
)

REM Activate the environment
echo Activating SV_AEM_T2P...
call conda activate SV_AEM_T2P

REM Confirm installation
echo Installed packages:
conda list

echo Environment SV_AEM_T2P is ready.