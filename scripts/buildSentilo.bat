@echo off

set CURRENT_DIR=%~dp0
cd %CURRENT_DIR%

set ERRORLEVEL=0

cls

REM Step 0: Validate prerequisites: environment variables M2_HOME and JAVA_HOME must be 
REM M2_HOME environment variable
if "%M2_HOME%" == "" goto not_m2_home
if "%JAVA_HOME%" == "" goto not_java_home

REM Step 1: Compile and build platform code.
cd %CURRENT_DIR%/..
set COMMAND=mvn clean install
echo Step 1: Compile and build platform code
echo.
echo This command will compile and build platform code via Maven:
echo.
echo %COMMAND%
echo.
echo Press enter when ready.

pause

call %COMMAND%
if not "%ERRORLEVEL%" == "0" goto error

REM Step 2: Run appassempler plugin into java-standalone modules for generate scripts for starts java processes.
REM For more  information about plugin visit http://mojo.codehaus.org/appassembler/appassembler-maven-plugin/ 
cd %CURRENT_DIR%/../sentilo-platform/sentilo-platform-server
set COMMAND=mvn package appassembler:assemble -P dev
echo Step 2: Generate scripts for start java processes 
echo.
echo %COMMAND%
echo.
echo Begins with sentilo-platform process
echo.
echo Press enter when ready.

pause

call %COMMAND%
if not "%ERRORLEVEL%" == "0" goto error

cd %CURRENT_DIR%/../sentilo-agent-alert
echo Now sentilo-agent-alert
echo.
echo Press enter when ready.

pause

call %COMMAND%
if not "%ERRORLEVEL%" == "0" goto error

cd %CURRENT_DIR%/../sentilo-agent-relational
echo Now sentilo-agent-relational
echo.
echo Press enter when ready.

pause

call %COMMAND%
if not "%ERRORLEVEL%" == "0" goto error

cd %CURRENT_DIR%/../sentilo-agent-location-updater
echo ... and finally sentilo-agent-location-updater
echo.
echo Press enter when ready.

pause

call %COMMAND%
if not "%ERRORLEVEL%" == "0" goto error

goto end


:not_m2_home
echo.
echo ERROR: M2_HOME is set to an invalid directory.
echo M2_HOME = "%M2_HOME%"
echo Please set the M2_HOME variable in your environment to match the
echo location of the Maven installation
echo.
goto error

:not_java_home
echo.
echo ERROR: M2_HOME not found in your environment.
echo Please set the M2_HOME variable in your environment to match the
echo location of the Maven installation
echo.
goto error


:error
set ERRORLEVEL=1

:end
cd %CURRENT_DIR%
echo.
echo.
echo Now you're ready to install the Sentilo platform. 
exit /B %ERRORLEVEL%