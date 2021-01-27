@echo off
:: See https://en.wikibooks.org/wiki/Windows_Batch_Scripting
SET mypath=%~p0
SET fullpath=%~dp0
SET drive=%fullpath:~0,1%
SET drive
CALL :LoCase drive

SET mydir=%drive%:%mypath:\=/%

set DATADIR=c:/DATA

:: nmrview Container
set VIEW_IMAGE=nmrprocflow/nmrview
set VIEW_CONTAINER=nmrview
set VIEW_CONF=%mydir%nview.conf

:: nmrspec Container
set SPEC_PORT=8080
set SPEC_IMAGE=nmrprocflow/nmrspec
set SPEC_CONTAINER=nmrspec
set SPEC_CONF=%mydir%npflow.conf


if -%1-==-- echo Argument one not provided & exit /b
if -%1-==-restart- call :restart
if -%1-==-start- call :start
if -%1-==-stop- call :stop
if -%1-==-ps- call :ps
exit /b

:restart
call :stop
call :start
exit /b

:start
echo Start ...
:: run NMRviewer
echo docker run -d --env-file %VIEW_CONF% -v %DATADIR%:/opt/data --name %VIEW_CONTAINER% %VIEW_IMAGE%

:: run NMRProcFlow
echo docker run -d --env-file %SPEC_CONF% -v %DATADIR%:/opt/data -p %SPEC_PORT%:80 --link %VIEW_CONTAINER%:nvapp --name %SPEC_CONTAINER% %SPEC_IMAGE%

:: Show the two last created containers
echo docker ps -n=2

exit /b

:stop
echo Stop ...
echo docker rm -f %SPEC_CONTAINER% %VIEW_CONTAINER%
exit /b

:ps
echo docker ps --filter=name=nmr*
exit /b

:LoCase
:: Subroutine to convert a variable VALUE to all lower case.
:: See http://www.robvanderwoude.com/battech_convertcase.php
FOR %%i IN ("A=a" "B=b" "C=c" "D=d" "E=e" "F=f" "G=g" "H=h" "I=i" "J=j" "K=k" "L=l" "M=m" "N=n" "O=o" "P=p" "Q=q" "R=r" "S=s" "T=t" "U=u" "V=v" "W=w" "X=x" "Y=y" "Z=z") DO CALL SET "%1=%%%1:%%~i%%"
exit /b

