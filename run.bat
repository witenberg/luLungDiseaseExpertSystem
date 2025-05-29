@echo off
echo Kompilowanie projektu...
javac -d bin src/main/java/*.java
if %ERRORLEVEL% neq 0 (
    echo Wystapil blad podczas kompilacji!
    pause
    exit /b %ERRORLEVEL%
)
echo Uruchamianie projektu...
java -cp "bin;lib/jpl.jar" main.java.LungDiseaseExpertSystemMain
pause