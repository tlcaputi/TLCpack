@echo off

SET /P _inputname= Please enter commit message:
cd C:/Users/tcapu/Google Drive/tlcPack
"Rscript.exe" "C:/Users/tcapu/Google Drive/tlcPack/documentPackage.r"
echo "Documentation updated."
git init
git add .
git commit -m "%_inputname%"
:: git remote add origin https://github.com/tlcaputi/tlcPack.git
git push
:: -u origin master
pause

::@echo off
::cd C:/Users/tcapu/Google Drive/tlcPack
::set /p commit_text="Enter Commit Text: "
::updatePackage_background %commit_text% > updatePackage_background.txt

