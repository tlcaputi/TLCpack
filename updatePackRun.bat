SET /P _inputname= Please enter an input:
cd C:/Users/tcapu/Google Drive/tlcPack
Rscript documentPackage.r
echo "Documentation updated."
git init
git add .
git commit -m %_inputname%
git remote add origin https://github.com/tlcaputi/tlcPack.git
git push -u origin master


pause

::@echo off
::cd C:/Users/tcapu/Google Drive/tlcPack
::set /p commit_text="Enter Commit Text: "
::updatePackage_background %commit_text% > updatePackage_background.txt

