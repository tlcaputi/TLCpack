cd "C:/Users/tcapu/Google Drive"
R -e "devtools::install_github('tlcaputi/tlcPack')"
R CMD INSTALL -build tlcPack_0.0.0.9100.tar.gz
pause
