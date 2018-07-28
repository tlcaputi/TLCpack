# pacman::p_load(devtools)

if ( !require(devtools) ) install.packages("devtools")
if ( !require(roxygen2) ) devtools::install_github("klutometis/roxygen")
library(devtools)
library(roxygen2)

setwd("C:/Users/tcapu/Google Drive")
if(!dir.exists("tlcPack")) create("tlcPack")

setwd("./tlcPack")
document()

print("Documentation process successful.")


