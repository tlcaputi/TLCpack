# pacman::p_load(devtools)
# devtools::install_github("klutometis/roxygen")
library(devtools)
library(roxygen2)

setwd("C:/Users/tcapu/Google Drive")
if(!dir.exists("tlcPack")) create("tlcPack")

setwd("./tlcPack")
document()

