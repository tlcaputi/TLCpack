install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("C:/Users/tcapu/Google Drive")
create("tlcPack")

setwd("./tlcPack")
document()
