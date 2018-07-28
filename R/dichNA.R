#' Dichotomize with NAs
#'
#' This function dichotomizes variables while including a column for NAs.
#' @param data Dataset from which you are pulling variables
#' @param variables List variable names in a string
#' @keywords Dichotomize
#' @export 
#' @examples
#' dichNA(data=y,variables=c("age","sex"))



dichNA = function(data,variables,add.data=F){
  nonnumeric_variables = c()
  for (i in variables){
    if(!is.numeric(data[,i])){
      data[,i] <- ifelse(is.na(data[,i]), "NA", data[,i])
      # data[,i] <- paste0(i,"_",data[,i])
      nonnumeric_variables <- c(nonnumeric_variables,i)
    }
  }
  dat =cbind.data.frame(netCoin::dichotomize(data,nonnumeric_variables,add=F), 
                        data[,! (names(data) %in% nonnumeric_variables)])
  return(dat)
}
