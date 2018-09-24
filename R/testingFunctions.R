#' Test what's not in the list
#'
#' This function creates a table of summary statistics.
#' @param data Data vector
#' @param string URL string with "<TEST>" where you want to insert the data
#' @param sample number to try
#' @param test default to FALSE
#' @keywords
#' @export
#' @examples
#'  testOutside(y$meetid, string="http://co.milesplit.com/meets/<TEST>", sample=20, test=F)
#'

testOutside <- function(data, string="http://co.milesplit.com/meets/<TEST>", sample=20, start=NULL, test=F){
  poss_ids <- 1:max(data, na.rm=T)
  missing_ids <- poss_ids[!(poss_ids %in% data)]
  set.seed(1234)
  test_set <- sample(missing_ids, 100000, replace=F)
  if (!is.null(start)) assign("wefrdbhjuifbedjhi", start, envir=.GlobalEnv)
  if(!exists("wefrdbhjuifbedjhi", envir=.GlobalEnv)) {
    assign("wefrdbhjuifbedjhi", 1, envir=.GlobalEnv)
  } else{
    assign("wefrdbhjuifbedjhi", .GlobalEnv$wefrdbhjuifbedjhi + sample, envir=.GlobalEnv)
  }
  print(wefrdbhjuifbedjhi)
  test_ids <- test_set[(wefrdbhjuifbedjhi-sample+1):wefrdbhjuifbedjhi]
  for (i in test_ids){
    url <- gsub("<TEST>",i, string)
    if (!test) browseURL(url, browser = getOption("browser"), encodeIfNeeded = FALSE)
  }
}


#' Test what's in the list
#'
#' This function creates a table of summary statistics.
#' @param data Dataframe
#' @param col Name of the column to pull from
#' @param string URL string with "<TEST>" where you want to insert the data
#' @param sample number to try
#' @param test default to FALSE
#' @keywords
#' @export
#' @examples
#'  testInside(y$meetid, col="schoolid", string="http://co.milesplit.com/teams/<TEST>", sample=1, test=F)
#'


testInside <- function(data, col="schoolid", string="http://co.milesplit.com/teams/<TEST>", sample=1, start=NULL, test=F){
  set.seed(1234)
  test_rows <- sample(1:nrow(data), 1000, replace=F)
  test_set <- data[test_rows, ]
  if (!is.null(start)) assign("nhjdr4nh3498n43h289", start, envir=.GlobalEnv)
  if(!exists("nhjdr4nh3498n43h289", envir=.GlobalEnv)) {
    assign("nhjdr4nh3498n43h289", 1, envir=.GlobalEnv)
  } else{
    assign("nhjdr4nh3498n43h289", .GlobalEnv$nhjdr4nh3498n43h289 + sample, envir=.GlobalEnv)
  }
  test_ids <- test_set[(nhjdr4nh3498n43h289-sample+1):nhjdr4nh3498n43h289, col]
  for (i in test_ids){
    url <- gsub("<TEST>",i, string)
    if (!test) browseURL(url, browser = getOption("browser"), encodeIfNeeded = FALSE)
  }
  head(test_set[(nhjdr4nh3498n43h289-sample+1):nhjdr4nh3498n43h289, ], sample)
}

#' Read Pipe Delimited Files
#'
#' This function creates a table of summary statistics.
#' @param filename filename for pipe-delimited file
#' @keywords read pipe
#' @export
#' @examples
#'  read.pipe(filename)
#'


read.pipe <- function(filename, sep="|", header=T, stringsAsFactors=F, quote="", ...) {
  read.delim(filename, sep=sep, header=header, stringsAsFactors=stringsAsFactors, quote=quote, ...)}

#' As numeric
#'
#' This function creates a table of summary statistics.
#' @param x Data
#' @keywords summary
#' @export
#' @examples
#'  an("45678")
#'

an <- function(x) as.numeric(as.character(x))
