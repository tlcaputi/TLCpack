#' Make it acceptable for a function to end in a comma
#'
#' @param
#' @keywords
#' @export
#' @examples

ok.comma <- function(FUN) {
  function(...) {
    arg.list <- as.list(sys.call())[-1L]
    len <- length(arg.list)
    if (len > 1L) {
      last <- arg.list[[len]]
      if (missing(last)) {
        arg.list <- arg.list[-len]
      }
    }
    do.call(FUN, arg.list)
  }
}

#' Make it acceptable for the c() function to end in a comma
#'
#' @param
#' @keywords
#' @export
#' @examples


c0          <- ok.comma(base::c)

#' Make it acceptable for the list() function to end in a comma
#'
#' @param
#' @keywords
#' @export
#' @examples

list0       <- ok.comma(base::list)

#' Make it acceptable for the data.frame() function to end in a comma
#'
#' @param
#' @keywords
#' @export
#' @examples

data.frame0 <- ok.comma(base::data.frame)
