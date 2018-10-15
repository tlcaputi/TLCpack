#' Change Directory
#'
#' This function allows you to set a directory, whether it exists or not.
#' @param directory The directory you want to set.
#' @keywords directory
#' @export
#' @examples
#' cd()



cd <- function(directory){
  if(dir.exists(directory)) { # if the directory exists
    setwd(directory) # set the working directory
    } else { # if it doesn't do exist
      dir.create(directory) # create the directory
      setwd(directory) # then set the working directory
    }
    return(directory) ## also return the working directory
}
