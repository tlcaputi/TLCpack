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


#' Outersect
#'
#' Takes the non-intersection of two sets
#' @param
#' @keywords
#' @export
#' @examples
#' outersect(1:10, 5:15)

outersect <- function(x, y, ...) {
  big.vec <- c(x, y, ...)
  duplicates <- big.vec[duplicated(big.vec)]
  setdiff(big.vec, unique(duplicates))
}


#' Predict and Expand
#'
#' Takes the non-intersection of two sets
#' @param
#' @keywords
#' @export
#' @examples
#' predict_expand(reg = lm(outcome ~ time), new.time = 1990:2020)


predict_expand <- function(reg, new.time, orig.data=NA){




  if(length(all.vars(reg$terms))!=2) stop("Regression must be univariate: outcome ~ time")

  if(!require(pacman)) install.packages("pacman")
  pacman::p_load(BMisc, devtools, rlang)

  outcome_var <- BMisc::lhs.vars(reg$terms)
  time_var <- BMisc::rhs.vars(reg$terms)

  model_data <- reg$model[,c(time_var, outcome_var)]

  if(all(new.time %in% model_data[,time_var])) {

    warning("Nothing to impute, returning original data")
    new_data <- orig.data %>% arrange(!!parse_quosure(time_var))

  } else {

  names(model_data) <- c("time", "outcome")
  missing_time <- tlcPack::outersect(new.time, model_data$time)
  temp_data <- data.frame(time=missing_time, outcome=NA)
  new_data <- rbind.data.frame(model_data, temp_data)
  names(new_data) <- c(time_var, outcome_var)
  new_data$fitted <- predict(reg, newdata=new_data, type="response")
  new_data[,outcome_var] <- ifelse(is.na(new_data[,outcome_var]), new_data$fitted, new_data[,outcome_var])
  new_data <- new_data %>% select(time_var, outcome_var) %>% arrange(!!parse_quosure(time_var))

  }

  if(is.data.frame(orig.data)){

    if(!all.equal(
      orig.data[,c(time_var, outcome_var)],
      reg$model[,c(time_var, outcome_var)]
    )) { stop("Model data and orig.data not the same") }

    other_variables <- outersect(names(orig.data), all.vars(reg$terms))
    other_data <- orig.data[,c(other_variables, time_var)]
    new_data <- merge(new_data, other_data, by=time_var, all.x=T)
  }

  return(new_data)

}



#' Read SPSS File
#'
#' @param
#' @keywords
#' @export
#' @examples
#'

read.spss0 <- function(...) suppressWarnings(read.spss(..., to.data.frame=T))
