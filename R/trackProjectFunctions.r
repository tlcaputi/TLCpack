
#' Custom Read Feather
#'
#' @param
#' @keywords
#' @export
#' @examples


read_feather0 <- function(data, ...) as.data.frame(read_feather(data, ...))

#' Does string contain letters only?
#'
#' @param
#' @keywords
#' @export
#' @examples

letters_only <- function(x) !grepl("[^A-Za-z]", x)


#' Does string contain numbers only?
#'
#' @param
#' @keywords
#' @export
#' @examples



numbers_only <- function(x) !grepl("\\D", x)


#' How many words in this string?
#'
#' @param
#' @keywords
#' @export
#' @examples


numWords <- function(str1) sapply(strsplit(str1, " "), length)



#' Is this text in this data?
#'
#' @param
#' @keywords
#' @export
#' @examples


te <- function(data, text) text %in% data



#' Custom print with systime
#'
#' @param
#' @keywords
#' @export
#' @examples


print0 <- function(x) print(paste0(Sys.time(),"-- ",x))


#' Is this pattern in the string (Logical)
#'
#' @param
#' @keywords
#' @export
#' @examples


wordExists <- function(pattern, string){
  ax <- grep(pattern, string)
  ret <- ifelse(length(ax)>0, T, F)
}


#' Find the first name
#'
#' @param
#' @keywords
#' @export
#' @examples




fnFinder <- function(x){
  if(tlcPack::wordExists(",",x)){
    x <- gsub(",", " ,", x)
    comNum <- c()
    for (r in 1:tlcPack::numWords(x)){
      if ( word(x, r)==",") comNum <- c(comNum, r)
    }
    return(word(x, comNum[1] + 1))
  } else {
    return(word(x, 1))
  }
}




#' Find the last name
#'
#' @param
#' @keywords
#' @export
#' @examples



lnFinder <- function(x){
  if(tlcPack::wordExists(",",x)){
    x <- gsub(",", " ,", x)
    return(word(x, 1))
  } else {
    return(word(x, -1))
  }
}


#' Find the hyphen
#'
#' @param
#' @keywords
#' @export
#' @examples



hypFinder <- function(x, before=T){
  if(tlcPack::wordExists("-",x)){
    x <- gsub("-", " - ", x)
    comNum <- c()
    for (r in 1:tlcPack::numWords(x)){
      if ( word(x, r)=="-") comNum <- c(comNum, r)
    }
    if (before){
    return(word(x, comNum[1] - 1))
      } else {
      return(word(x, comNum[1] + 1))
      }
  } else {
    return(NA) #word(x, 1))
  }
}





#' Select variables of a dataset if they exist
#'
#' @param
#' @keywords
#' @export
#' @examples


xvars <- function(data, variables) {
  names(data) <- tolower(names(data))
  return(as.data.frame(data[,intersect(names(data), variables)]))
}



#' Fix years
#'
#' @param
#' @keywords
#' @export
#' @examples




yrFun <- function(dat){
    if("year" %in% names(dat)){
      dat$year <- as.numeric(dat$year)
      dat$year <- with(dat, ifelse(year < 50, year + 2000, ifelse(year>=50 & year <100, year+1900, year)))
      dat$monthNum <- with(dat, month) #sapply(month, function(x) grep(paste("(?i)",x,sep=""), month.name))) #ifelse(month(month)<=6, year, year + 1))
      dat$szn <- with(dat, ifelse(as.numeric(monthNum) <= 6, year, year+1)) #ifelse(month(month)<=6, year, year + 1))
    }
  return(dat)
}



#' Unique with na.omit and as.numeric option
#'
#' @param
#' @keywords
#' @export
#' @examples




unique00 <- function(x, an=T){
  if(an) {
    ax <- unique(as.numeric(x))
  } else {
    ax <- unique(x)
  }
  ay <- na.omit(ax)
  aw <- sort(ay)
  return(aw)
}


#' Separate a single column into many columns by a delimiter
#'
#' @param
#' @keywords
#' @export
#' @examples




separate0 <- function(data, col_to_split, sep){
  num_names = max(str_count(data[,col_to_split], sep), na.rm=T) + 1
  new_col_names <- paste0(col_to_split, 1:num_names)
  return(separate(data, col_to_split, new_col_names, sep))
}




#' Read pipe-delimited file
#'
#' @param
#' @keywords
#' @export
#' @examples


read.pipe <- function(filename, ...) read.delim(filename, sep="|", header=T, quote="")




#' Take only numbers before the decimal
#'
#' @param
#' @keywords
#' @export
#' @examples



wn <- function(x){
  ax <- sapply(as.numeric(x), FUN = function(h) strsplit(as.character(h), "[.]")[[1]][1])
  return(as.character(ax))
}



#' Comprehensively clean/standardize data
#'
#' @param
#' @keywords
#' @export
#' @examples


cleanData <- function(x){
  tlcPack::print0("renaming columns")
  names(x) <- tolower(names(x))
  tlcPack::print0("to lower case")
  x <- apply(x,2,tolower)
  tlcPack::print0("to data frame")
  x <- as.data.frame(x)
  tlcPack::print0("school name procedure")
  if ("school" %in% names(x)){
    for (q in c("hs", "high", "school", "academy", "the", "a",
     "highschool", "senior", "sr", "schl")){
      pat <- paste0("\\<", q, "\\>")
      tlcPack::print0(paste0("removing ", pat))
      x$unischool<- gsub(pat, "", x$school)
    }
  }

  tlcPack::print0("name procedure")
  if ("name" %in% names(x)){
    nmNum <- which(names(x)=="name")[1]
    for (q in c("sr.", "sr", "senior", "jr.","jr","junior","mr.", "mrs.","ms.","miss",
                "iii","ii","iv","v","vi","mr","mrs","ms")) {
                  pat <- paste0("\\<", q, "\\>")
                  tlcPack::print0(paste0("removing ", pat))
                  x$name <- gsub(pat, "", x$name)
                }

    tlcPack::print0("first names")
    x$firstname <- sapply(x$name, function(x) {
      fnFinder(x)
    })
    tlcPack::print0("last names")
    x$lastname <- sapply(x$name, lnFinder)
    tlcPack::print0("uninames")
    x$uniname <- paste0(word(x$firstname, 1), " ", word(x$lastname, 1))
    tlcPack::print0("before hyp")
    x$beforehyp <- sapply(x$name, function(x) tlcPack::hypFinder(x, before=T))
    tlcPack::print0("after hyp")
    x$afterhyp <- sapply(x$name, function(x) tlcPack::hypFinder(x, before=F))

    nmNum <- which(names(x) %in% c("name", "firstname","lastname","uniname"))
    tlcPack::print0("replacing hyps")
    x[,-nmNum] <- apply(x[,-nmNum], 2, function(h) gsub("-", " ", h))

  } else {
    x <- apply(x, 2, function(h) gsub("-", " ", h))
  }

  tlcPack::print0("removing punct")
  x <- apply(x,2,function(h) removePunctuation(h))
  tlcPack::print0("stripping white space")
  x <- apply(x,2,function(h) stripWhitespace(h))
  tlcPack::print0("trimming white space")
  x <- apply(x,2,function(h) trimws(h))
  tlcPack::print0("numbers only")
  x <- apply(x,2,function(h) suppressWarnings(ifelse(numbers_only(h), as.numeric(as.character(h)), h)))
  tlcPack::print0("fixing years")
  x <- tlcPack::yrFun(x)
  tlcPack::print0("done")
  return(as.data.frame(x))
}



#' select0
#'
#' @param
#' @keywords
#' @export
#' @examples

select0 <- function(x) as.data.frame(dplyr::select())



#' Load Packages
#'
#' @param
#' @keywords
#' @export
#' @examples
load_packages <- function(){
pacman::p_load(devtools, survey, MASS, netCoin, feather, tm,
  readr, data.table, lubridate, reshape, stringr, svMisc,
  plyr, dplyr, tidyr, haven)
}


#' CSV to Feather (if necessary)
#'
#' @param
#' @keywords
#' @export
#' @examples

c2f <- function(in_fn, out_fn=in_fn, clean=F, pipe=F, txt=F, test=F, test_n=10000, read_in_feather=T){
  # f <- read.csv(paste0(in_fn, ".csv"), header=T, stringsAsFactor=F)
  # assign(f, read.csv(paste0(in_fn, ".csv"), header=T, stringsAsFactor=F), envir=.GlobalEnv)
  if(pipe){
    rd <- function(...) read.pipe(...)
  } else {
    rd <- function(...) read.csv(...)
  }

  if(test){
    read <- function(...) dplyr::sample_n(rd(...), test_n)
  } else {
    read <- function(...) rd(...)
  }

  # feather_name <- paste0(in_fn,".feather")

  if (clean) {
    print("reading from csv, cleaning, and writing to feather")
    if (txt){
      f <- read(paste0(in_fn, ".txt"), header=T, stringsAsFactor=F)
    } else {
      f <- read(paste0(in_fn, ".csv"), header=T, stringsAsFactor=F)
    }
    f <- tlcPack::cleanData(f)
    write_feather(f, paste0(out_fn, ".feather"))
  } else {

  if(file.exists(paste0(out_fn,".feather")) & read_in_feather=T) {
      print("feather file already exists, just reading from disk")
      f <- tlcPack::read_feather0(paste0(out_fn,".feather"))
    } else {
      print("file doesn't exist -- reading file from csv, writing to feather")
      if (txt){
        f <- read(paste0(in_fn, ".txt"), header=T, stringsAsFactor=F)
      } else {
        f <- read(paste0(in_fn, ".csv"), header=T, stringsAsFactor=F)
      }
      write_feather(f, paste0(out_fn, ".feather"))
    }
  }
  return(f)
}
