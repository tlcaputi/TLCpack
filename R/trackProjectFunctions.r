#' This function removes all NAs from a vector.
#'
#' @param
#' @keywords
#' @export
#' @examples


na_omit <- function(x) x[!is.na(x)]



#' This is a wrapper for the read_feather function. It returns a data.frame and fixes the first name, which often has an extra character/
#'
#' @param
#' @keywords
#' @export
#' @examples


read_feather0 <- function(data, ...) {
  tmp <- as.data.frame(read_feather(data, ...))
  names(tmp) <- gsub("<ef>..", "", names(tmp))
  return(tmp)
}


#' Does string contain letters only? If so, it returns TRUE. Otherwise, FALSE.
#'
#' @param
#' @keywords
#' @export
#' @examples

letters_only <- function(x) !grepl("[^A-Za-z]", x)


#' Does string contain numbers only? If so, it returns TRUE. Otherwise, FALSE.
#'
#' @param
#' @keywords
#' @export
#' @examples



numbers_only <- function(x) !grepl("\\D", x)


#' How many words in this string? (Based upon a split of " ")
#'
#' @param
#' @keywords
#' @export
#' @examples


numWords <- function(str1, split=" ") sapply(strsplit(str1, split), length)



#' Text exists: Is this text in this vector? For example, is "A" in LETTERS?
#'
#' @param
#' @keywords
#' @export
#' @examples


te <- function(data, text) text %in% data



#' Custom print with systime -- prints whatever you'd like, but with the systime.
#'
#' @param
#' @keywords
#' @export
#' @examples


print0 <- function(x) print(paste0(Sys.time(),"-- ",x))


#' Is this pattern in the string (Logical) - just like grepl
#'
#' @param
#' @keywords
#' @export
#' @examples


wordExists <- function(pattern, string){
  ax <- grep(pattern, string)
  ret <- ifelse(length(ax)>0, T, F)
}


#' Find the first name. Split up all the words by spaces. If there is a comma, it takes the first word after the comma (Last, First). Otherwise, it takes the first name.
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




#' Find the last name. Split up all words by spaces. If there is a comma, it takes the first word (Last, First). Otherwise, it takes the last name.
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


#' Find the hyphen. Look for a hyphenated name. If a hyphenated name exists (e.g., Smith-Jones), it can return the word before the hyphen (if before=T, Smith) or after (if before=F, Jones).
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





#' Select variables of a dataset if they exist. Basically, this is to avoid errors for "variable doesn't exist in the dataset".
#'
#' @param
#' @keywords
#' @export
#' @examples


xvars <- function(data, variables) {
  names(data) <- tolower(names(data))
  return(as.data.frame(data[,intersect(names(data), variables)]))
}



#' Fix years: Some years are pretty obviously incorrect. If the year is between 50-100, we add 1900. If it is between 0-50, we add 2000. Otherwise, we keep it the same.
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



#' Unique with na.omit and as.numeric option. This is just an advanced unique with a few extra options.
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


#' Separate a single column into many columns by a delimiter. If a value for a column is "cat dog giraffe" and sep=" ", we get three columns "cat", "dog", and "giraffe".
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




#' Read pipe-delimited file: This just sets some defaults for reading in pipe delimited rather than comma files.
#'
#' @param
#' @keywords
#' @export
#' @examples


read.pipe <- function(filename, ...) read.delim(filename, sep="|", header=T, quote="")




#' Take only numbers before the decimal. "43890.478943" turns to "43890348"
#'
#' @param
#' @keywords
#' @export
#' @examples



wn <- function(x){
  ax <- sapply(as.numeric(x), FUN = function(h) strsplit(as.character(h), "[.]")[[1]][1])
  return(as.character(ax))
}



#' Comprehensively clean/standardize data -- run the function to get details in comments
#'
#' @param
#' @keywords
#' @export
#' @examples


cleanData <- function(x){ ## the input is a dataset
  gc() ## first we do garbage collection, which is just to save memory
  tlcPack::print0("renaming columns")
  names(x) <- tolower(names(x)) ## we transfer all the names of the dataset to lower case
  tlcPack::print0("to lower case")
  x <- apply(x,2,tolower) ## now we make everything in the dataset lower case
  tlcPack::print0("to data frame")
  x <- as.data.frame(x); gc() ## we make it into a dataframe
  tlcPack::print0("school name procedure")
  if ("school" %in% names(x)){ ## if "school" is one of the variables

    ## there are a number of terms that we can clean
    for (q in c("hs", "high", "school", "academy", "the", "a",
     "highschool", "senior", "sr", "schl")){
       ## it's important that we only look for these patterns when they are in their own word
      pat <- paste0("\\<", q, "\\>")
      ## we remove the term
      tlcPack::print0(paste0("removing ", pat))
      ## we name the new variable unischool -- which stands for uniform school. This basically means
      ## that we have standardized the name.
      x$unischool <- gsub(pat, "", x$school)
    }
  }

  tlcPack::print0("name procedure")
  ## if "name" is one of the variables in the dataframe
  if ("name" %in% names(x)){
    nmNum <- which(names(x)=="name")[1]
    ## first we get rid of some extra terms
    for (q in c("sr.", "sr", "senior", "jr.","jr","junior","mr.", "mrs.","ms.","miss",
                "iii","ii","iv","v","vi","mr","mrs","ms")) {
                  pat <- paste0("\\<", q, "\\>")
                  tlcPack::print0(paste0("removing ", pat))
                  x$name <- gsub(pat, "", x$name)
                }
    ## then we find the first name
    tlcPack::print0("first names")
    x$firstname <- sapply(x$name, function(x) {
      fnFinder(x)
    }); gc()
    ## and the last name
    tlcPack::print0("last names")
    x$lastname <- sapply(x$name, lnFinder)

    ## we create "uninames" -- uniform names or standardized names by combining the first words of the first and last name
    tlcPack::print0("uninames")
    x$uniname <- paste0(word(x$firstname, 1), " ", word(x$lastname, 1))

    ## what if there is a hyphen in the name? We create new variables for the name before
    ## and after the hyphen
    tlcPack::print0("before hyp")
    x$beforehyp <- sapply(x$name, function(x) tlcPack::hypFinder(x, before=T)); gc()
    tlcPack::print0("after hyp")
    x$afterhyp <- sapply(x$name, function(x) tlcPack::hypFinder(x, before=F))


    ## now that we've captured the hyphenated names, we get rid of the hyphens in the unique names
    nmNum <- which(names(x) %in% c("name", "firstname","lastname","uniname"))
    tlcPack::print0("replacing hyps")
    x[,-nmNum] <- apply(x[,-nmNum], 2, function(h) gsub("-", " ", h))

  } else {
    x <- apply(x, 2, function(h) gsub("-", " ", h))
  }

  ## Now we do some more basic cleaning, including removing punctuation, removing the white space
  tlcPack::print0("removing punct")
  x <- apply(x,2,function(h) removePunctuation(h)); gc()
  tlcPack::print0("stripping white space")
  x <- apply(x,2,function(h) stripWhitespace(h))
  tlcPack::print0("trimming white space")
  x <- apply(x,2,function(h) trimws(h))
  ## if something is made of numbers, we want it to be numeric
  tlcPack::print0("numbers only")
  x <- apply(x,2,function(h) suppressWarnings(ifelse(numbers_only(h), as.numeric(as.character(h)), h)))

  ##fix years like "99" to "1999" and "04" to "2004"
  tlcPack::print0("fixing years")
  x <- tlcPack::yrFun(x); gc()
  tlcPack::print0("done")

  ## return as a data frame.
  return(as.data.frame(x))
}



#' select0 -- this is a wrapper for dplyr::select that returns a data frame instead of a tibble.
#'
#' @param
#' @keywords
#' @export
#' @examples

select0 <- function(x) as.data.frame(dplyr::select())



#' Load Packages -- this just makes it easy to load all the packages I normally use for the track project
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


#' CSV to Feather (if necessary) -- this is an omnibus read-data-and-save function. We use feather data files rather than CSVs because feather files are lighter.
#' This detects if a feather file already exists. If it doesn't
#' it reads in CSV and saves a feather file. It can read normal CSV or pipe-delimited files. It returns the dataset, so you don't have to read it in again.
#'
#' @param
#' @keywords
#' @export
#' @examples

c2f <- function(in_fn, out_fn=in_fn, clean=F, pipe=F, txt=F,
                test=F, test_n=1000, read_in_feather=T, na_rm_nameid=F){
  # f <- read.csv(paste0(in_fn, ".csv"), header=T, stringsAsFactor=F)
  # assign(f, read.csv(paste0(in_fn, ".csv"), header=T, stringsAsFactor=F), envir=.GlobalEnv)
  if(pipe){
    rd <- function(...) read.pipe(...)
  } else {
    rd <- function(...) read.csv(...)
  }

  if(na_rm_nameid){
    rd2 <- function(...) {
      wax <- rd(...)
      if("nameid" %in% names(wax)){
        way <- data.frame(dplyr::filter(wax, !is.na(nameid)))
      } else {
        way <- wax
      }
      return(way)
    }
  } else {
    rd2 <- function(...) data.frame(rd(...))
  }

  if(test){
    read <- function(...) {
      ax <- rd2(...)
      if ("nameid" %in% names(ax)){
        sample_nameids <- sample(tlcPack::na_omit(ax$nameid), test_n)
        print(sample_nameids[1:20])
        ay <- ax[ax$nameid %in% sample_nameids,]; rm(ax)
      } else {
        ay <- dplyr::sample_n(ax, test_n); rm(ax)
      }
      return(ay)
      }
    } else {
      read <- function(...) rd2(...)
    }

  if (clean) {
    print("reading from csv or text, cleaning, and writing to feather")
    if (txt){
      f <- read(paste0(in_fn, ".txt"), header=T, stringsAsFactor=F)
    } else {
      f <- read(paste0(in_fn, ".csv"), header=T, stringsAsFactor=F)
    }
    f <- tlcPack::cleanData(f); gc()
    write_feather(f, paste0(out_fn, ".feather"))
  } else {

  if(file.exists(paste0(out_fn,".feather")) & read_in_feather) {
      print("feather file already exists, just reading from disk")
      f <- tlcPack::read_feather0(paste0(out_fn,".feather"))
    } else {
      print("file doesn't exist -- reading file from csv/text, NOT cleaning, and writing to feather")
      if (txt){
          f <- read(paste0(in_fn, ".txt"), header=T, stringsAsFactor=F)
        } else {
          f <- read(paste0(in_fn, ".csv"), header=T, stringsAsFactor=F)
        }
        write_feather(f, paste0(out_fn, ".feather"))
    }
  }
  gc()
  names(f) <- gsub("<ef>..", "", names(f))
  return(f)
}


#' Unique -- exactly unique(), just shorter
#'
#' @param
#' @keywords
#' @export
#' @examples

u <- function(...) unique(c(...))


#' Is NA or NULL? If so, TRUE. Otherwise, FALSE. Helpful because it looks for strings "NA" and "NULL".
#'
#' @param
#' @keywords
#' @export
#' @examples

is.na0 <- function(x) I(is.na(x) | x=="NA" | is.null(x) | x=="NULL")


#' As numeric, then as character
#'
#' @param
#' @keywords
#' @export
#' @examples

acan <- function(x) as.character(as.numeric(x))


#' Is ID? Equivalent to Stata function isid()
#'
#' @param
#' @keywords
#' @export
#' @examples


isid <- function(x) length(x)==length(u(x))


#' Sort a vector of strings by number of characters
#'
#' @param
#' @keywords
#' @export
#' @examples

sort_nchar <- function(cf, desc=F) {
  if(desc) {
    cf[order(-nchar(cf), cf)]
  } else {
    cf[order(nchar(cf), cf)]
  }
}


#' Remove articles and other small words (and/of/for/in/the) from text -- helps with abbreviations
#'
#' @param
#' @keywords
#' @export
#' @examples

remove_articles <- function(q){
  q <- gsub("\\<and\\>|\\<of\\>|\\<for\\>|\\<in\\>|\\<the\\>", " ", q)
  q <- trimws(q)
  q <- gsub("  ","",q)
  q <- gsub("^ ","",q)
  q <- gsub(" $","",q)
  return(q)
}


#' Take abbreviation -- translates a string into its abbreviation
#'
#' @param
#' @keywords
#' @export
#' @examples


abb <- function(q, suffix="", prefix="", rm_articles=F) {
  q <- gsub("-", " ", q)
  if(rm_articles) as.vector(sapply(q, remove_articles))
  sa <- sapply(unlist(str_split(q, " ")), function(x){
    if(letters_only(x)) {
      return(substr(x, 0, 1))
    } else {
      return(x)
    }
  }
  )
  ax <- paste0(as.vector(sa), collapse="")
  if(nchar(ax) + nchar(prefix) + nchar(suffix) > 1){
    return(paste0(prefix, ax, suffix))
  }
  else {
    return(q)
  }
}

#' grepl for all words -- one function to look for all words in a pattern string in any order.
#'
#' @param
#' @keywords
#' @export
#' @examples grepl_allWords("cats dogs giraffes", "dogs giraffes lions tigers cats") == T
#' @examples grepl_allWords("cats dogs giraffes", "dogs giraffes lions tigers") == F

grepl_allWords <- function(pattern, string, spaces=T, spaces_for_pattern=T){
  if(spaces) string <- paste0("\\<",string,"\\>")
  ax <- c()
  words <- unlist(stringr::str_split(trimws(pattern)," "))
  if (any(table(words)>1)){
    more_than_1 <- names(table(words))[which(table(words)>1)]
  } else {
    more_than_1 <- c()
  }
  for (i in unique(words)){

    if(spaces_for_pattern) i <- paste0("\\<",i,"\\>")

    if (i %in% more_than_1){
      ax <- c(ax, strcount(string, i, split=" ") == as.numeric(table(words)[which(names(table(words)) == i)]))
    } else {
      ax <- c(ax, grepl(i, string))
    }
  }
  return(all(ax))
}

#' Basic clean -- specific for track project (see code)
#'
#' @param
#' @keywords
#' @export
#' @examples


basicClean <- function(x){


  ## A lot of meets include the dates of the meet in the meet name. This removes that.
  ## Here are some examples of strings that would be removed:


  ## 4/26
  x <- as.vector(sapply(x, function(q) gsub("\\<[0-9]+/[0-9]+\\>", "", q)))
  ## 12-13
  x <- as.vector(sapply(x, function(q) gsub("\\<[0-9]+-[0-9]+\\>", "", q)))
  ## (11/08)
  x <- as.vector(sapply(x, function(q) gsub("\\<([0-9]+/[0-9]+)\\>", "", q)))
  ## (11-08)
  x <- as.vector(sapply(x, function(q) gsub("\\<([0-9]+-[0-9]+)\\>", "", q)))
  ## (mon/tues/wed/th/fri/sat/sun/m/t/w/th/f)
  x <- as.vector(sapply(x, function(q) gsub("\\<(*-*th-*)*\\>", "", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<(*-*wed-*)*\\>", "", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<(*-*fri-*)*\\>", "", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<(*-*mon-*)*\\>", "", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<(*-*tue-*)*\\>", "", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<(*-*sat-*)*\\>", "", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<(*-*sun-*)*\\>", "", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<(*-*t-*)*\\>", "", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<(*-*th-*)*\\>", "", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<(*-*f-*)*\\>", "", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<(*-*m-*)*\\>", "", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<(*-*w-*)*\\>", "", q)))

  ## (-) which may be left over after removing the others
  x <- as.vector(sapply(x, function(q) gsub("\\<([[:punct:]])\\>", "", q)))
  ## - which make be left over after removing others
  x <- as.vector(sapply(x, function(q) gsub("\\<[[:punct:]]\\>", "", q)))


  ## cat-123
  x <- as.vector(
    sapply(x, function(q){
      if(grepl("\\<[a-z]{1,3}-\\d{1,3}\\>", q)) {
        return(gsub("-","",q))
      } else {
        return(gsub("-"," ",q))
      }
    })
  )

  ## changes 0X to X for numbers
  x <- as.vector(sapply(x, function(q) gsub("\\<00\\>", "0", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<01\\>", "1", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<02\\>", "2", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<03\\>", "3", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<04\\>", "4", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<05\\>", "5", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<06\\>", "6", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<07\\>", "7", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<08\\>", "8", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<09\\>", "9", q)))

  ## makes a few substitutions to standardize names and/or correct spelling mistakes
  x <- as.vector(sapply(x, function(q) gsub("\\<ath\\>", "athletic", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<university interscholastic league\\>", "uil", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<ath\\.", "athletic", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<gold cost\\.", "gold coast", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<gold coast league\\.", "gold coast league finals", q)))
  x <- as.vector(sapply(x, trimws))
  x <- as.vector(sapply(x, function(q) gsub("[[:punct:]]", " ", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<championship trails\\>", "championship trials", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<valle\\>", "valley", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<div\\>", "division", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<i\\>", "1", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<ii\\>", "2", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<iii\\>", "3", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<iv\\>", "4", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<v\\>", "5", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<vi\\>", "6", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<vii\\>", "7", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<viii\\>", "8", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<so cal\\>", "south california", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<socal\\>", "south california", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<avo\\>", "avocado", q)))
  x <- as.vector(sapply(x, function(q) gsub("\\<dieggo\\>", "diego", q)))
  return(x)
}

#' Bind data from all school-meets in CSV. This was necessary to use Audrey's school-league data, but we don't use that data anymore.
#'
#' @param
#' @keywords
#' @export
#' @examples

bind_data <- function(regex="TX_", directory = "./data/ca_tx_meets"){
  tex_leagues <- grep(regex, dir(directory, ".csv", full.names=T), value=T)
  lis <- list()
  for (i in 1:length(tex_leagues)){
    fn <- paste0(tex_leagues[i])
    y <- read.csv(fn, header=T, stringsAsFactors=F)
    y$schoolid <- as.numeric(as.character(gsubfn::strapplyc(y$link, "SchoolID=([0-9]+)$")))
    y <- sapply(y, tolower)
    y <- data.frame(y)
    y$region <- y$league
    y$league <- y$subleague
    lis[[i]] <- y
  }
  tx_leagues <- do.call(rbind.data.frame, lis)
  return(tx_leagues)
}


#' Count the number of times the pattern appears
#'
#' @param
#' @keywords
#' @export
#' @examples

strcount <- function(x, pattern, split){
unlist(lapply(
    strsplit(x, split),
       function(z) na.omit(length(grep(pattern, z)))
   ))
}


#' Over 80 percent of the elements in a vector are the same
#'
#' @param
#' @keywords
#' @export
#' @examples

over.80.percent <- function(x) any(prop.table(table(x))>=0.8) | all(prop.table(table(x))==0)


#' There is more than one unique value in the vector
#'
#' @param
#' @keywords
#' @export
#' @examples

more.than.1.league <- function(x) length(table)>1


#' There are more than two elements different from the most common element in a vector
#'
#' @param
#' @keywords
#' @export
#' @examples

more.than.two.diff.league <- function(x)  sum(sort(table(x))[-length(table(x))])>2


#' Most common league -- the most common vector in a list
#'
#' @param
#' @keywords
#' @export
#' @examples

most.common.league <- function(x) names(sort(table(x)))[length(table(x))]


#' Advanced grepl -- this was an attempt to make grepl a bit more usable. It doesn't work well.
#'
#' @param
#' @keywords
#' @export
#' @examples


grepl0 <- function(pattern, string){
  pat <- unique(c(unlist(strsplit(pattern, split="&")),unlist(strsplit(pattern,split="(?:.+)"))))
  txt <- paste0("grepl('",pat,"', get(string))", collapse="&")
  ret <- eval(parse(text=txt))
  return(ret)
}

#' Track abbreviations -- this adds a few suffixes to league abbreviations: ac, c, al, and l
#' This is because the league "Butte Valley" might be coded as "Butte Valley Athletic Conference",
#' "Butte Valley Conference", "Butte Valley Atheltic League", or "Butte Valley League"
#'
#'
#' @param
#' @keywords
#' @export
#' @examples


track_abb <- function(lgs){
  return(
        c(
          lgs,
         as.vector(sapply(lgs[numWords(lgs)>1], function(x) abb(x))),
         as.vector(sapply(lgs[numWords(lgs)>1], function(x) abb(x, "ac"))),
         as.vector(sapply(lgs[numWords(lgs)>1], function(x) abb(x, "c"))),
         as.vector(sapply(lgs[numWords(lgs)>1], function(x) abb(x, "al"))),
         as.vector(sapply(lgs[numWords(lgs)>1], function(x) abb(x, "l")))
       )
       )
}

#' Extract abbreviations from parentheses in a string
#'
#' @param
#' @keywords
#' @export
#' @examples


abbv <- function(lgs) {
  paren_lgs <- grep("[(]", lgs)
  lgs <- track_abb(lgs)
  lgs <- removePunctuation(trimws(c(lgs[!1:length(lgs) %in% paren_lgs], unlist(str_split(lgs[paren_lgs], "[(]")))))
  return(lgs)
}

#' Texas T&F league abbreviations -- takes abbreviations of Texas leagues
#'
#' @param
#' @keywords
#' @export
#' @examples

tx_league_abbv <- function(x){
  x <- gsub("university interscholastic league", "uil", x)
  x <- gsub("southwest preparatory conference", "spc", x)
  x <- gsub("texas association of private and parochial schools", "tapps", x)
  x <- gsub("texas christian athletic fellowships", "tcaf", x)
  x <- gsub("texas association of independent athletic organization", "taiao", x)
  x <- gsub("texas christian athletic league", "tcal", x)
}

#' Advanced NA omit -- removes "NA", "NULL" and, if remove_blanks=T, ""
#'
#' @param
#' @keywords
#' @export
#' @examples

na_omit0 <- function(x, remove_blanks=T) {
  if(remove_blanks) {
    return(x[-which(is.na(x)|is.null(x)|x=="NA"|x=="NULL"|x=="")])
  } else {
    return(x[-which(is.na(x)|is.null(x)|x=="NA"|x=="NULL")])
  }
}

#' Navigate to meet in Athletic.net using meetID
#'
#' @param
#' @keywords
#' @export
#' @examples

meet <- function(meetid) browseURL(gsub("MEETID",meetid,"https://www.athletic.net/TrackAndField/meet/MEETID/results"))


#' Make sapply into a vector
#'
#' @param
#' @keywords
#' @export
#' @examples

avsapply <- function(...) as.vector(sapply(...))
