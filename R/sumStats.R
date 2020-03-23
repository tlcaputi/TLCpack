#' This function creates a table of summary statistics.
#'
#' @param
#' @keywords
#' @export
#' @examples

ss = function(x,name=""){

  r = function(x) round(x,2) #this makes it easier to round things
  if(is.numeric(x)){ # for variables that are numeric

    n = sum(!is.na(x)) # I take the N, mean, std dev, sE(mean), and 95%ci
    mn = mean(x,na.rm=T)
    if(all(x %in% 0:1)) mn * 100
    stdev = sd(x,na.rm=T)
    stderror = sd(x,na.rm=T)/sqrt(sum(!is.na(x)))
    lo95 = mn-1.96*stderror
    hi95 = mn+1.96*stderror
    return(rbind(c("variable"=name,"N"=r(n),"Mean"=r(mn),"SD"=r(stdev),
                   "SE"=r(stderror),"lo95"=r(lo95),"hi95"=r(hi95))))
  } else { # for variables that are not numeric i.e., categorical
    q = 0
    for (i in sort(unique(x))){ # I run a for-loop  over each value that collects
      v = ifelse(name!="",paste0(name,"_",i),i) # this creates a varible name to go in the table
      n = sum(I(x==i),na.rm=T) # the in-group N and mean (proportion)
      mn = mean(I(x==i),na.rm=T)*100
      ax = prop.test(sum(I(x==i),na.rm=T),sum(!is.na(x))) # I compute a 1-sample prop test
      lo95 = ax$conf.int[1] # so I can extract the confidence interval
      hi95 = ax$conf.int[2]
      rw = c("variable"=v,"N"=r(n),"Mean"=r(mn),"SD"=NA,
             "SE"=NA,"lo95"=r(lo95),"hi95"=r(hi95))
      q = q + 1
      if (q==1){ # this just makes a table out of the for-loop
        assign("tab",rw,envir=.GlobalEnv)
      } else {
        assign("tab", rbind(tab,rw),envir=.GlobalEnv)
      }
    }
    row.names(tab) = NULL # this removes pesky rownames
    return(tab)
  }
}
