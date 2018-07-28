#' newZelig
#'
#' This function provides simulation-based risk ratios from a regression that involves factors. 
#' @param ind_var_group The factor variable for which you want an estimate 
#' @param ind_var_value The value of ind_var_group that will be evaluated. Default set at 1.
#' @param zero_out The factor variable for which we want to zero-out all other possibilities. 
#' @param dep_var Dependent variable
#' @param other_variables What other confounders do you want to include in the model
#' @param factor_variables Which of the variables should be considered categorical?
#' @param sex_interaction Include if you want to interact the independent variable with sex. Default set to F.
#' @param data Dataset you want to use
#' @keywords Zelig
#' @keywords Risk Ratios
#' @export 
#' @examples
#' 


newZelig = function(
  ind_var_group 
  ,
  ind_var_value = 1
  ,
  zero_out = NULL
  ,
  dep_var 
  ,
  other_variables 
  ,
  factor_variables 
  ,
  .id = NULL
  ,
  .weight= NULL
  ,
  .nest= NULL
  ,
  .strata= NULL
  ,
  total_variables=c(dep_var,zero_out,other_variables,
                    substr(.id,1,10)[2],
                    substr(.weight,1,10)[2],
                    substr(.strata,1,10)[2])
  ,
  dat 
  ,
  sex_interaction=F
  ,
  totalData = dat[,total_variables]
  ,
  seed=1234
  ,
  function_family=binomial(link="logit")
){
  
  n = sum(totalData[!(is.na(totalData[,dep_var])),zero_out]==ind_var_group, na.rm=T)
  
  if ("lst" %in% ls()) rm(lst) # removes anything from before so I can tell if there's an error
  
  #the zero_out functionality allows the user to set all categories == 0. The formula naturally 
  #changes every value to its mean. If you zero_out a group of variables, you can see what
  #just one subgroup looks like.
  if(! (ind_var_group %in% unique(totalData[,zero_out])) ) stop('Group not in Variable') 
  
  
  # this step makes the correct formula
  if (!(is.na(zero_out))){
    ind_var = paste0(zero_out,ind_var_group)
  } else {
    ind_var = ind_var_group
  }
  
  
  if (sex_interaction==T & "sex" %in% other_variables){
    othvars = other_variables[which(other_variables!="sex")]
    fmla = paste0(dep_var,"~",zero_out,":sex+",paste(othvars,collapse="+"))
    
  } else {
    othvars = other_variables
    fmla = paste0(dep_var,"~",zero_out,"+",paste(othvars,collapse="+"))
  }
  
  for (r in factor_variables){
    totalData[,r] = factor(totalData[,r])
  }
  
  desg = svydesign(id=.id, weight=.weight, nest=.nest, 
                   strata=.strata, data = totalData)
  
  
  ### this creates the matrix of simulated logistic regressions
  set.seed(seed)
  model.glm <- svyglm(as.formula(fmla), desg, family=function_family)
  sim.coef <- mvrnorm(n = 10000, mu=model.glm$coefficients, Sigma=vcov(model.glm))
  
  
  ## this is the complicated part. basically, the whole point of this section is to automatically
  ## calculate what the mean of all the factors should be. This is somewhat difficult because
  ## of the complex survey design.
  
  
  fctrs = names(model.glm$data)[!(names(model.glm$data) %in% ".survey.prob.weights")]
  
  
  for (i in fctrs){
    if (i == fctrs[1]){
      df = data.frame()
    }
    
    # i = fctrs[7]  
    a = data.frame(svymean(~get(i),design=desg, na.rm=T)) #takes the svymean in a loop
    rwn = unlist(strsplit(rownames(a),")"))[1:nrow(a)*2]
    if( length(rwn) < 2) {
      rownames(a) = i
    } else {
      row.names(a) = paste0(i,unlist(strsplit(rownames(a),")"))[1:nrow(a)*2])
    }
    names(a) = c("mean","SE")
    df = rbind.data.frame(df,a)
    
  }
  
  df = rbind.data.frame(df,"(Intercept)"=c(1,0)) # adds a value to multiply w intercept
  
  vctr = rep(NA,length=length(colnames(sim.coef)))
  names(vctr) = colnames(sim.coef)
  df2 = as.vector(t(df[,1]))
  names(df2) = rownames(df)
  
  ## this fills out the vector using all the svymean data we collected
  for (i in 1:length(df2)){
    nm = names(df2)[i]
    if (length(grep(nm,names(vctr)))!=0){
      vctr[grep(nm,names(vctr))] = df2[i]
    }
  }
  
  ## this is the zero-out functionality. For example, we're taking all
  ## the sex_subgroups and making them 0 so we can look at the predicted prob
  ## for people who are a certain sex_subgroup. Then we set the coefficient for
  ## the population we're looking at to 1. Here, we make all the sex_subgroups
  ## except hetero females = 0.
  
  
  if (!(is.na(zero_out))){
    vctr[grep(zero_out,names(vctr))] = 0 
  }
  if (ind_var %in% names(vctr)) {
    vctr[ind_var] = ind_var_value
  }
  
  
  ## this is where we actually calcualte the probabilities
  mat1 = matrix(sim.coef,ncol=ncol(sim.coef)) # these are all the simulated logistic regression coefficients
  mat2 = matrix(vctr,ncol=1) # this is the vertical vector with all the coefficients we want to use
  
  odds = exp(as.numeric(mat1 %*% mat2)) # matrix multiply then exponentiate
  probs = odds / ( 1 + odds ) # translate into probability
  
  # outcome = c(mean(probs),quantile(probs,c(0.025,0.975)))
  
  lst = list("probs"=probs,"mat1"=mat1,"mat2"=mat2,"model.glm"=model.glm,"vctr"=vctr,
             "means"=df2,"sim.coef"=sim.coef,"fmla"=fmla,"n"=n)
  return(lst) # the function returns 10,000 simulated probabilities
  
}

