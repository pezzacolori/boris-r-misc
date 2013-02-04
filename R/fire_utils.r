#'Correlations above a threshold, showing aicc's of a logistic according to fire presence
#'
#'This function returns a dataframe with the variable pairs above a given correlation threshold
#'
#'It is based on the \code{\link{cor}} function, but instead of a correlation matrix it returns
#'a dataframe with the pairwise combinations above a threshold. 
#'
#'@param vars vector of column names or column numbers holding the variables to analyse
#'@param data dataframe with the data
#'@param fire column name of number holdinf fire presence [0/1]
#'@param threshold correlation threshold
#'@param use  an optional character string giving a method for computing covariances in the 
#'presence of missing values. This must be (an abbreviation of) one of the strings 
#'"everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"
#'@param method a character string indicating which correlation coefficient 
#'is to be computed. One of "pearson" (default), "kendall", or "spearman", can be abbreviated
#'@return a list with \itemize{
#'\item dataframe holding the variable pairs with a correlation higher than the specified threshold, and the relative aicc's'
#'\item dataframe holding  allthe aicc's' for all variables
#'} 
#'@seealso \code{\link{cordf}}
#'@export
#'
cor2df.fire <- function(vars, data, fire, threshold, use = "everything",method= c("pearson", "kendall", "spearman")){
  yy <- d[,fire]
  m <- apply(data[, vars],MARGIN =2,FUN=function(a) summary(glm(yy ~ a,,family=binomial(link="logit")))$aic)
  m <- data.frame(m)
  
  u <- cordf(vars, data, threshold=threshold, use = use, method=method)
  
  for (i in 1:nrow(u)){
    u$aic1[i] <-  m$m[rownames(m)==as.character(u$Var1[i])]
    u$aic2[i] <-  m$m[rownames(m)==as.character(u$Var2[i])]
  }
  
  list(cors=u, aiccs=m)  #returns a list of the pairwise correlatiosn with aicc and also all the aicc for al variables
}





