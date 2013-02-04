


#'GLM with pseudoabsences
#'
#'Wrapper around \code{\link{glm}} to use a pseudoabsence approach, substituting 
#'absences (zeros's) with the prevalence (mean occurrence).
#'
#'@param formula formula with dependent and independent variables
#'@param family a description of the error distribution and link function to be used in the model. 
#'This can be a character string naming a family function, a family function or 
#'the result of a call to a family function. (See \code{\link{family}} for details of family functions.)
#'@param data dataframe holding the data
#'@param ... additional parameters to pass to glm
#'@return glm model
#'@seealso \code{\link{glm}}
#'@export
#'
glm.pseudoabsence <- function(formula, family=gaussian,data,...){
  #   p <- data[,as.character(attr(terms(formula(formula)),"variables")[[2]])] 
  p <- data[, dependents(formula(formula))] 
  p[p==0] <- length(p[p==1])/length(p[p==0])
  #   data[,as.character(attr(term(formula(formula)),"variables")[[2]])] <- p
  data[, dependents(formula(formula))] <- p
  glm(formula, family, data,...)
}



#'Maxent with formula
#'
#'Wrapper for Maxent to use a formula instead of data and presences
#'
#'@param formula formula with dependent and independent variables
#'@param data dataframe holding the data
#'@param ... additional parameters to pass to maxent
#'@return Maxent model
#'@seealso \code{\link{maxent}}
#'@export
#'
maxent.formula <- function(formula, data, ...){
  #   x <- data[,attr(terms(formula(formula)),"term.labels")]
  #   p <- data[,as.character(attr(terms(formula(formula)),"variables")[[2]])]
  x <- data[, dependents(formula(formula))]
  p <- data[, independents(formula(formula))]
  maxent(x,p,...)  
}




#Sequential k-fold partitioning
#'
#'Modified version of the \code{\link{dismo::kfold}} function, that returns 
#'subsequent (not random) folds (consistent among runs)
#'
#'@param x a vector, matrix, data.frame, or Spatial object
#'@param k number of groups
#'@param by Optional argument. A vector or factor with sub-groups (e.g. species). 
#'Its length should be the same as the number of records in x
#'@return a vector with group assignments
#'@seealso \code{\link{dismo::kfold}}
#'@export
#'
kfold.seq <- function (x, k = 5, by = NULL) 
{
  singlefold <- function(obs, k) {
    if (k == 1) {
      return(rep(1, obs))
    }
    else {
      i <- obs/k
      if (i < 1) {
        stop("insufficient records:", obs, ", with k=", 
             k)
      }
      i <- round(c(0, i * 1:(k - 1), obs))
      times = i[-1] - i[-length(i)]
      group <- c()
      for (j in 1:(length(times))) {
        group <- c(group, rep(j, times = times[j]))
      }
      #r <- order(runif(obs))
      #return(group[r])
      return(group)
    }
  }
  if (is.vector(x)) {
    obs <- length(x)
  }
  else if (inherits(x, "Spatial")) {
    if (inherits(x, "SpatialPoints")) {
      obs <- nrow(coordinates(x))
    }
    else {
      obs <- nrow(x@data)
    }
  }
  else {
    obs <- nrow(x)
  }
  if (is.null(by)) {
    return(singlefold(obs, k))
  }
  by = as.vector(as.matrix(by))
  if (length(by) != obs) {
    stop("by should be a vector with the same number of records as x")
  }
  un <- unique(by)
  group <- vector(length = obs)
  for (u in un) {
    i = which(by == u)
    kk = min(length(i), k)
    if (kk < k) 
      warning("lowered k for by group: ", u, "  because the number of observations was  ", 
              length(i))
    group[i] <- singlefold(length(i), kk)
  }
  return(group)
}

