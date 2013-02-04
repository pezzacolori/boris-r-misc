
################### generic utility functions ###################################################


#' Concatenate and evaluate string expressions in a specified environment
#' 
#' This function allows to write in a shorter form the evaluation of a 
#' vector of characters.
#' 
#' @export
#' @return The result of evaluating the object: for an expression vector 
#' this is the result of evaluating the last element
#' @examples
#' evaltext("m = ",prefix,formulae[i], suffix)
#' @export
#' 
evaltext<-function(...,envir = parent.frame(), 
                   enclos = if(is.list(envir) || is.pairlist(envir))
                            parent.frame() else baseenv(),   sep=""){
  
  eval(parse(text=paste(..., sep=sep)), envir=envir, enclos=enclos)
}


#' Rescale a vector of numbers between 0 and 1
#' 
#' This function resscales the values of a numeric vector
#' between 0 and 1
#' 
#' @param x numeric vector to rescale
#' @param na.rm logical indicating whether missing values should be removed
#' @return numeric vector with rescaled values
#' @export
#' 
rescale01 <- function(x, na.rm=FALSE){
  (x- min(x,na.rm=na.rm))/(max(x,na.rm=na.rm)-min(x,na.rm=na.rm)) 
}


#'Correlations above a threshold
#'
#'This function returns a dataframe with the variable pairs above a given correlation threshold
#'
#'It is based on the \code{\link{cor}} function, but instead of a correlation matrix it returns
#'a dataframe with the pairwise combinations above a threshold. 
#'
#'@param vars vector of column names or column numbers holding the variables to analyse
#'@param data dataframe with the data
#'@param threshold correlation threshold
#'@param use  an optional character string giving a method for computing covariances in the 
#'presence of missing values. This must be (an abbreviation of) one of the strings 
#'"everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"
#'@param method a character string indicating which correlation coefficient 
#'is to be computed. One of "pearson" (default), "kendall", or "spearman", can be abbreviated
#'@return a dataframe holding the variable pairs with a correlation higher than the specified threshold
#'@seealso \code{\link{cor2df}}
#'@export
#'
cordf <- function(vars, data, threshold=0.6, use = "everything", method= c("pearson", "kendall", "spearman")){
  d <- cor(data[, vars],use = use, method=method)
  cor2df(d)
}


#'Correlated variable pairs 
#'
#'This function returns a dataframe with the variable pairs above a given correlation threshold
#'
#'It is based on the \code{\link{cor}} function, but instead of a correlation matrix it returns
#'a dataframe with the pairwise combinations above a threshold. 
#'
#'@param d correlation matrix
#'@param threshold correlation threshold
#'@return a dataframe holding the variable pairs with a correlation higher than the specified threshold
#'@seealso \code{\link{cordf}}
#'@export
#'
cor2df <- function(d, threshold=0.6){
  require(reshape2)
  for (i in 1:ncol(d)){
    for (j in 1:nrow(d)){
      if (j<=i) d[j,i] <- NA    #set to NA half of the correlation matrix plus the diagonal
    }
  }
  d_m <- melt(d, na.rm=T)  
  unique(d_m[abs(d_m$value)>threshold & !is.na(d_m$value),])
}



#'Dependent variable(s)
#'
#'Extract the name of the dependent variable(s) from formula 
#'
#'@param formula formula to inspect
#'@return name of the dependent variable(s)
#'@export
#'
dependents <- function(formula){
  as.character(attr(terms(formula),"variables")[[2]])
}


#'Independent variable(s)
#'
#'Extract the name of the independent variable(s) from formula 
#'
#'@param formula formula to inspect
#'@return name of the independent variable(s)
#'@export
#'
independents <- function(formula){
  attr(terms(formula),"term.labels")
}


#'Formulae from variable combinations
#'
#'Build the formulae (as strings) from variable names
#'
#'@param dep name of the dependent variable
#'@param vars character vector with the names of the independent variables
#'@param nullmodelterm to specify in case of an always required fixed term (should not be included in the vars)
#'@param minsize minimum size of the formula (number of independent variables)
#'@param maxsize maximum size of the formula (number of independent variables). NULL means unrestricted.
#'@return character vector hold the strings of the generated formulae.
#'@export
#'
formulae <- function(dep, vars, nullmodelterm="1", minsize=1, maxsize=NULL){        #vars are without nullmodelterm   #nullmodelterm in case of fixed term    #polyDegree can be a vector specifying for each term of vars the degree
  
  l=if (nullmodelterm!="") list(nullmodelterm) else list()
  for (i in 1:length(vars)){
    f = combn(vars,i,simplify=F)
    if (!nullmodelterm %in% c("","1")) f=lapply(f,function(x) c(nullmodelterm,x))  #for (x in f) {x[[1]] = c(nullmodelterm,x[[1]])}#paste(nullmodelterm,f,sep=" + ")
    l = c(l, f)
  }
  if (minsize>1) l <- l[sapply(l,length)>=minsize]
  if (!is.null(maxsize)) l <- l[sapply(l,length)<=maxsize]
  sapply(l,function(x) paste(dep,"~",paste(x,collapse="+")))
}


#'Formulae from formula with all terms
#'
#'Build the formulae (as strings) from a formula holding all the terms
#'
#'@param formula formula with all the terms
#'@param nullmodelterm to specify in case of an always required fixed term (should not be included in the input formula)
#'@param minsize minimum size of the formula (number of independent variables)
#'@param maxsize maximum size of the formula (number of independent variables). NULL means unrestricted.
#'@return character vector hold the strings of the generated formulae.
#'@export
#'
formulae2 <- function(formula, nullmodelterm="1", minsize=1, maxsize=NULL){        #vars are without nullmodelterm   #nullmodelterm in case of fixed term    #polyDegree can be a vector specifying for each term of vars the degree
  dep <- dependents(formula)
  vars <- independents(formula)
  formulae(dep, vars, nullmodelterm, minsize)
}




#get variable combinations avoiding correlated variables
formulaeCleaned <- function(dep, vars, data, nullmodelterm="1", minsize=1,maxsize=NULL, threshold=0.6, use = "everything",method = c("pearson", "kendall", "spearman")){        #vars are without nullmodelterm  
  l=if (nullmodelterm!="") list(nullmodelterm) else list()
  for (i in 1:length(vars)){
    f = combn(vars,i,simplify=F)#,FUN=function(x) cleanFormulae(x,data))
    if (!nullmodelterm %in% c("","1")) f=lapply(f,function(x) c(nullmodelterm,x))  #for (x in f) {x[[1]] = c(nullmodelterm,x[[1]])}#paste(nullmodelterm,f,sep=" + ")
    l = c(l, f)
  }
  if (minsize>1) l <- l[sapply(l,length)>=minsize]
  if (!is.null(maxsize)) l <- l[sapply(l,length)<=maxsize]
  cors<-getCor(vars=vars,data=data,threshold=threshold,use=use,method=method)
  l<-cleanFormulae(formulae=l,cors=cors)
  sapply(l,function(x) paste(dep,"~",paste(x,collapse="+")))
}

#eliminate the variable combinations that contains 2 correlated variables, according to cors 
cleanFormulae <- function(formulae, cors){
  for (i in 1:length(formulae)){
    f<-formulae[i][[1]]
    if (nrow(cors)>0){
      for (k in 1:nrow(cors)){
        if (cors[k,1] %in% f & cors[k,2] %in% f){
          formulae[i]=NULL
          break
        } 
      }
    }
  }
  formulae[sapply(formulae,function(x) !is.null(x))]
}





#'Remove NA's
#'
#'Return the given dataframe without the rows where one of the independent variables (extracted from formula) are NA
#'
#'This is useful to link abundances to the model datasets, since the built models internally exclude those rows
#'
#'@param data dataframe with the data
#'@param formula formula with dependent and independent variables
#'@return A dataframe without NA's in the columns holded by independent variables of the formula
#'@export
#'
getNonNADataFromFormula <- function (data, formula){
  if (is.character(formula)) formula <- formula(formula)
  vars <- independents(formula)
  for (var in vars){
    #     print(var)
    data <- data[!is.na(data[,var]),]
  }
  data
}


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



#'Area under a curve
#'
#'Calculates the trapezoid area (boxes+traingles) under the curve y=f(x)
#'
#'@param x vector holding the x values
#'@param y vector holding the y values
#'@return area under the curve
#'@seealso \code{\link{calcAreaLim}}
#'@export
#'
calcArea <- function (x, y){
  bases <- abs(x[-1]-x[-length(x)])
  heights <- abs(y[-1]-y[-length(y)])
  baseheights <- pmin(y[-1],y[-length(y)])
  tria <- heights * bases /2
  #   boxes <- y[-c(1,length(y))] * bases[-1]       #(-length(bases))
  boxes <- baseheights * bases   #2012.04.12
  
  sum(boxes) + sum (tria)
}


#'Area under a curve
#'
#'Calculates the trapezoid area (boxes+traingles) under the curve y=f(x)
#'up to a given x limit (xupper), when given
#'
#'If xupper is not one of the x values, the corresponding y value is 
#'calculated using the approx function
#'
#'@param x vector holding the x values
#'@param y vector holding the y values
#'@param xupper x value
#'@return area under the curve
#'@seealso \code{\link{calcArea}}
#'@export
#'
calcAreaLim <- function (x, y, xupper=NULL){
  if (!is.null(xupper)) {
    if (xupper>max(x)) xupper <- max(x)
    yupper <- approx(x,y,xupper)$y
    x <- c(x[x<xupper],xupper)
    y <- c(y[1:(length(x)-1)],yupper)
  }
  calcArea(x,y)
}