
################### generic utility functions ###################################################


#' Concatenate and evaluate string expressions in a specified environment
#' 
#' This function allows to write in a shorter form the evaluation of a 
#' vector of characters.
#' 
#' @param ... character strings holding the code to be evaluated
#' @param envir the environment in which expr is to be evaluated. May also be NULL, 
#' a list, a data frame, a pairlist or an integer as specified to sys.call.
#' @param enclos	Relevant when envir is a (pair)list or a data frame. Specifies the enclosure, 
#' i.e., where R looks for objects not found in envir. This can be NULL (interpreted as the base 
#' package environment, baseenv()) or an environment.
#' @param sep separator character to be used as in the \code{\link{paste}} function
#' @return The result of evaluating the object: for an expression vector 
#' this is the result of evaluating the last element
#' @export
#' 
evaltext<-function(...,envir = parent.frame(), 
                   enclos = if(is.list(envir) || is.pairlist(envir))
                            parent.frame() else baseenv(),   sep=""){
  
  eval(parse(text=paste(..., sep=sep)), envir=envir, enclos=enclos)
}


#' Change levels order
#' 
#' This function changes the oder of the levels of a factor
#' 
#' @param x factor
#' @param neworder numeric or character vector specifiyng the new order of the levels
#' @param ordered logical specifying if the factor will be ordered or not (defaults to input factor class)
#' @param ... other parameters to be passed to factor function (labels, exclude)
#' @return factor with levels order changed according to specifications
#' @export
#' 
orderfactor <- function(x, neworder, ordered=is.ordered(x), ...){
  if (is.numeric(neworder)) neworder <- levels(x)[neworder]
  
  factor(x, levels=neworder, ordered=ordered, ...)
}


#' Rescale a vector of numbers between 0 and 1
#' 
#' This function rescales the values of a numeric vector
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
#'@param data dataframe with the data
#'@param vars vector of column names or column numbers holding the variables to analyse. If not specified all the columns will be used.
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
cordf <- function(data, vars=NULL, threshold=0.6, use = "everything", method= c("pearson", "kendall", "spearman")){
  if (!is.null(vars)) data <- data[, vars]
  d <- cor(data, use = use, method=method)
  cor2df(d)
}


#'Variable pairs correlated above a threshold
#'
#'This function returns a dataframe with the variable pairs above a given correlation threshold
#'
#'It is based on the \code{\link{cor}} function, but instead of a correlation matrix it returns
#'a dataframe with the pairwise combinations above a threshold. 
#'
#'@param cor.matrix correlation matrix
#'@param threshold correlation threshold
#'@return a dataframe holding the variable pairs with a correlation higher than the specified threshold
#'@seealso \code{\link{cordf}}
#'@export
#'
cor2df <- function(cor.matrix, threshold=0.6){
  require(reshape2)
  for (i in 1:ncol(cor.matrix)){
    for (j in 1:nrow(cor.matrix)){
      if (j<=i) cor.matrix[j,i] <- NA    #set to NA half of the correlation matrix plus the diagonal
    }
  }
  d_m <- melt(cor.matrix, na.rm=T)  
  unique(d_m[abs(d_m$value)>threshold & !is.na(d_m$value),])
}



#'Dependent variable
#'
#'Extract the name of the dependent variable from formula 
#'
#'@param formula formula to inspect, either as formula object or string
#'@return name of the dependent variable
#'@seealso  \code{\link{all.vars}} from base package to get all variables
#'@export
#'
dep.vars <- function(formula){
  if (is.character(formula)) formula <- formula(formula)
  t <- terms(formula)
  if (attr(t,"response")==0) NA 
  else all.vars(parse(text = as.character(attr(t,"variables"))[[2]]))   #index 1 is the list
}


#'Independent variable(s)
#'
#'Extract the name of the independent variable(s) from formula 
#'
#'@param formula formula to inspect, either as formula object or string
#'@return name of the independent variable(s)
#'@seealso  \code{\link{all.vars}} from base package to get all variables
#'@export
#'
ind.vars <- function(formula){
  if (is.character(formula)) formula <- formula(formula)
  t <- terms(formula)  
  i <- as.character(attr(t,"variables"))
  i[(attr(t,"response")+2):length(i)]
}


#'Formulae from variable combinations
#'
#'Build the formulae (as strings) from variable names
#'
#'@param formula formula with all the terms (beyond optimal model)
#'@param dep name of the dependent variable. If the \code{formula} is specified, this argument is not considered.
#'@param vars character vector with the names of the independent variables (wihtout nullmodel term). If the \code{formula} is specified, this argument is not considered.
#'@param nullmodelterm to specify in case of an always required fixed term (should not be included in the vars)
#'@param minsize minimum size of the formula (number of independent variables)
#'@param maxsize maximum size of the formula (number of independent variables). NULL means unrestricted.
#'@return character vector hold the strings of the generated formulae.
#'@export
#'
formulae <- function(formula, dep=NULL, vars=NULL, nullmodelterm="1", minsize=1, maxsize=NULL){        #vars are without nullmodelterm   #nullmodelterm in case of fixed term    #polyDegree can be a vector specifying for each term of vars the degree
  if (!missing(formula)){
    if (!is.null(dep) || !is.null(vars)) warning('Formula is specified and also dep and/or vars. Only the formula term will be considered.')
    dep <- dep.vars(formula)
    vars <- ind.vars(formula)    
  }
  
  l=if (nullmodelterm!="") list(nullmodelterm) else list()
  for (i in 1:length(vars)){
    f = combn(vars,i,simplify=F)
    if (!nullmodelterm %in% c("","1")) f=lapply(f,function(x) c(nullmodelterm,x))  #for (x in f) {x[[1]] = c(nullmodelterm,x[[1]])}#paste(nullmodelterm,f,sep=" + ")
    l = c(l, f)
  }
  if (minsize>1) l <- l[sapply(l,length)>=minsize]
  if (!is.null(maxsize)) l <- l[sapply(l,length)<=maxsize]
  f=sapply(l,function(x) paste(dep,"~",paste(x,collapse="+")))
  list(formulae=f, vars=l)
}


#get variable combinations avoiding correlated variables
#'Formulae from variable combinations without correlated variables
#'
#'Build the formulae (as strings) from variable names
#'
#'@param formula formula with all the terms (beyond optimal model)
#'@param dep name of the dependent variable. If the \code{formula} is specified, this argument is not considered.
#'@param vars character vector with the names of the independent variables (wihtout nullmodel term). If the \code{formula} is specified, this argument is not considered.
#'@param nullmodelterm to specify in case of an always required fixed term (should not be included in the vars)
#'@param minsize minimum size of the formula (number of independent variables)
#'@param maxsize maximum size of the formula (number of independent variables). NULL means unrestricted.
#'@param data dataframe holding the dataset with the column names corresponding to vars
#'@param threshold correlation threshold
#'@param use  an optional character string giving a method for computing covariances in the 
#'presence of missing values. This must be (an abbreviation of) one of the strings 
#'"everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"
#'@param method a character string indicating which correlation coefficient 
#'is to be computed. One of "pearson" (default), "kendall", or "spearman", can be abbreviated
#'@return list of \itemize{
#'\item formulae: character vector hold the strings of the generated formulae
#'\item vars: list of variables names combinations 
#'} 
#'@export
#'
formulae.cleaned <- function(formula, dep=NULL, vars=NULL, nullmodelterm="1", minsize=1, maxsize=NULL, data, threshold=0.6, use = "everything", method = c("pearson", "kendall", "spearman")){        #vars are without nullmodelterm  
  t <- formulae(formula, dep, vars, nullmodelterm, minsize, maxsize)
  l <- t$vars
  cors<-cordf(data=data, vars=vars, threshold=threshold, use=use, method=method)
  l<-cleanFormulae(formulae=l,cors=cors)
  f=sapply(l,function(x) paste(dep,"~",paste(x,collapse="+")))
  list(formulae=f, vars=l)
}

#eliminate the variable combinations that contains 2 correlated variables, according to cors 
cleanFormulae <- function(formulae, cors){
  for (i in 1:length(formulae)){
    f<-formulae[[i]]
    if (nrow(cors)>0){
      for (k in 1:nrow(cors)){
        if (cors[k,1] %in% f & cors[k,2] %in% f){
          formulae[[i]]=NA
          break
        } 
      }
    }
  }
  formulae[sapply(formulae,function(x) !is.na(x[1]))]
}





#'Remove NA's
#'
#'Return the given dataframe without the rows where one of the independent variables (extracted from formula) are NA
#'
#'This is useful to link abundances to the model datasets, since the built models internally exclude those rows
#'
#'@param data dataframe with the data
#'@param selection column names or formula with dependent and independent variables
#'@return A dataframe without NA's in the columns holded by independent variables of the formula
#'@export
#'
without.na <- function (data, selection){
  vars <- if (class(try(as.formula(selection)))=='formula')
            ind.vars(selection)
          else
            selection

  for (var in vars){
    #     print(var)
    data <- data[!is.na(data[,var]),]
  }
  data
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
  if (length(x) != length(y))
    stop("x and y must have the same length")
  if (length(unique(x)) < 2)
    return(NA) 
  
  bases <-  abs(diff(x))    #    abs(x[-1]-x[-length(x)])
  heights <- abs(diff(y))   #     abs(y[-1]-y[-length(y)])
  baseheights <- pmin(y[-1], y[-length(y)])
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



#'Fill 1-value gaps in a vector
#'
#'Fill gaps of single values with linearization (mean of the adjacent values) or
#'repetition of previous/next value.
#'
#'
#'@param x numeric vector 
#'@param method how to fill in the gaps (default by linearization, otherwise by 
#'previous/next value duplication)
#'@return numeric vector with filled 1-value gaps
#'@export
#'
fill.1.na <-  function(x, method=c('linearize', 'previous', 'next')){
  
  roll.na <- rollapply(x, width=3, FUN=function(a) sum(is.na(a)), fill=999)
  
  #select only 1day gaps
  one.day <- which(is.na(x) & roll.na==1)
  
  #linearize
  #   for (i in one.day)  x[i] <- x[i-1] + (x[i+1]-x[i-1])/2
  if (substr(method,1,1)=='l')
    for (i in one.day)  x[i] <- mean(c(x[i-1],x[i+1]))
  else if (substr(method,1,1)=='p')
    for (i in one.day)  x[i] <- x[i-1]
  else if (substr(method,1,1)=='n')
    for (i in one.day)  x[i] <- x[i+1]

  x
}


#'Fill gaps in a dataframe with data from another dataframe
#'
#'Function replacing NA values in a dataframe with sequentially corresponding 
#'data from another dataframe of the same length and with same column names.
#'
#'
#'@param to dataframe holding the NA values to replace 
#'@param from dataframe holding the values to replace the NA's
#'@param colnames character vector with the names of the columns
#'@param case.sensitive logcial indicating if column names are considered according to case or not
#'@return dataframe with replaces NA's
#'@export
#'
mirror.na <- function (to, from, colnames, case.sensitive=T){

  if (!case.sensitive){
    names(to) <- tolower(names(to))
    oldnames <- names(to)
    names(from) <- tolower(names(from))
    colnames <- tolower(colnames)  
  }
  
  for (var in colnames)  to[is.na(to[,var]), var] <- from[is.na(to[,var]), var]
  
  if (!case.sensitive) names(to) <- oldnames
  to
}


#'Filename without extension
#'
#'Strips the extension form the filename.
#'
#'@param file name of the file
#'@return file name without extension
#'@export
#'
filename <- function(file){
  x <- strsplit(file,'.',fixed=T)[[1]]
  paste(x[-length(x)],collapse='.')
}


#'Extract and Load command line arguments into session
#'
#'Loads the command line arguments supplied when this R session was invoked
#'into the session environment.
#'
#'@return Nothing
#'@export
#'
getArgs = function() {
  args=(commandArgs(TRUE))
  if(length(args) > 0) {
    for (i in 1:length(args)){
      eval.parent (parse(text=args[[i]]))
    } 
  } 
}




#'Reads a fixed width formatted data with the header in the same format
#'
#'The base function \code{\link{read.fwf}} can read fixed width formatted data, however when including an header,
#'this needs to have another format (e.g. tab-separated, as specified by the sep argument).
#'This function allows to read data with the header specifically in the same fixed width format as the data.
#'
#'@param file name of the file.
#'@param widths integer vector, giving the widths of the fixed-width fields (of one line).
#'@param ... further arguments to be passed to \code{\link{read.fwf}}.
#'@return A data.frame as produced by \code{\link{read.fwf}} which is called internally.
#'@export
#'
read.fwf.fixedheader <- function(file, widths, ...){
   
  cols <- read.fwf(file,
                   widths=widths,
                   header=F,
                   n=1, stringsAsFactors=F)
  cols <- sapply(cols[1,,drop=T], str_trim)
  
  d <- read.fwf(file,
                widths=widths,
                header=F,
                skip=1,
                col.names=cols, ...)
  d
}







