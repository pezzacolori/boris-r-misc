


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




#'Sequential k-fold partitioning
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





#'Models from formulae 
#'
#'Build a list of models according to different formulae, a prefix and a suffix
#'
#'@param x a vector, matrix, data.frame, or Spatial object
#'@param prefix string representation of the model prefix (e.g.  "glm(") 
#'@param formulae list of formulae as string expressions
#'@param suffix string representation of the model suffix (e.g.  ", family="binomial")"   )
#'@param envir environment in which to evaluate the model expression
#'@return a list of models
#'@export
#'
getModelList <- function(prefix,formulae,suffix, envir=parent.frame(1)){  #formulas as characters
  l=list()
  for (formula in formulae){
    l = c(l,try(list(evaltext(prefix,formula,suffix,envir=envir))))     #evaluate in the environment that calls getModelList
  }
  #    l=l[sapply(l,is.list)]    # eliminate rows with errors (no model)
  l
}




# function to evaluate a maxent model (me), to be used as subfunction
#abundance is optional and should be passed in same order as presences (and same length)

#'Accuracy function to evaluate a presence/absence/background model 
#'
#'Evaluate according to multiple criteria
#'
#'@param p presences
#'@param a absences
#'@param abundance abundances, should be passed in same order as presences and same length
#'@return a vector of named arguments (n=number data, np=numer of presences, auc=auc, auc.bg=auc on the background)
#'@export
#'
accuracy.simple <- function(p, a, abundance=NULL){   
#   library(SDMTools)
  g
  dp <- data.frame(presence=rep(1,length(p)), y=p)
  da <- data.frame(presence=rep(0,length(a)), y=a)
  
  #for standard auc
  d <- rbind(dp,da)
  
  #for auc.bg (background instead of absences) (add again presences as absences)
  d.bg <- d
  d.bg$presence <- 0
  d.bg <- rbind(dp,d.bg)
  
  
  #for area calculation relative to abundance (burnt area)
  if (!is.null(abundance)){
    if (length(abundance)!=length(p)){
      stop(paste('abundance has to be the same length as model data! abundance =',length(abundance),', presence =',length(p)))
      area <-NA
      area.log <-NA
    } else{   
      tp.rate <- 1:length(p)
      tp.rate <- rescale01(tp.rate)
      abundance[is.na(abundance)] <- 0              #replace missing abundances with 0 ha
      mx.csum <- max(cumsum(abundance))
      mx.log.csum <- max(cumsum(log(abundance+1)))
      
      #-------
      abundance.ord <- abundance[order(abundance)]
      w.min <- cumsum(abundance.ord)/mx.csum
      area.min <- calcArea(tp.rate, w.min)      
      w.max <- cumsum(rev(abundance.ord))/mx.csum
      area.max <- calcArea(tp.rate, w.max)      
      
      abundance <- abundance[order(p)]      
      w <- cumsum(rev(abundance))/mx.csum
      area <- calcArea(tp.rate, w)
      
      area.rel <- (area-area.min)/(area.max-area.min)
      
      #-------
      w.log.min <- cumsum(log(abundance.ord+1))/mx.log.csum
      area.log.min <- calcArea(tp.rate, w.log.min)      
      w.log.max <- cumsum(rev(log(abundance.ord+1)))/mx.log.csum
      area.log.max <- calcArea(tp.rate, w.log.max)      
      
      w.log <- cumsum(rev(log(abundance+1)))/mx.log.csum
      area.log <- calcArea(tp.rate, w.log)
      
      area.log.rel <- (area.log-area.log.min)/(area.log.max-area.log.min)      
    }
    #check auc
    #auc.area <- 1-calcArea(tp.rate,fpr.for.tpr(d,tp.rate))   #was consistent with auc  (less than 0.01 difference)
  }
  auc.usual=auc(d$presence,d$y)
  auc.bg=auc(d.bg$presence,d.bg$y) 
    
  #put results in a vector with column names (for binding in data.frame later)              
  out <- c(
    n=nrow(d),
    np=length(p),
    auc=auc.usual,      
    auc.bg=auc.bg)
  
  if (!is.null(abundance)){ 
    out <- c(out,
             area= area,
             area.log=area.log,
             area.rel=area.rel,
             area.log.rel=area.log.rel,
             #       area.max=area.max,
             #       area.log.max=area.log.max,
             #       intersect.auc.area.rel = auc.usual + area.rel -1,
             #       intersect.auc.area.log.rel = auc.usual + area.log.rel -1,  
    )
  }
  out
}

#'Accuracy function to evaluate a maxent model 
#'
#'Evaluate according to multiple criteria a maxent model (\code{\link{dismo::maxent}})
#'
#'@param me maxent model
#'@param p presences
#'@param a absences
#'@param abundance abundances, should be passed in same order as presences and same length
#'@return a vector of named arguments (n=number data, np=numer of presences, )
#'@export
#'
accuracy.me.simple <- function(me, p, a, abundance=NULL){   
  #   library(SDMTools)
  
  auc.me=me@results[5]  
  aicc.me <- aicc.me(me)
  
  a <- accuracy.simple(p,a, abundance)
  
  #put results in a vector with column names (for binding in data.frame later)              
  out <- c(
    n=a$n,
    np=a$np,
    auc=a$auc,
    auc.me=auc.me,       
    auc.bg=a$auc.bg)
  
  if (!is.null(abundance)){ 
    out <- c(out,
             area= a$area,
             area.log=a$area.log,
             area.rel=a$area.rel,
             area.log.rel=a$area.log.rel,
             #       area.max=area.max,
             #       area.log.max=area.log.max,
             #       intersect.auc.area.rel = auc.usual + area.rel -1,
             #       intersect.auc.area.log.rel = auc.usual + area.log.rel -1,  
             aicc.me = a$aicc.me
    )
  }
  out
}

#'Accuracy function to evaluate a glm model 
#'
#'Evaluate according to multiple criteria a glm model
#'
#'@param m glm model
#'@param p presences
#'@param a absences
#'@param abundance abundances, should be passed in same order as presences and same length
#'@return a vector of named arguments (n=number data, np=numer of presences, )
#'@export
#'
accuracy.glm.simple <- function(m, p,a, abundance=NULL){
  #   library(glmulti)  #for aicc
  
  a <- accuracy.simple(p,a, abundance)
  
  out <- a
  
  if (!is.null(abundance)){ 
    out <- c(out,
             area= a$area,
             area.log=a$area.log,
             area.rel=a$area.rel,
             area.log.rel=a$area.log.rel,
#              area.min=area.min,
#              area.max=area.max,
             #         area.log.max=area.log.max,  
             #         intersect.auc.area.rel = auc.usual + area.rel -1,
             #         intersect.auc.area.log.rel = auc.usual + area.log.rel -1,
             aic = m$aic,
             aicc = NA )#aicc(m)) #aicc(m))
  }
  out
  
}


#'Accuracy function to evaluate a maxent model for training and test dataset
#'
#'Evaluate according to multiple criteria a maxent model (\code{\link{dismo::maxent}}) for training and test dataset
#'
#'@param me maxent model
#'@param p presences
#'@param a absences
#'@param abundance abundances, should be passed in same order as presences and same length
#'@param test dataframe holding test data
#'@param depvar_name name of te dependent variable in the test dataframe
#'@param abundance_name name of te abundance variable in the test dataframe
#'@return a vector of named arguments (n=number data, np=numer of presences, )
#'@export
#'
accuracy.me.cross <- function(me, abundance, test=NULL, depvar_name, abundance_name){   
#   library(SDMTools)
  
  # --------------------------  calc training accuracy   (data stored into maxent model)
  p <-predict(me,me@presence) 
  a <-predict(me,me@absence)
  #   print(paste(length(abundance),length(p)))  #mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
  out <- accuracy.me.simple(me,p,a,abundance)
  
  
  #-----------------------------  calc test accuracy
  
  if (!is.null(test)){
    p <-predict(me,test[test[,depvar_name]==1,])    
    a <-predict(me,test[test[,depvar_name]==0,]) 
    abundance <- test[test[,depvar_name]==1,abundance_name]
    out.test <- accuracy.me.simple(me,p,a,abundance)
    names(out.test) <- paste(names(out.test),'.test',sep='')
    out <- c(out,out.test[-length(out.test)])  #don't copy aic test
  }
  
  out
}


#'Accuracy function to evaluate a gml model for training and test dataset
#'
#'Evaluate according to multiple criteria a gml model for training and test dataset
#'
#'@param m glm model
#'@param p presences
#'@param a absences
#'@param abundance abundances, should be passed in same order as presences and same length
#'@param test dataframe holding test data
#'@param depvar_name name of te dependent variable in the test dataframe
#'@param abundance_name name of te abundance variable in the test dataframe
#'@return a vector of named arguments (n=number data, np=numer of presences, )
#'@export
#'
accuracy.glm.cross <- function(m, abundance, test, depvar_name, abundance_name){   
#   library(SDMTools)
  
  
  # --------------------------  calc training accuracy   (data stored into model)
  #   p <-predict(m,m@presence) 
  #   a <-predict(m,m@absence)
  p <- m$fitted.values[m$model[,1]==1]
  a <- m$fitted.values[m$model[,1]!=1]
  #   print(paste(length(abundance),length(p)))  #mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
  out <- accuracy.glm.simple(m,p,a,abundance)
  
  
  #-----------------------------  calc test accuracy
  
  if (!missing(test)){
    p <-predict(m,test[test[,depvar_name]==1,])    
    a <-predict(m,test[test[,depvar_name]==0,]) 
    abundance <- test[test[,depvar_name]==1,abundance_name]
    out.test <- accuracy.glm.simple(m,p,a,abundance)
    names(out.test) <- paste(names(out.test),'.test',sep='')
    l <- length(out.test)
    out <- c(out,out.test[-c((l-1):l)])  #don't copy aic and aicc test
  }
  
  out
}





#' True positive rate (sensitivity)  
#' 
#'@param d dataframe 
#'@param thr threshold value
#'@param depvar_name name of the column holding the model output
#'@param colname name of the column holding presence [0/1]
#'@return true positive rate (sensitivity)
#'@export
#'  
tpr <- function(d, thr, depvar_name='y', ocurrence_colname='presence'){    
  #   singletpr <- function(d,thr){    
  #     nrow(d[d$presence==1 & d$y>=thr,])/ nrow(d[d$presence==1,])
  #   }
  #   Vectorize(singletpr,'thr')(d,thr)
  
  x <- d[d[, occurrence_colname]==1, depvar_name]
  n <- length(x)
  sapply(thr, FUN=function(th) length(x[x>=th])/n)
}


#' False positive rate (1- specificity) 
#' 
#'@param d dataframe 
#'@param thr threshold value
#'@param depvar_name name of the column holding the model output
#'@param ocurrence_colname name of the column holding presence [0/1]
#'@return false positive rate (1- specificity) 
#'@export
#'  
fpr <- function(d, thr, depvar_name='y', ocurrence_colname='presence'){     
  #   singlefpr <- function(d,thr){   
  #     nrow(d[d$presence==0 & d$y>=thr,])/ nrow(d[d$presence==0,])
  #   }
  #   Vectorize(singlefpr,'thr')(d,thr)
  
  x <- d$y[d$presence==0]
  n <- length(x)
  sapply(thr, FUN=function(th) length(x[x>=th])/n) 
}

#' Background proportion
#' 
#'@param d dataframe 
#'@param thr threshold value
#'@param depvar_name name of the column holding the model output
#'@return background proportion
#'@export
#'  
bkr <- function(d,thr, depvar_name='y'){    
  x <- d[, depvar_name]
  n <- length(x)
  sapply(thr, FUN=function(th) length(x[x>=th])/n) 
}


#' False positive rate for a given true positive rate
#' 
#'@param d dataframe 
#'@param tp,rate true positive rate
#'@param depvar_name name of the column holding the model output
#'@param ocurrence_colname name of the column holding presence [0/1]
#'@return false positive rate
#'@export
#'  
fpr.for.tpr <- function(d,tp.rate, depvar_name='y', ocurrence_colname='presence'){     
  d <- d[order(d[,depvar_name]),]
  thr <- approx( tpr(d,d[,depvar_name], depvar_name, occurrence_colname), d[,depvar_name] ,tp.rate, rule=2)[, depvar_name]
  fpr(d, thr,  depvar_name, occurrence_colname)
}


#' Background portion for a given true positive rate
#' 
#'@param d dataframe 
#'@param tp,rate true positive rate
#'@param depvar_name name of the column holding the model output
#'@param ocurrence_colname name of the column holding presence [0/1]
#'@return background proportion
#'@export
#'  
bkr.for.tpr <- function(d, tp.rate, depvar_name='y', ocurrence_colname='presence'){     
  d <- d[order(d[,depvar_name]),]
  thr <- approx(tpr(d,d[, depvar_name], depvar_name, occurrence_colname), d[,depvar_name], tp.rate, rule=2)[, depvar_name]
  #   Vectorize(function(x,thr) nrow(x[x$y>=thr,]),'thr' )(d, thr)/ nrow(d)
  
  bkr(d, thr, depvar_name)
  #   x <- d$y
  #   n <- length(d)
  #   sapply(thr, FUN=function(th) length(x[x>=th])/n)  
  
}





########################## functions to calculate maxent' AICc ############################

#' Extract lambda file values
#' 
#' Filla a dataframe with the lambda file values of a maxent model (\code{\link{dismo::maxent}})
#' 
#'@param m maxent model
#'@return dataframe with the lambda values (what, lambda, min, max)
#'@export
#' 
getMElambdas <- function(m){   #m is a maxent model
  m.par <- m@lambdas[1:(length(m@lambdas)-4)]
  tmp <- sapply(m.par, function(x) strsplit(x,','), USE.NAMES=F)   # split with comma
  lambdas <- data.frame(what=NA, lambda=NA,min=NA,max=NA)
  for (i in 1:length(tmp)) {
    x <- tmp[[i]]                                               #read rows and put into correct variables
    lambdas[i,1]<-x[1]
    lambdas[i,2]<-as.real(x[2])
    lambdas[i,3]<-as.real(x[3])
    lambdas[i,4]<-as.real(x[4])
    
  }
  lambdas
}  


#' Count the number of parameters (with lambda!=0) 
#' 
#' Cunts the number of parameters (with lambda!=0)  of a maxent model (\code{\link{dismo::maxent}})
#' 
#'@param m maxent model
#'@return number of parameters
#'@export
#' 
getMEparNum <- function(m){
  l <- getMElambdas(m)
  nrow(l[l$lambda!=0.0,]) 
}

#calculate aicc
aicc.me <- function(m, d, presence.name='presence', mpfr.precision=100){
  if (missing(d)){
    p <- m@presence
    a <- m@absence
    p[,presence.name] <- 1
    a[,presence.name] <- 0
    d <- rbind(p,a)
  }
  k <- getMEparNum(m)
  n <- nrow(d)
  if (k==0 || k>n){
    NA
  }else{
    #     library(Rmpfr)
    #     mraw <- predict(m, d, progress='text', args=c("outputformat=raw"))
    #     mraw.s <- mpfr(mraw/sum(mraw),mpfr.precision)
    # #     mraw.s <- mraw/sum(mraw)
    #     #   print(sum(mraw.s))
    #       
    #     ll <- as.real( log(prod(mraw.s[d[,presence.name]==1])) )
    # #     t <- prod(mraw.s[d[,presence.name]==1]*exp(8))   #prod(mraw.s[d$fire==1]*exp(8))
    # #     ll <- log(t) -8*length(mraw.s)
    #     
    #     2*k -2*ll + 2*k*(k+1)/(n-k-1)
    NA
  }
}



#' Constants of a maxent model
#' 
#' Extract the constants of a maxent model (\code{\link{dismo::maxent}})
#' 
#'@param m maxent model
#'@return dataframe holding the constants (what, value)
#'@export
#' 
getMEconstants <- function(m){   #m is a maxent model
  m.par <- m@lambdas[(length(m@lambdas)-3) : length(m@lambdas)]
  tmp <- sapply(m.par, function(x) strsplit(x,','), USE.NAMES=F)
  constants <- data.frame(what=NA, value=NA)
  for (i in 1:length(tmp)) {
    x <- tmp[[i]]
    constants[i,1]<-x[1]
    constants[i,2]<-as.real(x[2])    
  }
  constants
}

#calculate new values of maxent
#' Predict new values of a maxent model
#' 
#' Calculates new values of a maxent model (\code{\link{dismo::maxent}})
#' 
#'@param m maxent model
#'@param data frame holding the data
#'@return predictions
#'@export
#' 
predictME <- function(m, data){
  l <- getMElambdas(m)
  
  l$type <- 'RAW'
  l$type[grep('^2',l$what, fixed=T)] <- 'QUADRATIC'
  l$type[grep('*',l$what, fixed=T)] <- 'PRODUCT'
  l$type[grep('(',l$what, fixed=T)] <- 'THRESHOLD'
  l$type[grep("'",l$what, fixed=T)] <- 'FORWARD_HINGE'
  l$type[grep('`',l$what, fixed=T)] <- 'REVERSE_HINGE'
  l$var <- NA
  l$value <- NA
  
  for (i in 1:nrow(l)){
    s <- l[i,]
    #     print(s)
    
    s$var = switch(s$type, RAW = s$what,
                   QUADRATIC =  gsub('^2','',s$what, fixed=T),
                   FORWARD_HINGE = gsub("'",'',s$what, fixed=T),
                   REVERSE_HINGE  = gsub("`",'',s$what, fixed=T),
                   THRESHOLD = substr( strsplit(s$what,'<',fixed=T)[[1]][2], 1, nchar( strsplit(s$what,'<',fixed=T)[[1]][2]) -1),
                   PRODUCT = strsplit(s$what,'*',fixed=T)[[1]][1]   )
    
    
    s$value <- switch(s$type, RAW = s$lambda * (data[,s$var] - s$min)/(s$max - s$min),
                      QUADRATIC =  s$lambda * (data[,s$var]^2 - s$min)/(s$max - s$min),
                      FORWARD_HINGE =  if (data[,s$var] >= s$min)  s$lambda * (data[,s$var] - s$min)/(s$max - s$min) else 0,
                      REVERSE_HINGE=  if (data[,s$var] < s$max)  s$lambda * (s$max - data[,s$var])/(s$max - s$min) else 0,
                      THRESHOLD = if (data[,s$var] < as.double(substr( strsplit(s$what,'<',fixed=T)[[1]][1], 2, 999))) 0 else s$lambda,
                      PRODUCT = s$lambda * (data[,s$var] * data[,strsplit(s$what,'*',fixed=T)[[1]][2] ] - s$min)/(s$max - s$min)   )
    
    l[i,] <- s
  }
  #   print(l)
  cs <- getMEconstants(m)
  linearPredictorNormalizer <- cs$value[cs$what=='linearPredictorNormalizer']
  densityNormalizer <- cs$value[cs$what=='densityNormalizer']
  entropy <-  cs$value[cs$what=='entropy']
  
  S <- sum( l$value, na.rm=T) - linearPredictorNormalizer
  qx <- exp(S) / densityNormalizer
  
  out <- (qx*exp(entropy) / (1 + qx*exp(entropy)))
  out
}



