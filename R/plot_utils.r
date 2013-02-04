



#-------------------------------functions to summarize graphically data-------------------------------------
#plots yearly data
#data -> plot all the data
#mean -> plot mean, sd (grey) and min/max
#NAs  -> plot a line for a period of missing data (the different years are on y axis)
#'Multiple plots of daily data over years
#'
#'Plots summaries of daily data in a yearly plot
#'
#'@param data dataframe with data
#'@param vars character vector with variable (columns) names holding the data to summarise
#'@param year name of the column holding the year
#'@param doy name of the column holding the doy  [1-366]
#'@param what what to plot: all values, means over the years or presence of NA's 
#'@param rows number of rows for the multiple plots. Defaults accordind to the number of 
#'@param cols number of columns for the multiple plots. Defaults to 4 
#'@return plot
#'@export
#'
plotOverview <- function(data, vars, year='x',doy='y', what=c('data','mean','na'),rows=floor(length(vars)/cols+1), cols=4){
  library(plyr)
  d<-data
  if (year!='x') d$x<-d[,year]
  if (doy!='y') d$y<-d[,doy]
  op <- par(no.readonly=T)
  par(mfrow=c(rows,cols))
  par(mar=c(2,4,1,1))
  par(oma=c(3,3,0,0))
  
  if (what[1]=='data'){
    for (v in vars){
      #   var <- d[,v]
      xlim=range(d$y)
      ylim=range(d[,v],na.rm=T)
      for (year in min(d$x):max(d$x)){
        s <- d[d$x==year,] 
        plot(s$y, s[,v],xlab='Doy',ylab=v,type='l',xlim=xlim,ylim=ylim)
        par(new=T)
      }
      par(new=F)
    }
  } else if (what[1]=='mean'){
    s <- ddply(d,.(y),mean)
    sd <- ddply(d,.(y),function(x) sd(x))
    #     print(sd)
    mx <- ddply(d,.(y),numcolwise(max))
    mn <- ddply(d,.(y),numcolwise(min))
    #     print(mn)
    q90 <- ddply(d,.(y),numcolwise(function(x) quantile(x,0.90,na.rm=T)))
    #     print(q90)
    q10 <- ddply(d,.(y),numcolwise(function(x) quantile(x,0.10,na.rm=T)))
    sd$y=s$y
    for (v in vars){
      xlim=range(d$y)
      ylim=range(d[,v],na.rm=T)
      plot(s$y, s[,v], ylab=v,type='l',xlim=xlim,ylim=ylim)
      #       lines(sd$y,s[,v]+sd[,v],col='grey')
      #       lines(sd$y,s[,v]-sd[,v],col='grey')
      lines(q90$y,q90[,v],col='grey')
      lines(q10$y,q10[,v],col='grey')
      lines(mx$y,mx[,v],lty=3)
      lines(mn$y,mn[,v],lty=3)
    }    
  } else {   # NAs
    for (v in vars){
      xlim=range(d$y)
      ylim=range(d$x)
      plot(d$y, ifelse(is.na(d[,v]),d$x,NA), ylab=v,type='l',xlim=xlim,ylim=ylim)
    }
  }
  par(op)
}