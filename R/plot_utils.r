



#-------------------------------functions to summarize graphically data-------------------------------------


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
yearplots <- function(data, vars, year='year',doy='doy', what=c('data','mean','na'),rows=floor(length(vars)/cols+1), cols=4){
  library(dplyr)
  d <-data.frame(data)
  d <- d %>% mutate(year=!!year,
                    doy=!!doy)
  d <- d %>% select(year, doy, vars)
  
  op <- par(no.readonly=T)
  par(mfrow=c(rows,cols))
  par(mar=c(2,4,1,1))
  par(oma=c(3,3,0,0))
  
  if (what[1]=='data'){
    for (v in vars){
      #   var <- d[,v]
      xlim=range(d$doy)
      ylim=range(d[,v],na.rm=T)
      for (y in min(d$x):max(d$year)){
        s <- d %>% filter(year==y)
        plot(s$y, s[,v],xlab='Doy',ylab=v,type='l',xlim=xlim,ylim=ylim)
        par(new=T)
      }
      par(new=F)
    }
  } else if (what[1]=='mean'){
    s <-  d %>% group_by(doy) %>% summarise_if(is.numeric, mean, na.rm=T) 
    
    sd <- d %>% group_by(doy) %>% summarise_if(is.numeric,sd, na.rm=T)
    #     print(sd)
    mx <- d %>% group_by(doy) %>% summarise_if(is.numeric,max,na.rm=T)
    mn <- d %>% group_by(doy) %>% summarise_if(is.numeric,min,na.rm=T)
    #     print(mn)
    q90<- d %>% group_by(doy) %>% summarise_if(is.numeric,function(x) quantile(x,0.90,na.rm=T))
    #     print(q90)
    q10 <- d %>% group_by(doy) %>% summarise_if(is.numeric,function(x) quantile(x,0.10,na.rm=T))
    sd$doy=s$doy
    for (v in vars){
      xlim=range(d$doy)
      ylim=range(d[,v],na.rm=T)
      plot(s$y, s[,v], ylab=v,type='l',xlim=xlim,ylim=ylim)
      #       lines(sd$y,s[,v]+sd[,v],col='grey')
      #       lines(sd$y,s[,v]-sd[,v],col='grey')
      lines(q90$doy,q90[,v],col='grey')
      lines(q10$doy,q10[,v],col='grey')
      lines(mx$doy,mx[,v],lty=3)
      lines(mn$doy,mn[,v],lty=3)
    }    
  } else {   # NAs
    for (v in vars){
      xlim=range(d$doy)
      ylim=range(d$year)
      plot(d$doy, ifelse(is.na(d[,v]),d$x,NA), ylab=v,type='l',xlim=xlim,ylim=ylim)
    }
  }
  par(op)
}
