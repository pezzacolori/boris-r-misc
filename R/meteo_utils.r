

#'Resample a data.frame with meteorological data with hourly interval to a daily interval,
#'allowing the specification at which time to cut the day (e.g. can be summarized foro noon to noon).
#'
#'@param h a data.frame holding the hourly data
#'@param time_h hour value at which to cut the hourly data.frame to build summaries (e.g. 12 for noon to noon).
#'Defaults to 24.
#'@param timevar name of the column holding the date-time information (in POSIX numeric format).
#'@param varnames character vector holding the names of the columns holding the variables to be processed.
#'@param aggregation character vector holding the types of aggregations to perform on the selected variables.
#'@param na.rm boolean to specify if aggregation function should consider or skip NA's.
#'@param add_suffix boolean to specify if the vairable names sohould be completed with the specification of the aggregation.
#'@return a new data.frame with daily timestep and the selected variables and aggregations .
#'@seealso \code{\link{kfold}}
#'@export
#'
resample.meteo_h2d <- function(h, time_h=24, timevar, varnames, aggregation = c('sample','sum','mean','max','min'), na.rm=F,
                               add_suffix=F){
  library(plyr)
  
  h$dtnum <- h[, timevar]
  h$day <- trunc(h$dtnum)
  if (aggregation=='sample') {
    d <- h[round(h$dtnum - trunc(h$dtnum),10)== round(time_h/24,10),c("day","dtnum",varnames)]
    #names(d) <-c("day","dtnum", paste(varnames, time_h, sep=''))
    if (add_suffix) names(d) <-c("day","dtnum", paste(varnames, time_h, sep=''))
    d <- d[-2]
  } else if (aggregation %in% c('sum','mean','max','min')){
    prefix= 'ddply(h[,c("dtnum",varnames)], .(trunc(dtnum +(24-time_h-0.1)/24)), summarise,'          # -0.1 is to avoid errors due to floating
    cols =  sapply(varnames, function(x) paste(x,'=',aggregation,'(',x,', na.rm=',na.rm ,')', sep=''))
    d <- evaltext(paste(prefix, paste(cols,collapse=','),')', sep=''))
    if (add_suffix) names(d) <-c("day", paste(varnames, aggregation, time_h, sep='_'))
  }
  
  names(d)[1] <- 'day'
  d$day <- chron(d$day)
  d
}


