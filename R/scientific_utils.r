


#'Vapour pressure deficit
#'
#'Calculate vapour pressure deficit from temperature and humidity
#'
#'@param T numeric vector with air temperature values [C]
#'@param H numeric vector with air humidity values [\%]
#'@return vapur pressure deficit
#'@export
#'
vpd <- function(T,H){
  es <-   0.6108*exp(17.27*T/(T+237.3))
  es*(1-H/100)
}
