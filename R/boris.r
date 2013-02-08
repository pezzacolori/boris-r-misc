#' boris
#' 
#' @name boris
#' @docType package
#' @import plyr reshape2 foreach zoo SDMTools

.onUnload <- function (libpath) {
  library.dynam.unload("boris", libpath)
}
