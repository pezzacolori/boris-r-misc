#' boris
#' 
#' @name boris
#' @docType package
#' @import reshape2

.onUnload <- function (libpath) {
  library.dynam.unload("boris", libpath)
}
