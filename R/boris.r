#' boris
#' 
#' @name boris
#' @docType package
#' @import reshape2 zoo

.onUnload <- function (libpath) {
  library.dynam.unload("boris", libpath)
}
