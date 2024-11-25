#' returns rss
#'
#' @param yhat a value
#' @param mean a value
#'
#' @return a value
#' @export
#'
#' @examples
#' myrss(10,1)
myrss <- function(yhat,mean){
  sum((yhat - mean)^2)
}
