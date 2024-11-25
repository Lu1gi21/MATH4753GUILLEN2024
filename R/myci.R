#' myci
#'
#' @param sample the data to be tested
#' @param conf_interval what percentage of confidence
#'
#' @return 2 values
#' @importFrom stats qt
#' @export
#'
#' @examples
#' myci(rnorm(30,mean=10,sd=12), conf_interval= .95)
myci <- function(sample, conf_interval){

  mean <- mean(sample)
  sd <- sd(sample)
  n <- length(sample)

  error <- qt(1- (1-conf_interval)/2, n -1) * (sd/sqrt(n))

  c(mean - error, mean+error)


}
