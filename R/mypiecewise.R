#' Piece wise equation
#'
#' @param x a vector
#' @param xk a int for assigning the break
#' @param coef a vector of coeficients of a quadratic linear model.
#'
#' @return a vector
#' @export
#'
#' @examples
#' mypiecewise(1,10,100)
mypiecewise <-function(x,xk,coef){
  coef[1] + coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}
