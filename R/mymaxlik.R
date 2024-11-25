#' mymaxlik
#'
#' @param lfun function to estimate max like for
#' @param x your data
#' @param param range of vlaues to eval
#' @param ... other parameters
#'
#' @return graph and table
#' @importFrom  graphics abline axis points
#' @export
#'
#' @examples
#' mymaxlik(lfun = function(x, mu) dnorm(x, mean = mu, sd = 1, log = TRUE),
#' x = c(-1.2, 0.5, 1.1, 0.8, -0.7, 0.3), param = seq(-2, 2, length.out = 100))
mymaxlik=function(lfun,x,param,...){
  # how many param values are there?
  np=length(param)
  # outer -- notice the order, x then param
  # this produces a matrix -- try outer(1:4,5:10,function(x,y) paste(x,y,sep=" "))   to understand
  z=outer(x,param,lfun) # A: Applies lfun to x and param
  # z is a matrix where each x,param is replaced with the function evaluated at those values
  y=apply(z,2,sum)

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  plot(param,y,col="Blue",type="l",lwd=2,...)
  # which gives the index for the value of y == max.
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger indice
  i=max(which(y==max(y))) # B: This line identifies the index of the maximum value in y, where y is the vector of summed log-likelihoods over all samples for each parameter value.
  abline(v=param[i],lwd=2,col="Red")

  # plots a nice point where the max lik is
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  #check slopes. If it is a max the slope shoud change sign from + to
  # We should get three + and two -vs
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}
