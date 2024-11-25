#' myncurve
#'
#' @param mu Mean of the normal distribution.
#' @param a the value up to which the area under the curve is shaded, used to calculate the cumulative probability P(X ≤ a).
#' @param sigma Standard deviation of the normal distribution.
#'
#' @return graph and probability
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#' @export
#' @examples
#' myncurve(mu = 0, sigma = 1, a = 1.5)
#' myncurve(mu = 5, sigma = 2, a = 6)
myncurve = function(mu, sigma, a) {

  x <- seq(mu - 3*sigma, mu + 3*sigma, length=1000)

  curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu - 3*sigma, mu + 3*sigma),
        ylab = "Density", xlab = "x", main = paste("Normal Curve (mu =", mu, ", sigma =", sigma, ")"))


  x_shade <- seq(mu - 3*sigma, a, length=1000)
  y_shade <- dnorm(x_shade, mean=mu, sd=sigma)
  polygon(c(mu - 3*sigma, x_shade, a), c(0, y_shade, 0), col="skyblue", border=NA)


  prob <- pnorm(a, mean=mu, sd=sigma)


  list(mu = mu, sigma = sigma, a = a, probability = prob)
}
