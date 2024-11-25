#' ntickets
#'
#' @param N number of seats
#' @param gamma probability of overbooked
#' @param p probability of show
#'
#' @return plots and a table
#' @importFrom graphics abline text title
#' @importFrom stats optimize pbinom
#' @export
#'
#' @examples
#' ntickets(200,.02,.95)
#' ntickets(400,.02,.95)
ntickets <- function(N, gamma, p) {

  low = N - N/8
  high = N + N/8
  # Objective function for discrete case using binomial distribution
  objective_binom <- function(n, N, p) {
    1 - pbinom(N, size = n, prob = p)
  }

  # Objective function for continuous case using normal approximation
  objective_normal <- function(n, N, p) {
    mu <- n * p  # mean
    sigma <- sqrt(n * p * (1 - p))  # standard deviation
    1 - pnorm(N + 0.5, mean = mu, sd = sigma)
  }

  # Find the optimal n for discrete case
  optimize_discrete <- function(N, p, gamma) {
    n_values <- low:high  # Search over this range
    objective_values <- sapply(n_values, objective_binom, N = N, p = p)
    n_opt <- n_values[which.min(abs(objective_values - gamma))]
    return(n_opt)
  }

  # Find the optimal n for continuous case
  optimize_continuous <- function(N, p, gamma) {
    result <- optimize(function(n) abs(objective_normal(n, N, p) - gamma), interval = c(low, high))
    return(result$minimum)
  }

  ### Plot for Discrete Case
  n_values <- low:high
  objective_values <- sapply(n_values, objective_binom, N = N, p = p)
  optimal_n_discrete <- optimize_discrete(N, p, gamma)

  plot(n_values, objective_values, type = 'o', pch = 19, col = "blue", ylim = c(0, 1),
       main = "Objective Vs n to find optimal tickets sold (Discrete)",
       xlab = "n", ylab = "Objective")

  abline(h = gamma, col = "red", lwd = 2)

  abline(v = optimal_n_discrete, col = "red", lwd = 2)

  text(optimal_n_discrete, gamma, labels = paste("(", optimal_n_discrete, ")", sep = ""), pos = 4, col = "red")

  # Add subtitle for discrete case
  title(sub = paste("(", optimal_n_discrete, ") gamma=", gamma, " N=", N, " discrete", sep = ""))

  ### Plot for Continuous Case
  n_values_cont <- seq(low, high, by = 0.5)
  objective_values_cont <- sapply(n_values_cont, objective_normal, N = N, p = p)
  optimal_n_continuous <- optimize_continuous(N, p, gamma)

  plot(n_values_cont, objective_values_cont, type = 'l', lwd = 2, col = "black", ylim = c(0, 1),
       main = "Objective Vs n to find optimal tickets sold (Continuous)",
       xlab = "n", ylab = "Objective")

  abline(h = gamma, col = "blue", lwd = 2)

  abline(v = optimal_n_continuous, col = "blue", lwd = 2)

  # Annotate optimal n
  text(optimal_n_continuous, gamma, labels = paste("(", round(optimal_n_continuous, 3), ")", sep = ""), pos = 4, col = "blue")

  # Add subtitle for continuous case
  title(sub = paste("(", round(optimal_n_continuous, 3), ") gamma=", gamma, " N=", N, " continuous", sep = ""))

  # Create and print the named list
  result_list <- list(
    nd = optimal_n_discrete,
    nc = round(optimal_n_continuous, 3),
    N = N,
    p = p,
    gamma = gamma
  )

  print(result_list)

  return(result_list)
}


