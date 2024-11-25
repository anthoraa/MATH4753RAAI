#' n tickets Function
#'
#' @param N number of seats
#' @param gamma overbooking probability
#' @param p probability of show
#'
#' @return list with nd (number of tickets for discrete), nc (number of tickets for continuous), N, gamma, p
#' @export
#'
#' @examples
#' ntickets(400, 0.02, 0.95)
ntickets = function(N, gamma, p){ # N = number of seats, p = probability of show, gamma = overbooking prob, calculate the number of tickets to be sold

  # Discrete distribution
  discrete <- function(n) {
    abs(1 - gamma - pbinom(N, size = round(n), prob = p)) # = 0, from pbinom(...) = 1 - gamma
    # Additionally, round n because this is a discrete distribution
  }

  # Normal distribution
  normal <- function(n) {
    mu = n * p # Mean
    sigma = sqrt(n * p * (1 - p)) # Standard deviation
    abs(1 - gamma - pnorm(N + 0.5, mean = mu, sd = sigma)) # = 0, from pnorm(...) = 1 - gamma
    # Additionally, add 0.5 to N to account for a continuous distribution
  }

  # Find n using optimize(), interval of N to N + (N*0.1) (you should never sell less tickets than there are seats on the flight, hence the lower bound of N)
  nd = round(optimize(discrete, interval = c(N, N + (N*0.1)))$min) # Round nd because it is from a discrete distribution
  nc = optimize(normal, interval = c(N, N + (N*0.1)))$min

  # Discrete curve
  curve(discrete, xlim = c(N, N + (N*0.1)), lwd = 2,
        main = paste("Objective vs n to find optimal tickets sold\n(n =", nd, "),", "Gamma =", gamma, ", N =", N, ", discrete"),
        xlab = "n", ylab = "Objective", type = "n")

  points(N:(N + (N*0.1)), sapply(N:(N + (N*0.1)), discrete), pch = 19, col = "Blue")
  lines(N:(N + (N*0.1)), sapply(N:(N + (N*0.1)), discrete), col = "Black", lwd = 2)

  abline(v = nd, col = "Red")
  abline(h = discrete(nd), col = "Red")

  # Normal curve
  curve(normal, xlim = c(N, N + (N*0.1)), lwd = 2,
        main = paste("Objective vs n to find optimal tickets sold\n(n =", nc, "),", "Gamma =", gamma, ", N =", N, ", continuous"),
        xlab = "n", ylab = "Objective")

  abline(v = nc, col = "Blue")
  abline(h = normal(nc), col = "Blue")

  # List
  l = list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  return(l)
}
