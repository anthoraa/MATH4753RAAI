#' My n curve
#'
#' @param mu distribution mean
#' @param sigma distribution standard deviation
#' @param a x value used to calculate P(Y < a)
#'
#' @return graph of curve with region and area/probability plotted as well as a list of mu, sigma, and a
#' @export
#'
#' @examples
#' myncurve(mu = 100, sigma = 15, a = 95)
myncurve = function(mu, sigma, a){
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3*sigma, mu + 3*sigma))
  xcurve = seq(mu - 5 * sigma, a, length = 1000) # Instead of -infinity, minus 5 standard deviations, which should be more than enough given the empirical rule!
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(mu - 3 * sigma, xcurve, a), c(0, ycurve, 0), col = "Red")
  prob = round(pnorm(a, mean = mu, sd = sigma), 4)
  text(x = mu, y = 0.5 * dnorm(mu, mean = mu, sd = sigma), paste0("Prob = ", prob))
  return(list(mu = mu, sigma = sigma, a = a))
}
