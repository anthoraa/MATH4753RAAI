#' Confidence Interval Function
#'
#' @param x data
#'
#' @return confidence interval from t test
#' @export
#'
#' @examples
#' myci(c(1, 5, 10, 2, 4, 6))
myci = function(x) {
  t = t.test(x, conf.level = 0.95)
  return(t$conf.int)
}
