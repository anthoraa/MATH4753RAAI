#' Pareto Function
#'
#' @param x any vector
#' @param mn title of barplot
#' @param ... allows for more arguments
#'
#' @return pareto barplot
#' @export
#'
#' @examples
#' pareto(x = c(1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5))
pareto <- function(x, mn = "Pareto barplot", ...) {
  x.tab = table(x)
  xx.tab = sort(x.tab, decreasing = TRUE, index.return = FALSE)
  cumsum(as.vector(xx.tab)) -> cs
  length(x.tab) -> lenx
  bp <- barplot(xx.tab, ylim = c(0,max(cs)),las = 2)
  lb <- seq(0,cs[lenx], l = 11)
  axis(side = 4, at = lb, labels = paste(seq(0, 100, length = 11), "%", sep = ""), las = 1, line = -1, col = "Blue", col.axis = "Red")
  for(i in 1:(lenx-1)){
    segments(bp[i], cs[i], bp[i+1], cs[i+1], col = i, lwd = 2)
  }
  title(main = mn, ...)
}
