#' Total Variation Distance plot
#'
#' @param data the data frame that need to calculate total variation distance
#' @param x the index of data, that the column need to be calculate probability density
#' @param y the index of data, that the showing the group
#' @param y_target the target in y
#' @param n the number of spaced in kernel density estimation
#'
#' @examples
#' TVD_plot(iris, 1:4, 5)
#' TVD_plot(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), "Species")
#'
#' @export


TVD_plot = function(data, x, y, y_target = c(), n = 512){
  tvd = TVD(data, x = x, y = y, y_target = y_target, n = n, detail = T)
  p = length(x)

  for(index in 1:p){
    TVD_plot_one(tvd[[index]])
  }
}
