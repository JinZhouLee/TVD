#' Total Variation Distance
#'
#' @param data the data frame that need to calculate total variation distance
#' @param x the index of data, that the column need to be calculate probability density
#' @param y the index of data, that the showing the group
#' @param y_target the target in y
#' @param n the number of spaced in kernel density estimation
#' @param detail a logic, if true, that will show all of the detail
#'
#' @return if detail is true, return a list contain TVD detail; else, only return TVD
#' @examples
#' TVD(iris, 1:4, 5)
#' TVD(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), "Species")
#'
#' @export

TVD = function(data, x, y, y_target = c(), n = 512, detail = FALSE){
  p = length(x)
  ls = list()

  for(index in 1:p){
    result = TVD_for_one(data %>% select(all_of(c(x[index], y))), 1, 2, y_target = y_target, n = n, detail = detail)
    ls[[index]] = result
  }

  if(!detail){
    ls = unlist(ls)
    names(ls) = data %>% select(all_of(x)) %>% colnames()
  }

  return(ls)
}
