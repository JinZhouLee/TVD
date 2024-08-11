#' Total Variation Distance for one column
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
#' TVD_for_one(iris, 1, 5)
#' TVD_for_one(iris, "Sepal.Length", "Species")
#'
#' @export

TVD_for_one = function(data, x, y, y_target = c(), n = 512, detail = FALSE){
  x_name = data %>% select(all_of(x)) %>% colnames()
  y_name = data %>% select(all_of(y)) %>% colnames()

  if(length(y_target) == 0){
    y_target = unique(data[, y])[1:2] %>% as.character()
  }

  group1 = which(data[, y] == y_target[1])
  group2 = which(data[, y] == y_target[2])

  from_to = range(data[union(group1, group2), x_name])
  dx = (from_to[2] - from_to[1]) / n

  density1 = density(data[group1, x_name], from = from_to[1], to = from_to[2])
  density2 = density(data[group2, x_name], from = from_to[1], to = from_to[2])
  diff = abs(density1$y - density2$y)
  TVD = sum(diff) * dx / 2

  if(detail){
    ls = list(x_name = x_name,
              y_name = y_name,
              y_target = y_target,
              n = n, dx = dx,
              x = density1$x,
              y1 = density1$y,
              y2 = density2$y,
              diff = diff,
              TVD = TVD)
  }else{
    ls = TVD
  }

  return(ls)
}
