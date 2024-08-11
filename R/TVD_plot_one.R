#' Total Variation Distance plot for one column
#'
#' @param tvd a list that come from TVD function
#'
#' @examples
#' tvd = TVD(iris, 1, 5, detail = TRUE)
#' TVD_plot_one(tvd[[1]])
#'
#' @export

TVD_plot_one = function(tvd){
  long_data = data.frame(x = tvd$x,
                         y1 = tvd$y1,
                         y2 = tvd$y2,
                         diff = tvd$diff) %>%
    pivot_longer(col = c("y1", "y2", "diff"))
  long_data$name = factor(long_data$name, levels = c("y1", "y2", "diff"))

  p = ggplot(long_data, aes(x = x, y = value, color = name)) +
    geom_line(linewidth = 1, alpha = 0.7) +
    scale_color_manual(
      values = c("y1" = "green", "y2" = "red", "diff" = "black"),
      labels = c("y1" = tvd$y_target[1], "y2" = tvd$y_target[2], "diff" = "Different")) +
    labs(title = paste0("Density plot of ", tvd$x_name, "\n",
                        "Total variation distance = " , round(tvd$TVD, 3)),
         x = "x", y = "density", color = tvd$y_name)
  print(p)
}
