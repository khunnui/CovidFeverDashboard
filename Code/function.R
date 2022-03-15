###################
# functions.R
#
###################

pie <- function(df, column) {

  # An R function with a parameter that accepts a data.frame column can't evaluate
  # the column argument until it is first 'quoted', followed by an 'unquote' within
  # the dyplr function. 'Quote' a column using enquo(), then 'unquote' it using !!.

  plot_ly(
    data = df,
    labels = ~ df[[1]],
    values = ~ count,
    type = 'pie'
  )
  #   geom_col(
  #     mapping = aes(x = "",
  #                   y = prop,
  #                   fill = !!column),
  #     color = "white",
  #     alpha = 0.5
  #   ) +
  #   geom_text(
  #     mapping = aes(x = "",
  #                   y = prop,
  #                   label = scales::percent(prop, 0.01)),
  #     position = position_stack(vjust = 0.5),
  #     color = "white"
  #   ) +
  #   scale_fill_lancet() +
  #   coord_polar(theta = "y", direction = -1) + # Pie chart
  #   labs(fill = "") + # Remove legend title
  #   theme_void() +
  #   theme(legend.position = "bottom") +
  #   guides(fill = guide_legend(nrow = 1))
  
}
