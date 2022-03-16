###################
# functions.R
#
###################

pie <- function(df, column, tt) {

  # An R function with a parameter that accepts a data.frame column can't evaluate
  # the column argument until it is first 'quoted', followed by an 'unquote' within
  # the dyplr function. 'Quote' a column using enquo(), then 'unquote' it using !!.
  column = enquo(column)
  
  df %>%
    group_by(!!column) %>% # Group by specified column
    summarise(count = sum(n)) %>% # Number of observations in each group
    plot_ly() %>%
    add_trace(
      labels = column,
      values = ~ count,
      #name = column,
      type = 'pie',
      marker = list(
        colors = colors,
        line = list(color = '#FFFFFF', width = 1)
      ),
      texttemplate = "%{percent:.1%}",
      hovertemplate = "%{percent:.1%}"
    ) %>%
    layout(
      title = tt,
      margin = list(l = 5, r = 5),
      legend = list(
        orientation = "h",
        # show entries horizontally
        xanchor = "center",
        # use center of legend as anchor
        x = 0.5
      )
    )             # put legend in center of x-axis)  # use center of legend as anchor)
  
}
