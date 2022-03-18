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
      sort = FALSE,
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
    )
  
}

hbar <- function(df, column, tt) {
  
  # An R function with a parameter that accepts a data.frame column can't evaluate
  # the column argument until it is first 'quoted', followed by an 'unquote' within
  # the dyplr function. 'Quote' a column using enquo(), then 'unquote' it using !!.
  column = enquo(column)

  plot_ly(
    data = df %>% 
      group_by(FinalResult, !!column) %>% 
      summarise(count = sum(n)),
    y = column,
    x = ~ count,
    type = "bar",
    orientation = 'h',
    color = ~ FinalResult,
    hoverinfo = 'x'
  ) %>% 
    layout(
      title = tt,
      barmode = 'stack',
      xaxis = list(title = 'Count',
                   bargap = 0.5),
      yaxis = list(title = '',
                   categoryorder = "total ascending",
                   ticks = "outside", 
                   tickcolor='white', 
                   ticklen = 10)
    ) %>%
    add_annotations(
      text = "PCR Result",
      xref = "paper",
      yref = "paper",
      x = 1.037,
      xanchor = "left",
      y = 0.9,
      yanchor = "bottom",
      # Same y as legend below
      legendtitle = TRUE,
      showarrow = FALSE
    ) %>%
    layout(legend = list(y = 0.9, yanchor = "top"))
  
}

scalebar <- function(df, column, colors) {

  # An R function with a parameter that accepts a data.frame column can't evaluate
  # the column argument until it is first 'quoted', followed by an 'unquote' within
  # the dyplr function. 'Quote' a column using enquo(), then 'unquote' it using !!.
  column = enquo(column)
  
  plot_ly(
    data = df %>%
      group_by(!!column, scale) %>%
      summarise(count = sum(n)) %>% 
      mutate(pct = count/sum(count)),
    y = column,
    x = ~ pct,
    type = "bar",
    orientation = 'h',
    color = ~ scale,
    colors = colors,
    hoverinfo = 'x'
  ) %>% 
    layout(barmode = 'stack',
           bargap = 0.3,
           xaxis = list(title = '',
                        tickformat = '.0%'),
           yaxis = list(title = '',
                        ticks = "outside", 
                        tickcolor='white', 
                        ticklen = 10),
           legend = list(traceorder = "normal",
                         orientation = "h"))

}