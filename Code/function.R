###################
# functions.R
#
###################

pie <- function(df, column) {

  # An R function with a parameter that accepts a data.frame column can't evaluate
  # the column argument until it is first 'quoted', followed by an 'unquote' within
  # the dyplr function. 'Quote' a column using enquo(), then 'unquote' it using !!.
  column <- enquo(column)
  
  dfsum <- df %>%
    filter(!is.na(!!column)) %>% 
    group_by(!!column) %>% # Group by specified column
    tally() %>% # Number of observations in each group
    mutate(prop = n / sum(n)) %>% 
    arrange(desc(!!column)) %>%
    mutate(ypos = cumsum(prop) - 0.5*prop)
  
  ggplot(data = dfsum,
         mapping = aes(
           x = "",
           y = prop,
           fill = !!column
         ),
         color = "white") +
    geom_col() + # White border color
    geom_text(aes(label = scales::percent(prop, 0.01)),
              position = position_stack(vjust = 0.5),
              color = "white") +
    # geom_text(aes(y = ypos, 
    #               label = scales::percent(prop, 0.01)), 
    #color = "white") +
    scale_fill_lancet() +
    coord_polar(theta = "y", direction = -1) + # Pie chart
    labs(fill = "") + # Remove legend title
    theme_void() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1))
  
}
