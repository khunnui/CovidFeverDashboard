###################
# functions.R
#
###################

pie <- function(df, column, tt, sort = FALSE, colors) {
  
  # An R function with a parameter that accepts a data.frame column can't evaluate
  # the column argument until it is first 'quoted', followed by an 'unquote' within
  # the dyplr function. 'Quote' a column using enquo(), then 'unquote' it using !!.
  column = enquo(column)
  
  df %>%
    group_by(!!column) %>% # Group by specified column
    summarise(count = sum(n)) %>% # Number of observations in each group
    arrange(!!column) %>% 
    plot_ly() %>%
    add_trace(
      labels = column,
      values = ~ count,
      type = 'pie',
      direction ='clockwise',
      sort = sort,
      marker = list(
        colors = colors,
        line = list(color = '#FFFFFF', width = 1)
      ),
      texttemplate = "%{percent:.1%}",
      hovertemplate = '%{value:,}<extra></extra>'
    ) %>%
    layout(
      title = tt,
      margin = list(l = 30, r = 30, t = 30, b = 30, pad = 20),
      legend = list(
        orientation = "h",
        # show entries horizontally
        xanchor = "center",
        # use center of legend as anchor
        x = 0.5
      )
    )
  
}

pie_gender <- function(df, tt) {

  df %>%
    group_by(s1gender) %>% # Group by specified column
    summarise(count = sum(n)) %>% # Number of observations in each group
    plot_ly() %>%
    add_trace(
      labels = ~ s1gender,
      values = ~ count,
      type = 'pie',
      direction ='clockwise',
      sort = FALSE,
      marker = list(
        colors = color_gender,
        line = list(color = '#FFFFFF', width = 1)
      ),
      textinfo = 'label+percent',
      texttemplate = "%{label}: %{percent:.1%}",
      hoverinfo = 'label+value',
      hovertemplate = '%{label}: %{value:,}<extra></extra>'
    ) %>%
    layout(
      title = tt,
      margin = list(l = 30, r = 30, t = 30),
      showlegend = FALSE
    )
  
}

bar_age <- function(df, tt) {
  
  plot_ly(
    data = df %>%
      group_by(agegroup) %>%
      summarise(count = sum(n)),
    x = ~ agegroup,
    y = ~ count,
    type = "bar",
    marker = list(color = color_age),
    hoverinfo = 'y'
  ) %>%
    layout(
      title = tt,
      xaxis = list(title = 'Age Group'),
      yaxis = list(title = 'Number Screened',
                   tickformat = ','),
      bargap = 0.5,
      margin = list(l = 20, r = 20)
    )
  
}

bar_h <- function(df, column, tt) {
  
  # An R function with a parameter that accepts a data.frame column can't evaluate
  # the column argument until it is first 'quoted', followed by an 'unquote' within
  # the dyplr function. 'Quote' a column using enquo(), then 'unquote' it using !!.
  column = enquo(column)

  plot_ly(
    data = df %>% 
      group_by(finalresult, !!column) %>% 
      summarise(count = sum(n)),
    y = column,
    x = ~ count,
    type = "bar",
    orientation = 'h',
    color = ~ finalresult,
    colors = color_posneg,
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
    layout(legend = list(
      traceorder = "normal",
      y = 0.9, 
      yanchor = "top", 
      margin = list(l = 50, r = 50, b = 50, t = 50, pad = 20)))
  
}

bar_scale <- function(df, column, colors) {

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
                        autorange = "reversed",
                        ticks = "outside", 
                        tickcolor='white', 
                        ticklen = 10),
           legend = list(traceorder = "normal",
                         orientation = "h"))

}

sunburst_df <- function(DF, value_column = NULL, add_root = FALSE){
  require(data.table)
  
  colNamesDF <- names(DF)
  
  if(is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  
  if(add_root){
    DT[, root := "Total"]  
  }
  
  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)
  DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]
  
  if(is.null(value_column) && add_root){
    setcolorder(DT, c("root", colNamesDF))
  } else if(!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }
  
  hierarchyList <- list()
  
  for(i in seq_along(hierarchy_columns)){
    current_columns <- colNamesDT[1:i]
    if(is.null(value_column)){
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
    }
    setnames(currentDT, length(current_columns), "labels")
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
  hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parent_columns) := NULL]
  return(hierarchyDT)
}

