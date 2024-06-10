library(gt)
library(rstatix)

# detach("package:gtExtras", unload = TRUE)
# devtools::install_github("Nartladac/gtExtras")
library(gtExtras)

###################
# functions.R
#
###################

pie1 <- function(df, column, tt, colors) {

  # An R function with a parameter that accepts a data.frame column can't evaluate
  # the column argument until it is first 'quoted', followed by an 'unquote' within
  # the dyplr function. 'Quote' a column using enquo(), then 'unquote' it using !!.
  column = enquo(column)

  df <- df %>%
    group_by(!!column) %>% # Group by specified column
    summarise(count = sum(n)) # Number of observations in each group
  
  plot_ly(df) %>%
    add_trace(
      labels = column,
      values = ~ count,
      type = 'pie',
      sort = FALSE,
      rotation = 0 - df$count[1] / sum(df$count) * 360,
      marker = list(
        colors = colors,
        line = list(color = '#FFFFFF', width = 1)
      ),
      textinfo = 'label+percent',
      texttemplate = "%{label}: %{percent:.1%}",
      hoverinfo = 'label+value',
      hovertemplate = '%{label}: %{value:,}<extra></extra>'
    ) %>%
    layout(
      title = list(text = tt, font = list(family = "Verdana", size = 14)),
      margin = list(l = 30, r = 30, t = 30),
      showlegend = FALSE
    )
  
}

pie2 <- function(df, column, tt, rotate = 0) {
  
  # An R function with a parameter that accepts a data.frame column can't evaluate
  # the column argument until it is first 'quoted', followed by an 'unquote' within
  # the dyplr function. 'Quote' a column using enquo(), then 'unquote' it using !!.
  column = enquo(column)
  
  df <- df %>%
    group_by(!!column) %>% # Group by specified column
    summarise(count = sum(n)) %>% # Number of observations in each group
    arrange(desc(count))

  plot_ly(df) %>%
    add_trace(
      labels = column,
      values = ~ count,
      type = 'pie',
      rotation = rotate - df$count[1] / sum(df$count) * 360,
      marker = list(
        colors = color_qual,
        line = list(color = '#FFFFFF', width = 1)
      ),
      texttemplate = "%{percent:.1%}",
      hoverinfo = 'label+value',
      hovertemplate = '%{label}: %{value:,}<extra></extra>'
    ) %>%
    layout(
      title = list(text = tt, font = list(family = "Verdana", size = 14)),
      margin = list(l = 30, r = 30, t = 30),
      legend = list(
        orientation = "h",
        # show entries horizontally
        xanchor = "center",
        # use center of legend as anchor
        x = 0.5
      )
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
      title = list(text = tt, font = list(family = "Verdana", size = 14)),
      xaxis = list(title = 'Age Group'),
      yaxis = list(title = 'Number Screened',
                   tickformat = ','),
      bargap = 0.5,
      margin = list(l = 20, r = 20)
    )
  
}

bar_scale <- function(df, column, colors,tt) {

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
    layout( title = list(text = tt, font = list(family = "Verdana", size = 14)),
          barmode = 'stack',
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

sunburst_df <- function(df, value_column = NULL, add_root = FALSE){
  require(data.table)
  
  colNamesdf <- names(df)
  
  if(is.data.table(df)){
    DT <- copy(df)
  } else {
    DT <- data.table(df, stringsAsFactors = FALSE)
  }
  
  if(add_root){
    DT[, root := "Total"]  
  }
  
  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)
  DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]
  
  if(is.null(value_column) && add_root){
    setcolorder(DT, c("root", colNamesdf))
  } else if(!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c(setdiff(colNamesdf, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesdf, value_column), "values"))
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

create_sum_table <- function(df_sum, tt, head, N0, N1, N2) {

  df_sum %>%
    gt() %>%
    gt_plt_bar_stack(
      posneg,
      palette = c('#b78f62','#a1caf1'),
      labels = c("PCR Positive", "PCR Negative"),
      position = "stack",
      width = 80
    ) %>%
    tab_options(
      table.font.size = px(11L),
      table.border.top.style = "hidden",
      data_row.padding = 2
    ) %>%
    tab_header(
      title = tt
    ) %>%
    tab_spanner(label = "PCR Result",
                columns = stat_1:stat_2) %>%
    tab_style(
      style = list(cell_text(font = "Verdana", size = 14)),
      locations = list(cells_title(groups = "title"))
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(cells_column_spanners())
    ) %>%
    tab_style(
      style = cell_borders(
        sides = c("bottom"),
        color = "LightGray",
        weight = px(0.5),
        style = "solid"
      ),
      locations = cells_body(columns = everything(),
                             rows = everything())
    ) %>%
    cols_label(
      variable = gt::html(paste0("<b>", head, "</b>")),
      stat_0   = gt::html(paste0("<b>Overall</b>  (N = ", N0, ")<br>n (%)")),
      stat_1   = gt::html(paste0("<b>Positive</b> (N = ", N1, ")<br>n (%)")),
      stat_2   = gt::html(paste0("<b>Negative</b> (N = ", N2, ")<br>n (%)")),
      p.value  = gt::html("<b>p value</b>")
    ) %>%
    cols_align(align = "center",
               columns = c(starts_with("stat_"), p.value)) %>%
    cols_width(starts_with("stat_") ~ px(120)) %>%
    cols_width(p.value ~ px(100))

}
# Function to create the overall column of table 1b
create_t1bo <- function(df) {
  t <- tbl_summary(
    data = df,
    digits = list(all_categorical() ~ c(0, 1)),
    missing = 'no'
  ) %>%
    bold_labels() %>%
    modify_header(update = list(all_stat_cols() ~ "N = {n}"))
}
# Function to create the columns for each IgM, IgG-N, IgG-S
create_t1b <- function(df, var) {
  var <- enquo(var)
  t <- tbl_summary(
    data = df,
    by = !!var,
    digits = list(all_categorical() ~ c(0, 1)),
    missing = 'no'
  ) %>%
    bold_labels() %>%
    add_p() %>%
    bold_p() %>%
    modify_header(update = list(all_stat_cols() ~ "**{level}**<br>N = {n}"))
}
