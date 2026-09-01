library(plotly)
library(ggplot2)

#' Render interactive bar graphs for metric comparisons
render_versatile_chart <- function(df, x_col = "Month", y_col = "Income", fill_col = "Category") {
  if (is.null(df) || nrow(df) == 0) return(plotly_empty())
  
  p <- ggplot(df, aes_string(x = x_col, y = "Metric_Value", fill = fill_col, group = fill_col)) +
    geom_col(position = "dodge") +
    theme_minimal() +
    labs(
      x = x_col, 
      y = y_col, 
      fill = fill_col, 
      title = paste(y_col, "Breakdown by", fill_col)
    ) +
    scale_y_continuous(labels = scales::dollar)
  
  ggplotly(p)
}