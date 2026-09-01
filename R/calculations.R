library(dplyr)

#' Calculate high-level performance indicators
calculate_kpis <- function(df, selected_year = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(total_income = 0, total_expense = 0, total_discount = 0, net_profit = 0))
  }
  
  filtered_df <- df
  if (!is.null(selected_year) && selected_year != "All" && "Year" %in% names(filtered_df)) {
    filtered_df <- filtered_df %>% dplyr::filter(as.character(Year) == as.character(selected_year))
  }
  
  list(
    total_income   = sum(filtered_df$Income, na.rm = TRUE),
    total_expense  = sum(filtered_df$Expense, na.rm = TRUE),
    total_discount = sum(filtered_df$Discount, na.rm = TRUE),
    net_profit     = sum(filtered_df$Income, na.rm = TRUE) - sum(filtered_df$Expense, na.rm = TRUE)
  )
}

#' Aggregate metrics dynamically based on user selections
aggregate_custom_metric <- function(df, metric_col = "Income", group_col = "Category", selected_year = NULL) {
  if (is.null(df) || nrow(df) == 0 || !metric_col %in% names(df) || !group_col %in% names(df)) return(NULL)
  
  filtered_df <- df
  if (!is.null(selected_year) && selected_year != "All" && "Year" %in% names(filtered_df)) {
    filtered_df <- filtered_df %>% dplyr::filter(as.character(Year) == as.character(selected_year))
  }
  
  filtered_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("Month", group_col)))) %>%
    dplyr::summarise(
      Metric_Value = sum(.data[[metric_col]], na.rm = TRUE),
      .groups = "drop"
    )
}