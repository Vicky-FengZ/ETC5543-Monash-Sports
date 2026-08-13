# -------------------------------------------------------------------------
# Shared UI helpers
# -------------------------------------------------------------------------

placeholder_chart <- function(
  text = "Chart placeholder",
  subtitle = "Real data will be connected in the next stage"
) {

  div(
    class = "chart-placeholder",
    bs_icon(
      "bar-chart-line",
      size = "32px"
    ),
    div(
      class = "placeholder-title",
      text
    ),
    div(
      class = "placeholder-subtitle",
      subtitle
    )
  )
}


prototype_note <- function(
  text = "Prototype values are placeholders for layout demonstration only"
) {
  span(
    class = "prototype-note",
    text
  )
}


chart_subtitle <- function(...) {
  div(
    class = "chart-header-subtitle",
    ...
  )
}


make_value_box_row <- function(boxes, width = 1 / 3) {

  layout_column_wrap(
    width = width,
    lapply(
      boxes,
      function(x) {
        value_box(
          title = x$title,
          value = x$value,
          x$subtitle,
          showcase = bs_icon(x$icon)
        )
      }
    )
  )
}
