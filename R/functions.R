#' Mean and count by group
#' @param data A data frame.
#' @param group Column to group by.
#' @param value Numeric column to average.
#' @export
group_summary <- function(data, group, value) {
  dplyr::summarise(dplyr::group_by(data, {{ group }}),
                   mean = mean({{ value }}, na.rm = TRUE),
                   n = dplyr::n())
}

#' Quick box plot by group
#' @param data A data frame.
#' @param group Column for the x axis.
#' @param value Numeric column for the y axis.
#' @export
quick_box <- function(data, group, value) {
  ggplot2::ggplot(data, ggplot2::aes(x = {{ group }}, y = {{ value }})) +
    ggplot2::geom_boxplot()
}