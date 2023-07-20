#' Compute year-over-year growth by category
#'
#' This function calculates the year-over-year (YoY) growth of a given numerical variable, grouped by a category variable. It filters the data by the specified date variable, selecting only the years specified by current_year and previous_year.
#'
#' @param data A data-frame or tibble.
#' @param date A date variable.
#' @param category A categorical variable.
#' @param value A numerical variable.
#' @param current_year numeric. Current year.
#' @param previous_year numeric. Prior year.
#'
#' @return A tibble
#'
#' @importFrom readr read_csv
#'
#' @examples
#' library(readr)
#' library(dplyr)
#' data <- read_csv(
#'   file = "https://raw.githubusercontent.com/zpio/datasets/main/sample_superstore.csv"
#' )
#'
#' data_yoy <-
#'   yoy_category(
#'     data = data,
#'     date = order_date,
#'     category = category,
#'     value = sales,
#'     current_year = 2021,
#'     previous_year = 2020
#'   )
#
# data_yoy
#'
#' @export
yoy_category <- function(data,
                         date,
                         category,
                         value,
                         current_year,
                         previous_year){

  date <- rlang::enquo(date)
  category <- rlang::enquo(category)
  value <- rlang::enquo(value)

  data_yoy <- data %>%
    dplyr::filter(lubridate::year(!!date) %in% c(current_year, previous_year)) %>%
    dplyr::select(!!date, !!category, !!value) %>%
    dplyr::mutate(
      CY = ifelse(lubridate::year(!!date) == current_year & !!date <= max(!!date), !!value, 0),
      PY = ifelse(lubridate::year(!!date) == previous_year & !!date <= max(!!date) - lubridate::years(1), !!value, 0)
    ) %>%
    dplyr::group_by(!!category) %>%
    dplyr::summarise(
      Amount = sum(!!value),
      CY = sum(CY),
      PY = sum(PY)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      CY_label = ifelse(
        abs(CY) >= 1e6, paste0(round(CY/1e6, 1), "M"),
        ifelse(abs(CY) >= 1e3, paste0(round(CY/1e3, 1), "K"), round(CY,2))
      ),
      PY_label = ifelse(
        abs(PY) >= 1e6, paste0(round(PY/1e6, 1), "M"),
        ifelse(abs(PY) >= 1e3, paste0(round(PY/1e3, 1), "K"), round(PY,2))
      ),
      CY_PY_label = paste0(CY_label, " (",PY_label,")"),
      YoY_growth = round((CY - PY)/CY,4),
      YoY_pct_label = paste0(YoY_growth*100,"%")
    ) %>%
    dplyr::rename(
      "{{value}}" := Amount,
      "CY_{{current_year}}" := CY,
      "PY_{{previous_year}}" := PY
    )

  return(data_yoy)

}
