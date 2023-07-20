#' Year-over-Year chart
#'
#' Year-over-Year chart in ggplot2.
#'
#' @param data A data frame or tibble.
#' @param date A date or date time variable.
#' @param value A numeric variable.
#' @param prior_year integer. Prior year.
#' @param current_year integer. Current year.
#' @param summarise_by character. Time unit to summarise the data.
#' @param color_prior character. Line color of the prior year.
#' @param color_current character. Line color of the current year.
#' @param linewidth_prior numeric. Line width of the prior year.
#' @param linewidth_current numeric. Line width for the current year.
#' @param label_size numeric. Label font size for the current year.
#' @param expand_mult_x A numeric vector of length 2 indicating the expansion of the x-axis in the line chart and bar chart.
#' @param title character. Title for the plot.
#' @param y_lab_p1 character. Y-axis label for the line chart.
#' @param y_lab_p2 character. Y-axis label for the bar chart.
#' @param color_positive character. Color for positive percentage changes.
#' @param color_negative character. Color for negative percentage changes.
#' @param label_size_p2 numeric. Label font size for the bar chart.
#' @param label_color_p2 character. Color labels on the bar chart.
#' @param expand_mult_y_p2 A numeric vector of length 2 indicating the amount to expand the y-axis limits for the bar chart.
#' @param pct logical. Whether or not to format the values as percentages.
#'
#' @importFrom readr read_csv
#'
#' @return A ggplot object
#'
#' @examples
#' library(readr)
#' library(dplyr)
#' data <- read_csv(
#'   file = "https://raw.githubusercontent.com/zpio/datasets/main/sample_superstore.csv"
#' )
#'
#' yoy_chart(
#'   data = data,
#'   date = order_date,
#'   value = sales,
#'   prior_year = 2020,
#'   current_year = 2021
#' )
#' @export
yoy_chart <- function(data, date, value,
                      prior_year,
                      current_year,
                      summarise_by = "month",
                      color_prior = "gray",
                      color_current = "#4E79A7",
                      linewidth_prior = 1,
                      linewidth_current = 1,
                      label_size = 3.5,
                      expand_mult_x = c(0.04, 0.12),
                      title = NULL,
                      y_lab_p1 = NULL,
                      y_lab_p2 = NULL,
                      color_positive = "green3",
                      color_negative = "red3",
                      label_size_p2 = 3,
                      label_color_p2 = "gray10",
                      expand_mult_y_p2 = c(0.25, 0.25),
                      pct = FALSE){

  date  <- rlang::enquo(date)
  value  <- rlang::enquo(value)

  data_years <- data %>%
    dplyr::filter(lubridate::year(!!date) %in% c(current_year, prior_year)) %>%
    timetk::summarise_by_time(
      !!date, .by = summarise_by,
      value = sum(!!value)
    )

  data_cy_py <- data_years %>%
    dplyr::mutate(p_date = !!date - lubridate::years(1)) %>%
    dplyr::left_join(data_years, by = c("p_date" = rlang::quo_name(date))) %>%
    dplyr::rename(
      CY = value.x,
      PY = value.y
    ) %>%
    dplyr::select(!!date, CY, PY) %>%
    dplyr::filter(lubridate::year(!!date) == current_year) %>%
    dplyr::mutate(
      YoY = round((CY - PY)/PY,3),
      YoY_pct = paste0(YoY*100, "%"),
      YoY_color = ifelse(YoY < 0, color_negative, color_positive),
      max_cy = ifelse(!!date == max(!!date), CY, NA)
    )

  point_date <- data_cy_py %>% dplyr::filter(max_cy != 'NA')

  breaks <- data_cy_py %>% dplyr::select(!!date) %>% dplyr::pull()

  if(pct){
    point_date <- point_date %>%
      dplyr::mutate(
        CY_abr = paste0(round(CY*100,2), "%")
      )
  }else{
    point_date <- point_date %>%
      dplyr::mutate(
        CY_abr = ifelse(
          abs(CY) >= 1e6, paste0(round(CY/1e6, 1), "M"),
          ifelse(abs(CY) >= 1e3, paste0(round(CY/1e3, 1), "K"), round(CY,1))
        )
      )
  }


  p1 <-
    data_cy_py %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(!!date, PY, color = "Prior"),
      linewidth = linewidth_prior
    ) +
    ggplot2::geom_line(
      ggplot2::aes(!!date, CY, color = "Current"),
      linewidth = linewidth_current
    ) +
    ggplot2::geom_text(
      data = point_date,
      ggplot2::aes(!!date, max_cy, label = CY_abr),
      fontface = "bold", size = label_size, hjust = -0.2, vjust = 0.5
    ) +
    ggplot2::scale_x_date(
      date_labels = "%b",
      expand = ggplot2::expansion(mult = expand_mult_x),
      breaks = breaks
    ) +
    ggplot2::scale_color_manual(
      name = "", values = c("Prior" = color_prior, "Current" = color_current),
      breaks = c("Prior", "Current"),
      labels = c(prior_year, current_year)
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = 1.5)))

  if(pct){
    p1 <- p1 +
      ggplot2::scale_y_continuous(labels = scales::percent_format())
  }else{
    p1 <- p1 +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      )
  }

  if(is.null(title)){
    title <- "Monthy Metric (YoY)"
  }

  if(is.null(y_lab_p1)){y_lab_p1 <- "Value"}

  p1 <- p1 +
    ggplot2::labs(title = title, y = y_lab_p1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face="bold", hjust = 0),
      legend.text = ggplot2::element_text(size = 10),
      legend.position = "top",
      legend.margin = ggplot2::margin(t = 0, unit='cm'),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 7),
      axis.ticks.y = ggplot2::element_blank()
    )

  if(is.null(y_lab_p2)){y_lab_p2 <- "YoY"}

  p2 <-
    data_cy_py %>%
    ggplot2::ggplot(ggplot2::aes(!!date, YoY)) +
    ggplot2::geom_hline(
      yintercept = 0, color = "gray", linewidth = 0.8, alpha = 0.7
    ) +
    ggplot2::geom_point(size = 3, color = "grey20") +
    ggplot2::geom_segment(
      ggplot2::aes(x = !!date, xend = !!date, y = 0, yend = YoY, color = YoY_color),
      linewidth = 1.15
    ) +
    ggplot2::geom_text(
      ggplot2::aes(!!date, YoY, label = YoY_pct, vjust = ifelse(YoY < 0, 1.90, -0.90)),
      size = label_size_p2, color = label_color_p2
    ) +
    ggplot2::scale_color_identity(guide = "none") +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(),
      expand = ggplot2::expansion(mult = expand_mult_y_p2)
    ) +
    ggplot2::scale_x_date(
      breaks = breaks,
      date_labels = "%b",
      expand = ggplot2::expansion(mult = expand_mult_x),
    ) +
    ggplot2::labs(x = NULL, y = y_lab_p2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 7),
      axis.text.y = ggplot2::element_text(size = 7, color = "white"),
      axis.ticks.y = ggplot2::element_blank()
    )

  pt <- ggpubr::ggarrange(p1,p2, ncol = 1, heights = c(1, 1))

  return(pt)
}
