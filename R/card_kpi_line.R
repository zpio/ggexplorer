#' Kpi card with line chart
#'
#' Kpi card with line chart in ggplot2
#'
#' @param data A data-frame or tibble.
#' @param date A date or date time column.
#' @param value A numeric column.
#' @param prior_year integer. Prior year.
#' @param current_year integer. Current year.
#' @param summarise_by character. Time unit to summarise.
#' @param line_color character. Line Color.
#' @param line_width numeric. Line_width.
#' @param area_color character. The color for the area chart (Prior year).
#' @param area_alpha numeric. Opacity for the area chart (Prior year).
#' @param label_size numeric. Label size.
#' @param show_label logical. Whether or not to include label.
#' @param expand_mult_x A numeric vector of length 2 containing the expansion multiplier.
#' @param title character. Title for the plot.
#' @param title_size numeric. The size of the title.
#' @param title_hjust numeric. The hjust of the title.
#' @param legend_position character. Legend position. Options include "right", "left", "top", "bottom", "none".
#' @param pct logical. Whether or not to format the values as percentages.
#' @param value_text character. The text for the current year.
#' @param value_size numeric. The text size for `value_text`.
#' @param value_color character. The text color for `value_text`.
#' @param value_coord numeric. A vector of length 2 containing the coordinates (x,y) for `value_text`.
#' @param yoy_text character. The text for YoY.
#' @param yoy_size numeric. The size for for `yoy_text`.
#' @param yoy_coord numeric. A vector of length 2 containing the coordinates (x,y) for `yoy_text`.
#' @param yoy_color character. The text color for `yoy_text`.
#' @param rect_coord numeric. A vector of length 4 containing the coordinates of the rectangle to be drawn around the `yoy_text`. The values are xmin, xmax, ymin, ymax.
#' @param rect_color character. The color of the rectangle to be drawn around the `yoy_text`.
#' @param rect_alpha numeric. Opacity for the rectangle to be drawn around the `yoy_text`.
#' @param text_top character. The text to display at the top of the `value_text`.
#' @param text_top_size numeric. The size of the text to display at the top of the `value_text`.
#' @param text_top_coord numeric. A vector of length 2 containing the coordinates (x,y) to display at the top of the `value_text`.
#' @param text_top_color character. The color of text to display at the top of the `value_text`.
#' @param text_bottom character. The text to display at the bottom of the `yoy_text`.
#' @param text_bottom_size numeric. The size of the text dto display at the bottom of the `yoy_text`.
#' @param text_bottom_coord numeric. A vector of length 2 containing the coordinates (x,y) the text to display at the bottom of the `yoy_text`.
#' @param text_bottom_color character. The color of text to display at the bottom of the `yoy_text`.
#' @param widths numeric. A vector of length 2 containing the relative widths of the line chart and kpi.
#' @param show_axis_text logical. Whether or not to show x-axis text.
#' @param interactive logical. Whether or not to return an interactive plot (plotly).
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
#'
#'
#' @examples
#' library(dplyr)
#'
#' data <- readr::read_csv(
#'   file = "https://raw.githubusercontent.com/zpio/datasets/main/sample_superstore.csv"
#' )
#'
#' data %>%
#'   card_kpi_line(
#'     date = order_date,
#'     value = sales,
#'     prior_year = 2020,
#'     current_year = 2021
#'   )
#'
#' data %>%
#'   filter(sub_category == "Machines") %>%
#'   card_kpi_line(
#'     date = order_date,
#'     value = sales,
#'     prior_year = 2020,
#'     current_year = 2021,
#'     title = "Subcategory: Machines",
#'     title_size = 10,
#'     line_color = "cyan3",
#'     area_color = "cyan",
#'     area_alpha = 0.2
#'   )
#'
#'
#' @export
card_kpi_line <- function(data,
                          date,
                          value,
                          prior_year,
                          current_year,
                          summarise_by = "month",
                          line_color = "#4E79A7",
                          line_width = 1.2,
                          area_color = "gray",
                          area_alpha = 0.4,
                          label_size = 3.5,
                          show_label = TRUE,
                          expand_mult_x = c(0.04, 0.12),
                          title = NULL,
                          title_size = NULL,
                          title_hjust = 0.5,
                          legend_position = "top",
                          pct = FALSE,
                          value_text = NULL,
                          value_size = 10,
                          value_color = "gray20",
                          value_coord = c(1, 1.80),
                          yoy_text = NULL,
                          yoy_size = 5,
                          yoy_coord = c(1, 1.71),
                          yoy_color = NULL,
                          rect_coord = c(0.978, 1.021, 1.67, 1.74),
                          rect_color = NULL,
                          rect_alpha = 0.2,
                          text_top = NULL,
                          text_top_size = 4,
                          text_top_coord = c(1, 1.87),
                          text_top_color = "gray20",
                          text_bottom = NULL,
                          text_bottom_size = 4,
                          text_bottom_coord = c(1, 1.64),
                          text_bottom_color = "gray20",
                          widths = c(1, 1.50),
                          show_axis_text = TRUE,
                          interactive = FALSE){

  date  <- rlang::enquo(date)
  value  <- rlang::enquo(value)

  data_years <- data %>%
    dplyr::filter(lubridate::year(!!date) %in% c(current_year, prior_year)) %>%
    timetk::summarise_by_time(
      !!date, .by = summarise_by,
      value = sum(!!value, na.rm = T)
    )

  data_cy_py <- data_years %>%
    dplyr::mutate(p_date = !!date - lubridate::years(1)) %>%
    dplyr::left_join(data_years, by = c("p_date" = rlang::quo_name(date))) %>%
    dplyr::rename(
      CY = value.x,
      PY = value.y
    ) %>%
    dplyr::select(!!date, CY, PY) %>%
    dplyr::filter(lubridate::year(!!date) == 2021) %>%
    dplyr::mutate(max_cy = ifelse(!!date == max(!!date), CY, NA))


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

  total_cy <- data_cy_py %>% dplyr::select(CY) %>% dplyr::pull() %>% sum(na.rm = T)

  if(is.null(value_text)){
    value_text <- ifelse(
      abs(total_cy) >= 1e6, paste0(round(total_cy/1e6, 1), "M"),
      ifelse(abs(total_cy) >= 1e3, paste0(round(total_cy/1e3, 1), "K"), round(CY,1))
    )
  }

  total_py <- data_cy_py %>% dplyr::select(PY) %>% dplyr::pull() %>% sum(na.rm = T)

  yoy <- (total_cy - total_py)/total_py

  if(is.null(yoy_text)){yoy_text <- paste0(round(yoy,3)*100, "%")}

  if(is.null(yoy_color)){yoy_color <- ifelse(yoy>=0,"green3","red3")}

  if(is.null(rect_color)){rect_color <- ifelse(yoy>=0,"green3","red3")}

  if(is.null(title)){title <- paste(rlang::as_name(value), current_year)}

  if(is.null(text_top)){text_top <- "Current Year Total"}

  if(is.null(text_bottom)){text_bottom <-  "vs. Prior Year"}

  if(is.null(title_size)){title_size <-  14}

  if(interactive){
    rect_coord[1] <- 0.977
    rect_coord[2] <- 1.022
    rect_coord[3] <- 1.68
    rect_coord[4] <- 1.75
  }

  df <- data.frame(
    x = c(0, 0, 2, 2),
    y = c(0, 1, 0, 2)
  )

  # kpi card
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::annotate("text", x = 1, y = 2, label = "") +
    ggplot2::annotate("text", x = 0.950, y = 1.71, label = "") +
    ggplot2::annotate("text", x = 1.05, y = 1.71, label = "") +
    ggplot2::annotate("text", x = 1, y = 1.5, label = "") +

    ggplot2::annotate("text", x = text_top_coord[1], y = text_top_coord[2], size = text_top_size,
                      label = text_top, color = text_top_color) +

    ggplot2::annotate("text", x = value_coord[1], y = value_coord[2], fontface = "bold", size = value_size,
                      label = value_text, color = value_color) +

    ggplot2::annotate("rect", xmin = rect_coord[1], xmax = rect_coord[2], ymin = rect_coord[3], ymax = rect_coord[4],
                      alpha = rect_alpha, fill = rect_color) +

    ggplot2::annotate("text", x = yoy_coord[1], y = yoy_coord[2], size = yoy_size, fontface = "bold",
                      label = yoy_text, color = yoy_color) +

    ggplot2::annotate("text", x = text_bottom_coord[1], y = text_bottom_coord[2], size = text_bottom_size,
                      label = text_bottom, color = text_bottom_color) +

    ggplot2::labs(y = NULL, x = NULL, title = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Line Chart
  p2 <-
    data_cy_py %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(!!date, PY, color = "Prior"),
      linewidth = 0.3, alpha = area_alpha
    ) +
    ggplot2::geom_area(
      ggplot2::aes(!!date, PY),
      fill = area_color, alpha = area_alpha
    ) +
    ggplot2::geom_line(
      ggplot2::aes(!!date, CY, color = "Current"),
      linewidth = line_width
    ) +
    ggplot2::scale_x_date(
      date_labels = "%b",
      expand = ggplot2::expansion(mult = expand_mult_x),
      breaks = breaks
    ) +
    ggplot2::scale_color_manual(
      name = "", values = c("Prior" = ggplot2::alpha(area_color, 0.5), "Current" = line_color)
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = 1.5))) +
    ggplot2::labs(y = NULL, x = NULL, title = NULL)

  if(pct){
    p2 <- p2+
      ggplot2::scale_y_continuous(labels = scales::percent_format())
  }else{
    p2 <- p2+
      ggplot2::scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      )
  }

  p2 <- p2+
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = 10),
      legend.position = legend_position,
      legend.margin = ggplot2::margin(t = 0, unit='cm'),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 7)
    )

  if(show_label){
    p2 <- p2 +
      ggplot2::geom_text(
        data = point_date,
        ggplot2::aes(!!date, max_cy, label = CY_abr),
        fontface = "bold", size = label_size, hjust = 0.5, vjust = 1.2
      )
  }


  if(show_axis_text){
    p2
  }else{
    p2 <- p2 +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank()
      )
  }

  pt <- ggpubr::ggarrange(p1, p2, nrow = 1, widths = widths)

  plot <- ggpubr::annotate_figure(
    pt,
    top = ggpubr::text_grob(
      label = title, color = "grey20", face = "bold", size = title_size, hjust = title_hjust
    )
  )

  if(interactive){
    plotly1 <- plotly::ggplotly(p1, tooltip = NULL)
    plotly2 <- plotly::ggplotly(p2, tooltip = c("x", "y"))

    fig <- plotly::subplot(plotly1, plotly2, widths = c(0.4, 0.6)) %>%
      plotly::layout(
        margin = list(t = 40),
        title = list(text =  paste('<b>', title, '<b>'), font = list(size = title_size+2))
      )
    return(fig)
  }else{
    return(plot)
  }



}





