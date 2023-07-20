#' YoY Chart by Category with sparklines
#'
#' Plot a YoY chart by Category with sparklines in ggplot2.
#'
#' @param data A data-frame or tibble.
#' @param date A date or date time variable.
#' @param category A categorical variable.
#' @param value  A numerical variable.
#' @param current_year integer. Current year.
#' @param previous_year integer. Prior year.
#' @param summarise_by character. The time unit to summarise.
#' @param top_n integer. Top n levels.
#' @param show_label logical. Whether or not to include label.
#' @param label_color numeric. Label color.
#' @param label_size numeric. Label size.
#' @param title character. Title for the plot.
#' @param title_size numeric. The size of the title.
#' @param title_hjust numeric. Horizontal justification of the title.
#' @param palette_gradient numeric. Divergent color palette. Color palettes used in Tableau (ggthemes).
#' @param bar_width numeric. Bar Width.
#' @param linepy_color character. Color of the prior year line in the bar chart.
#' @param linepy_width numeric. Size of the prior year line in the bar chart.
#' @param expand_mult  A numeric vector of length 2 indicating the expansion multiplier for the y-axis in bar chart.
#' @param axistext_size numeric. Size of the y-axis text in bar chart.
#' @param ts_linecolor character. Color of the lines in line chart.
#' @param ts_linewidth numeric. Width of the lines in line chart.
#' @param ts_pointsize numeric. Size of the points in line chart.
#' @param interactive logical. Whether or not to return an interactive plot (plotly).
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
#'
#'
#' @examples
#'
#' data <- readr::read_csv(
#'   file = "https://raw.githubusercontent.com/zpio/datasets/main/sample_superstore.csv"
#' )
#'
#' yoy_barchart(
#'   data = data,
#'   date = order_date,
#'   category = state,
#'   value = sales,
#'   current_year = 2021,
#'   previous_year = 2020,
#'   title = "Sales by Category 2021 (YoY %)"
#' )
#'
#' @export
yoy_barchart <- function(data,
                         date,
                         category,
                         value,
                         current_year,
                         previous_year,
                         summarise_by = "month",
                         top_n = 10,
                         show_label = TRUE,
                         label_color = "grey30",
                         label_size = 3,
                         title = NULL,
                         title_size = 14,
                         title_hjust = 0.5,
                         palette_gradient = 'Green',
                         bar_width = 0.5,
                         linepy_color = "grey20",
                         linepy_width = 1,
                         expand_mult = NULL,
                         axistext_size = 9,
                         ts_linecolor = "steelblue",
                         ts_linewidth = 0.5,
                         ts_pointsize = 0.85,
                         interactive = FALSE){

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
    dplyr::arrange(dplyr::desc(CY)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(
      CY_label = ifelse(
        abs(CY) >= 1e6, paste0(round(CY/1e6, 1), "M"),
        ifelse(abs(CY) >= 1e3, paste0(round(CY/1e3, 1), "K"), round(CY,2))
      ),
      PY_label = ifelse(
        abs(PY) >= 1e6, paste0(round(PY/1e6, 1), "M"),
        ifelse(abs(PY) >= 1e3, paste0(round(PY/1e3, 1), "K"), round(PY,2))
      ),
      YoY_growth = round((CY - PY)/CY,3),
      label_YoY_pct = paste0(YoY_growth*100,"%"),
      label = paste0(CY_label, " (", label_YoY_pct,")"),
      max = ifelse(CY > PY, CY, PY),
      y = max + 0.35*max(max)
    ) %>%
    dplyr::mutate(
      !!category := stats::reorder(!!category, CY)
    )

  max_v <- data_yoy %>% dplyr::select(max) %>% max()

  length <- data_yoy %>% dplyr::select(!!category) %>% dplyr::pull() %>% length()

  order <- data_yoy %>% dplyr::select(!!category) %>% dplyr::pull()

  if(is.null(title)){
    val <- rlang::quo_name(value)
    cat <- rlang::quo_name(category)
    title <- paste(val, "by", cat, "(YoY%)")
  }

  if(is.null(expand_mult)){
    if(show_label){
      if(interactive){
        expand_mult <- c(0.05, 0.2)
      }else{
        expand_mult <- c(0.05, 0.5)
      }

    }else{
      expand_mult <- c(0.05, 0.1)
    }
  }


  if(interactive){
    p1 <-
      ggplot2::ggplot(data_yoy) +
      ggplot2::geom_bar(ggplot2::aes(x =!!category, y = CY, fill = CY), stat = "identity", width = bar_width) +
      ggplot2::geom_bar(ggplot2::aes(x = !!category, y = PY), stat = "identity", fill = linepy_color, width = 0.02)+
      ggplot2::geom_point(ggplot2::aes(x = !!category, y = PY), color = linepy_color)

    if(show_label){
      p1 <- p1 +
        ggplot2::geom_text(ggplot2::aes(label = label, x = !!category, y = y),
                           colour = label_color, size = label_size, hjust = -0.1)
    }else{
      p1
    }


  }else{
    p1 <-
      ggplot2::ggplot(data_yoy, ggplot2::aes(x =!!category, y = CY, fill = CY)) +
      ggplot2::geom_bar(stat = "identity", width = bar_width) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = PY, ymax = PY), linewidth = linepy_width, color = linepy_color)

    if(show_label){
      p1 <- p1 +
        ggplot2::geom_text(ggplot2::aes(label = label, x = !!category, y = max),
                           colour = label_color, size = label_size, hjust = -0.1)
    }

  }


  p1 <- p1 +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      breaks = seq(0, max_v, length.out = 3),
      expand = ggplot2::expansion(mult = expand_mult)
    ) +
    ggplot2::labs(title = NULL) +
    ggthemes::scale_fill_gradient_tableau(palette = palette_gradient, guide = "none") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = axistext_size),
      axis.text.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::geom_vline(
      xintercept = seq(0.5, length, by = 1),
      color = "gray", linewidth = 0.2, alpha = 0.5
    )


  # lines

  data_p2 <- data %>%
    dplyr::filter(lubridate::year(!!date)==current_year) %>%
    dplyr::select(!!date, !!category, !!value) %>%
    dplyr::group_by(!!date, !!category) %>%
    timetk::summarise_by_time(
      .date_var = !!date,
      .by = summarise_by,
      value = sum(!!value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!category %in% order) %>%
    dplyr::mutate(
      sub_category = factor(!!category, levels = order)
    )


  if(interactive){
    if(ts_pointsize == 0.85){
      ts_pointsize <-  0.2
    }

    if(ts_linewidth == 0.5){
      ts_linewidth <-  0.2
    }
  }


  p2 <- ggplot2::ggplot(data_p2, ggplot2::aes(x = !!date, y = value)) +
    ggplot2::geom_line(color = ts_linecolor, linewidth = ts_linewidth) +
    ggplot2::geom_point(color = ts_linecolor, size = ts_pointsize)+
    ggplot2::facet_grid(ggplot2::vars(sub_category), scales = "free_y") +
    ggplot2::labs(x = " ") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.y = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank()
    )


  if(interactive){
    plotly1 <-
      plotly::ggplotly(p1, tooltip = c("x", "y")) %>%
      plotly::layout(
        hoverlabel = list(
          font = list(
            color = "white",
            family = "Consolas"
          ),
          bordercolor = "transparent"
        )
      )

    #plotly1$x$data[[13]]$hoverinfo <- "none"

    plotly2 <-
      plotly::ggplotly(p2, tooltip = c("x", "y")) %>%
      plotly::layout(
        hoverlabel = list(
          font = list(
            color = "white",
            family = "Consolas"
          ),
          bordercolor = "transparent"
        )
      )


    fig <- plotly::subplot(plotly1, plotly2, widths = c(0.6, 0.4)) %>%
      plotly::layout(
        margin = list(t = 40),
        title = list(text =  paste('<b>', title, '<b>'), font = list(size = title_size))
      )

    return(fig)


  }else{
    pt <- ggpubr::ggarrange(p1, p2, ncol = 2, widths = c(1, 0.60))

    plot <- ggpubr::annotate_figure(
      pt,
      top = ggpubr::text_grob(label = title, color = "grey20", face = "bold",
                              size = title_size, hjust = title_hjust)
    )
  }

  return(plot)
}
