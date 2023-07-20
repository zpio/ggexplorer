#' YoY Chart by Category
#'
#' Plot a YoY Chart by Category in ggplot2.
#'
#' @param data A data frame or tibble.
#' @param category A categorical variable that represents the y-axis.
#' @param cy_var integer. Current year.
#' @param py_var integer. Prior year.
#' @param label_var_p1 A categorical variable to label the bar chart.
#' @param label_var_p2 A categorical variable to label the YoY% plot.
#' @param label_color character. Label color.
#' @param label_size numeric. Label size.
#' @param title character. Title for the plot.
#' @param title_size numeric. Title size.
#' @param title_hjust numeric. Horizontal justification of the title.
#' @param palette_gradient numeric. Divergent color palette. Color palettes used in Tableau (ggthemes).
#' @param width_bar numeric. Width of the bars.
#' @param expandmult_p1 A numeric vector of length 2 indicating the expansion multiplier for the x-axis in bar chart.
#' @param expandmult_p2 A numeric vector of length 2 indicating the expansion multiplier for the x-axis in YoY% plot.
#' @param axis_text_size logical. Whether or not to return an interactive plot (plotly).
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
#'
#' @examples
#' data_sales <- data.frame(
#'   category = c("Furniture","Office Supplies","Technology"),
#'   sales = c(414288.7, 430037.2, 498095.0),
#'   sales_2021 = c(215387.3, 246097.2, 271730.8),
#'   sales_2020 = c(198901.4, 183940.0, 226364.2)
#' )
#'
#' yoy_barchart0(
#'   data = data_sales,
#'   category = category,
#'   cy_var = sales_2021,
#'   py_var = sales_2020
#' )
#'
#' @export
yoy_barchart0 <- function(data,
                          category,
                          cy_var,
                          py_var,
                          label_var_p1 = NULL,
                          label_var_p2 = NULL,
                          label_color = "grey30",
                          label_size = 3,
                          title = NULL,
                          title_size = 14,
                          title_hjust = 0.5,
                          palette_gradient = 'Green',
                          width_bar = 0.5,
                          expandmult_p1 = c(0, 0.4),
                          expandmult_p2 = c(0.5, 0.5),
                          axis_text_size = 9){

  category  <- rlang::enquo(category)
  value_cy  <- rlang::enquo(cy_var)
  value_py  <- rlang::enquo(py_var)
  label_var_p1  <- rlang::enquo(label_var_p1)
  label_var_p2  <- rlang::enquo(label_var_p2)

  data <- data %>%
    dplyr::mutate(
      max = ifelse(!!value_cy > !!value_py, !!value_cy, !!value_py),
      CY_abr = ifelse(
        abs(!!value_cy) >= 1e6, paste0(round(!!value_cy/1e6, 1), "M"),
        ifelse(abs(!!value_cy) >= 1e3, paste0(round(!!value_cy/1e3, 1), "K"), round(!!value_cy,0))
      ),
      PY_abr = ifelse(
        abs(!!value_py) >= 1e6, paste0(round(!!value_py/1e6, 1), "M"),
        ifelse(abs(!!value_py) >= 1e3, paste0(round(!!value_py/1e3, 1), "K"), round(!!value_py,0))
      ),
      label = paste0(CY_abr),
      YoY = !!value_cy - !!value_py,
      YoY_abr = ifelse(
        abs(YoY) >= 1e6, paste0(round(YoY/1e6, 1), "M"),
        ifelse(abs(YoY) >= 1e3, paste0(round(YoY/1e3, 1), "K"), round(YoY,0))
      ),
      YoY_pct = round((!!value_cy - !!value_py)/!!value_cy,3),
      label_YoY_pct = paste0(YoY_pct*100,"%"),
      label_YoY_YoYpct = paste0(YoY_abr, " (", YoY_pct*100,"%", ")"),
      color = ifelse(YoY < 0, "red3", "green3")
    )

  max_v <- data %>% dplyr::select(max) %>% max()

  length <- data %>% dplyr::select(!!category) %>% dplyr::pull() %>% length()

  p1 <-
    ggplot2::ggplot(data, ggplot2::aes(stats::reorder(!!category, !!value_cy), !!value_cy)) +
    ggplot2::geom_bar(stat = "identity", width = width_bar, ggplot2::aes(fill = !!value_cy)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = !!value_py, ymax = !!value_py), linewidth = 1, color = "grey20")


  if (!rlang::quo_is_null(label_var_p1)) {
    label_var_p1  <- rlang::enquo(label_var_p1)

    p1 <- p1 +
      ggplot2::geom_text(
        ggplot2::aes(label = !!label_var_p1, x = !!category, y = max),
        colour = label_color,
        size = label_size,
        hjust = -0.1
      )

  }else{

    p1 <- p1 +
      ggplot2::geom_text(
        ggplot2::aes(label = label, x = !!category, y = max),
        colour = label_color,
        size = label_size,
        hjust = -0.1
      )
  }

  if(is.null(title)){
    cat <- data %>% dplyr::select(!!category) %>% names()
    title <- paste(cat, "- Year over Year")
  }else{
    title
  }

  p1 <- p1 +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      breaks = seq(0, max_v, length.out = 3),
      expand = ggplot2::expansion(mult = expandmult_p1)
    ) +
    ggplot2::labs(title = NULL) +
    ggthemes::scale_fill_gradient_tableau(palette = palette_gradient, guide = "none") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face="bold", margin = ggplot2::margin(b=10)),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(5.5, 0, 5.5, 5.5), "pt"),
      axis.text.y = ggplot2::element_text(size = axis_text_size),
      axis.text.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::geom_vline(
      xintercept = seq(0.5, length, by = 1),
      color = "gray", linewidth = 0.5, alpha = 0.5
    )

  p2 <-
    ggplot2::ggplot(data, ggplot2::aes(x = stats::reorder(!!category, !!value_cy), y = YoY_pct))+
    ggplot2::geom_hline(
      yintercept = 0, color = "gray", linewidth = 0.8, alpha = 0.5
    ) +
    ggplot2::geom_point(size = 3, color = "grey20") +
    ggplot2::geom_segment(
      ggplot2::aes(x = !!category, xend = !!category, y = 0, yend = YoY_pct, color = color),
      linewidth = 1.15
    ) +
    ggplot2::scale_color_identity(guide = "none") +
    ggplot2::labs(title = NULL) +
    ggplot2::coord_flip()  +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = expandmult_p2),
      labels = scales::percent_format()
    )

  if (!rlang::quo_is_null(label_var_p2)) {
    label_var_p2  <- rlang::enquo(label_var_p2)

    p2 <- p2 +
      ggplot2::geom_text(
        ggplot2::aes(label = label_var_p2, hjust = ifelse(YoY < 0, 1.35, -0.35)),
        size = label_size, color = label_color
      )

  }else{

    p2 <- p2 +
      ggplot2::geom_text(
        ggplot2::aes(label = label_YoY_pct, hjust = ifelse(YoY < 0, 1.35, -0.35)),
        size = label_size, color = label_color
      )
  }

  p2 <- p2 +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b=10)),
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::geom_vline(
      xintercept = seq(0.5, length, by = 1),
      color = "gray", linewidth = 0.5, alpha = 0.5
    )

  pt <- ggpubr::ggarrange(p1, p2, ncol = 2)

  plot <- ggpubr::annotate_figure(
    pt,
    top = ggpubr::text_grob(
      label = title, color = "black", face = "bold", size = title_size, hjust = title_hjust
    )
  )

  return(plot)

}
