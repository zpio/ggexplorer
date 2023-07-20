#' Bubble Chart
#'
#' Plot Bubble Chart in ggplot2
#'
#' @param data A data.frame or tibble.
#' @param x A numeric variable.
#' @param y A numeric variable.
#' @param size_var A numeric variable indicating the size.
#' @param fill_var A categorical variable to group and fill.
#' @param facet_var A categorical variable used to create facet panels.
#' @param scale_size A numeric vector with the minimum and maximum size.
#' @param alpha numeric. Opacity. Range: (0, 1).
#' @param stroke numeric. Stroke width (outline).
#' @param trans_x character. X-axis transformation.
#' @param trans_y character. Y-axis transformation.
#' @param title character. Title for the plot.
#' @param x_lab character. X-axis label for the plot.
#' @param y_lab character. Y-axis label for the plot.
#' @param scales character. Control facet x & y-axis ranges. Options include "fixed", "free", "free_y", "free_x"
#' @param ncol integer. Number of facet columns.
#' @param legend_position character. Legend position. Options include "right", "left", "top", "bottom", "none".
#' @param interactive logical. Whether or not to return an interactive plot (plotly).
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @examples
#' library(dplyr)
#' library(gapminder)
#'
#' data <- gapminder %>% filter(year=="2002") %>%
#'   dplyr::select(-year)
#'
#' data_bubble <- data %>%
#'   mutate(pop=pop/1000000) %>%
#'   arrange(desc(pop)) %>%
#'   mutate(country = factor(country, country))
#'
#' bubble_chart(
#'   data = data_bubble,
#'   x = gdpPercap,
#'   y = lifeExp,
#'   size_var = pop,
#'   fill_var = continent,
#'   title = "GDPpercap vs Life Exp",
#'   x_lab = "Gdp Per Cap",
#'   y_lab = "Life Exp",
#'   trans_x = "log10",
#'   legend_position = "none",
#'   facet_var = continent
#' )
#'
#'
#' @export
bubble_chart <- function(data, x, y,
                         size_var,
                         fill_var,
                         facet_var = NULL,
                         scale_size = c(0.5, 20),
                         alpha = 0.8,
                         stroke = 1.5,
                         trans_x = "identity",
                         trans_y = "identity",
                         title = NULL,
                         x_lab = NULL,
                         y_lab = NULL,
                         scales = "free",
                         ncol = 3,
                         legend_position = "right",
                         interactive = FALSE){

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  size_var <- rlang::enquo(size_var)
  fill_var <- rlang::enquo(fill_var)
  facet_var <- rlang::enquo(facet_var)

  x_name <- rlang::quo_name(x)
  y_name <- rlang::quo_name(y)


  if (!is.data.frame(data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  if (rlang::quo_is_missing(x)) {
    stop(call. = FALSE, "bubble_chart(x) is missing. Please supply a numeric column.")
  }

  if (rlang::quo_is_missing(y)) {
    stop(call. = FALSE, "bubble_chart(y) is missing. Please supply a numeric column.")
  }

  if (rlang::quo_is_missing(size_var)) {
    stop(call. = FALSE, "bubble_chart(size_var) is missing. Please supply a numeric column.")
  }

  if (rlang::quo_is_missing(fill_var)) {
    stop(call. = FALSE, "bubble_chart(fill_var) is missing. Please supply a categorical column.")
  }

  if(is.null(x_lab)){x_lab <- x_name}else{x_lab <- x_lab}
  if(is.null(y_lab)){y_lab <- y_name}else{y_lab <- y_lab}

  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = !!x, y= !!y, size = !!size_var, color = !!fill_var, fill = !!fill_var)) +
    ggplot2::geom_point(alpha = alpha, shape = 21, stroke = stroke) +
    ggplot2::scale_size(range = scale_size, guide = "none") +
    ggplot2::scale_x_continuous(
      trans = trans_x,
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::scale_y_continuous(
      trans = trans_y,
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::labs(title = title, x = x_lab, y = y_lab)+
    ggthemes::scale_color_tableau()+
    ggthemes::scale_fill_tableau()+
    ggplot2::theme_minimal()+
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 14, face="bold", hjust = 0,
        margin = ggplot2::margin(b=10)
      ),
      legend.position = legend_position,
      panel.grid.minor = ggplot2::element_blank()
    )

  # facet
  if(!rlang::quo_is_null(facet_var)){

    facet_var_cat <- data %>% dplyr::ungroup() %>% dplyr::select(!!facet_var) %>%
      dplyr::select(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x)) )

    if (length(facet_var_cat)==0){
      stop(call. = FALSE, "bubble_chart(facet_var) is missing. Please supply a categorical column.")
    }

    p <- p +
      ggplot2::facet_wrap(dplyr::vars(!!facet_var), scales = scales, ncol = ncol)
  }


  if(!interactive){
    p <- p +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 3)))

    return(p)

  } else {
    pt <- plotly::ggplotly(p) %>%
      plotly::layout(
        showlegend = TRUE,
        title = list(size = 11),
        font = list(family = "Segoe UI"),
        # margin = list(t = 60, b=50),
        hoverlabel = list(
          font = list(
            color = "black",
            family = "Consolas"
          ),
          bordercolor = "transparent"
        )
      )

    return(pt)
  }

}

