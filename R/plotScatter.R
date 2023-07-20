#' Explore numeric columns with Scatterplot
#'
#' Plot Scatterplot of all numeric variables from data frame fixed to a selected numeric variable.
#'
#' @param data A data frame or tibble.
#' @param y A numeric column.
#' @param fill_var A categorical column to group and fill.
#' @param fill character. Color of the points. Overrided if fill_var is specified.
#' @param ncol integer. Number of facet columns.
#' @param alpha numeric. Opacity. Range: (0, 1).
#' @param size numeric. Size of the points.
#' @param scales character. Control facet x & y-axis ranges. Options include "fixed", "free", "free_y", "free_x".
#' @param sample integer. Number of rows to sample.
#' @param smooth logical. Whether or not to include a smooth line.
#' @param smooth_color character. Smooth line color
#' @param smooth_alpha numeric. Confidence interval opacity around the smooth line
#' @param smooth_method character. Smoothing method (function).
#' @param smooth_se logical. Whether or not to include a confidence interval around the smooth line.
#' @param smooth_fill character. Confidence interval fill around the smooth line.
#' @param trans_x character. X-axis transformation.
#' @param trans_y character. Y-axis transformation.
#' @param legend_position character. Legend position. Options include "right", "left", "top", "bottom", "none".
#' @param title character. Title for the plot.
#' @param x_lab character. X-axis label for the plot.
#' @param y_lab character. Y-axis label for the plot.
#' @param interactive logical. Whether or not to return an interactive plot (plotly).
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
#'
#' @examples
#'
#' plotScatter(iris, y = Sepal.Length)
#'
#' plotScatter(iris, y = Sepal.Length, fill_var = Species)
#'
#' plotScatter(ggplot2::diamonds, y=price, sample = 1000, alpha = 0.4)
#'
#' plotScatter(ggplot2::diamonds, y=price, sample = 1000, alpha = 0.4, fill="steelblue", smooth = TRUE)
#'
#' plotScatter(ggplot2::diamonds, y=price, sample = 1000, alpha = 0.4, smooth = TRUE)
#'
#'
#' @export
plotScatter <- function(data,
                        y,
                        fill_var = NULL,
                        fill = NULL,
                        ncol = 3,
                        alpha = 1,
                        size = 2,
                        scales = "free",
                        sample = NULL,
                        smooth = FALSE,
                        smooth_color = "dodgerblue4",
                        smooth_fill = "gray",
                        smooth_alpha = 0.3,
                        smooth_method = "loess",
                        smooth_se = FALSE,
                        legend_position = "right",
                        trans_x = "identity",
                        trans_y = "identity",
                        title = NULL,
                        x_lab = NULL,
                        y_lab = NULL,
                        interactive = FALSE) {

  y <- rlang::enquo(y)

  fill_var  <- rlang::enquo(fill_var)

  if (!is.data.frame(data)) {
    stop(call. = FALSE, "data is not a data-frame or tibble. Please supply a data-frame or tibble.")
  }

  if (rlang::quo_is_missing(y)) {
    stop(call. = FALSE, "plotScatter(y) is missing. Please supply a numeric column.")
  }

  y_name <- rlang::quo_name(y)

  fill_name <- rlang::quo_name(fill_var)

  pal_discrete <-
    rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10)

  sample <- if(is.null(sample)){sample = nrow(data)}else{sample}

  if (!rlang::quo_is_null(fill_var)) {

    df_fill <- data %>% dplyr::ungroup() %>% dplyr::select(!!fill_var) %>%
      dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

    if (length(df_fill)==0){
      stop(call. = FALSE, "Please supply fill_var with a categorical column")
    }

    data <- data %>% dplyr::ungroup() %>%
      dplyr::select(tidyselect::where(~ is.numeric(.x)), !!fill_var)

    data_formatted <- data %>%
      dplyr::slice_sample(n = sample) %>%
      tidyr::pivot_longer(
        !dplyr::any_of(c(y_name, fill_name)),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::arrange(variable)

    p1 <- data_formatted %>%
      ggplot2::ggplot() +
      ggplot2::geom_point(
        ggplot2::aes(x = value, y = !!y, color = !!fill_var),
        alpha = alpha, size = size
      ) +
      ggplot2::scale_color_manual(values = pal_discrete)


    if(smooth){
      p1 <- p1 +
        ggplot2::geom_smooth(
          ggplot2::aes(x = value, y = !!y, color = !!fill_var), linewidth = 1,
          method = smooth_method, se = smooth_se, alpha = smooth_alpha, fill = smooth_fill
        )
    }


  } else {

    data <- data %>% dplyr::ungroup() %>%
      dplyr::select(tidyselect::where(~ is.numeric(.x)))

    data_formatted <- data %>%
      dplyr::slice_sample(n = sample) %>%
      tidyr::pivot_longer(
        !dplyr::any_of(y_name),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::arrange(variable)

    if(is.null(fill)){
      p1 <- data_formatted %>%
        ggplot2::ggplot() +
        ggplot2::geom_point(
          ggplot2::aes(x = value, y = !!y, color = variable),
          alpha = alpha, size = size
        ) +
        ggplot2::scale_color_manual(values = pal_discrete, guide = "none")

    }else{
      p1 <- data_formatted %>%
        ggplot2::ggplot() +
        ggplot2::geom_point(
          ggplot2::aes(x = value, y = !!y), color = fill,
          alpha = alpha, size = size
        )
    }

    if(smooth){
      p1 <- p1 +
        ggplot2::geom_smooth(
          ggplot2::aes(x = value, y = !!y), linewidth = 1, color = smooth_color,
          method = smooth_method, se = smooth_se, alpha = smooth_alpha, fill = smooth_fill
        )
    }

  }

  if(is.null(y_lab)){y_lab <- y_name}

  p1 <- p1 +
    ggplot2::scale_x_continuous(trans = trans_x) +
    ggplot2::scale_y_continuous(trans = trans_y) +
    ggplot2::labs(title = title, x = x_lab, y = y_lab)+
    ggplot2::facet_wrap(dplyr::vars(variable), scales = scales, ncol = ncol)+
    ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 14, face="bold", hjust = 0),
      panel.spacing.y = ggplot2::unit(1, "lines"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = legend_position
    )


  if(!interactive){
    return(p1)
  }else{
    p <- plotly::ggplotly(p1, tooltip = c("x", "y", "fill")) %>%
      plotly::layout(
        showlegend = FALSE,
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

    return(p)
  }

}
