#' Scatter Chart
#'
#' Plot Scatter Chart in ggplot2
#'
#' @param data A data frame or tibble.
#' @param x A numeric column.
#' @param y A numeric column.
#' @param fill_var A numeric or categorical variable to group and fill.
#' @param facet_var A categorical variable used to create facet panels.
#' @param color character. Color of the points. Overrided if fill_var is specified.
#' @param alpha numeric. Opacity. Range: (0, 1).
#' @param size numeric. Size of the points.
#' @param ncol integer. Number of facet columns.
#' @param scales character. control facet x & y-axis ranges. Options include "fixed", "free", "free_y", "free_x".
#' @param highlight A vector of categories to highlight.
#' @param compare logical. Whether or not to compare a group with the others.
#' @param trans_x character. X-axis transformation.
#' @param trans_y character. Y-axis transformation.
#' @param smooth logical. Whether or not to include a smooth line.
#' @param se logical. Whether or not to include a confidence interval around the smooth line.
#' @param method character. Smoothing method (function).
#' @param title character. Title for the plot.
#' @param x_lab character. X-axis label for the plot.
#' @param y_lab character. Y-axis label for the plot.
#' @param legend_position character. Legend position. Options include "right", "left", "top", "bottom", "none".
#' @param interactive logical. Whether or not to return an interactive plot (plotly).
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
#'
#' @examples
#' scatter_chart(iris, x = Petal.Width, y=Sepal.Length)
#'
#' scatter_chart(iris, x = Petal.Width, y=Sepal.Length, facet_var = Species)
#'
#' scatter_chart(iris, x = Petal.Width, y=Sepal.Length, facet_var = Species,
#'               compare = TRUE)
#'
#' scatter_chart(iris, x = Petal.Width, y=Sepal.Length, fill_var = Species)
#'
#' scatter_chart(iris, x = Petal.Width, y=Sepal.Length, fill_var = Species,
#'               highlight = c("setosa"))
#'
#' scatter_chart(iris, x = Petal.Width, y=Sepal.Length, fill_var = Species,
#'               smooth = TRUE, se = TRUE)
#'
#' scatter_chart(iris, x = Petal.Width, y=Sepal.Length, fill_var = Species,
#'               facet_var = Species)
#'
#' scatter_chart(iris, x = Petal.Width, y=Sepal.Length, fill_var = Species,
#'               facet_var = Species, compare = TRUE)
#'
#' scatter_chart(iris, x = Petal.Width, y=Sepal.Length, fill_var = Species,
#'               facet_var = Species, compare = TRUE, highlight = "setosa")
#'
#' scatter_chart(iris, x = Petal.Width, y=Sepal.Length, fill_var = Species,
#'               facet_var = Species, scales = "free")
#'
#'
#' @export
scatter_chart <- function(data, x, y,
                          fill_var = NULL,
                          facet_var = NULL,
                          color = "#4E79A7",
                          alpha = 1,
                          size = 1.5,
                          ncol = 2,
                          scales = "fixed",
                          highlight = NULL,
                          compare = FALSE,
                          trans_x = "identity",
                          trans_y ="identity",
                          smooth = FALSE,
                          method = "lm",
                          se = FALSE,
                          title = NULL,
                          x_lab = NULL,
                          y_lab = NULL,
                          legend_position = "right",
                          interactive = FALSE){

  x  <- rlang::enquo(x)
  y  <- rlang::enquo(y)
  fill_var  <- rlang::enquo(fill_var)
  facet_var  <- rlang::enquo(facet_var)

  x_name <- rlang::quo_name(x)

  y_name <- rlang::quo_name(y)

  if (!is.data.frame(data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  if (rlang::quo_is_missing(x)) {
    stop(call. = FALSE, "scatter_chart(x) is missing. Please supply a numeric column.")
  }

  if (rlang::quo_is_missing(y)) {
    stop(call. = FALSE, "scatter_chart(y) is missing. Please supply a numeric column.")
  }

  pal_discrete <-
    rev(rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10))


  # fill var
  if (rlang::quo_is_null(fill_var)){

    p1 <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y)) +
      ggplot2::geom_point(color = color, alpha = alpha, size = size)

  }else{

    p1 <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y, color = !!fill_var))

    if (is.null(highlight)){

      p1 <- p1 + ggplot2::geom_point(alpha = alpha, size = size)

    }else{
      tmp <- data %>%
        dplyr::filter(!!fill_var %in% highlight)

      p1 <- p1 +
        ggplot2::geom_point(ggplot2::aes(group = !!fill_var), color = "lightgray", alpha = alpha, size = size) +
        ggplot2::geom_point(data = tmp, alpha = alpha, size = size)
    }

  }


  # Facet
  if (!rlang::quo_is_null(facet_var)){

    facet_var_cat <- data %>% dplyr::select(!!facet_var) %>%
      dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

    if (length(facet_var_cat)==0){
      stop(call. = FALSE, "scatter_chart(facet_var) is missing. Please supply a categorical column.")
    }


    if(rlang::quo_is_null(fill_var)){

      if(compare){
        tmp <- data %>%
          dplyr::mutate(fill_var_ = !!facet_var) %>%
          dplyr::select(-!!facet_var)

        p1 <-
          ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y)) +
          ggplot2::geom_point(data = tmp, ggplot2::aes(group = fill_var_), color = "lightgray", alpha = alpha, size = size) +
          ggplot2::geom_point(color = color, alpha = alpha, size = size)
      }

    }else{

      if(compare){

        tmp <- data %>%
          dplyr::mutate(fill_var_ = !!fill_var) %>%
          dplyr::select(-!!fill_var)

        p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = !!x, y = !!y, color = !!fill_var)) +
          ggplot2::geom_point(data = tmp, ggplot2::aes(group = fill_var_),
                              color = "lightgray", alpha = alpha, size = size)

        if(is.null(highlight)){

          p1 <- p1 +
            ggplot2::geom_point(alpha = alpha, size = size)

        }else{

          tmp <- data %>%
            dplyr::filter(!!fill_var %in% highlight)

          p1 <- p1 +
            ggplot2::geom_point(data = tmp, ggplot2::aes(x = !!x, y = !!y, color = !!fill_var),
                                alpha = alpha, size = size)

        }

      }

    }

    p1 <- p1 +
      ggplot2::facet_wrap(dplyr::vars(!!facet_var), scales = scales, ncol = ncol)

  }

  # scale_color
  if (!rlang::quo_is_null(fill_var)){

    fill_var_cat <- data %>% dplyr::ungroup() %>% dplyr::select(!!fill_var) %>%
      dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

    fill_var_num <- data %>% dplyr::ungroup() %>% dplyr::select(!!fill_var) %>%
      dplyr::select(c(tidyselect::where(~ is.numeric(.x) )))


    if(length(fill_var_cat)==1){
      p1 <- p1 +
        ggplot2::scale_color_manual(values = rev(pal_discrete))
    }

    if(length(fill_var_num)==1){
      p1 <- p1 +
        ggplot2::scale_colour_gradient(low = "#56B1F7", high = "#132B43")
    }
  }

  # trans
  p1 <- p1 +
    ggplot2::scale_x_continuous(
      trans = trans_x,
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::scale_y_continuous(
      trans = trans_y,
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    )

  if(smooth){
    p1 <- p1 +
      ggplot2::geom_smooth(ggplot2::aes(fill = !!fill_var), method = method, se = se, alpha = 0.2, linewidth = 1) +
      ggplot2::scale_fill_manual(values = rev(pal_discrete))

  } else{
    p1 <- p1
  }

  if(is.null(x_lab)){x_lab <- x_name}else{x_lab <- x_lab}
  if(is.null(y_lab)){y_lab <- y_name}else{y_lab <- y_lab}

  p1 <- p1 +
    ggplot2::labs(title = title, x = x_lab, y = y_lab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 14, face="bold", hjust = 0,
        margin = ggplot2::margin(b=10)
      ),
      legend.position = legend_position,
      panel.grid.minor = ggplot2::element_blank()
    )


  if(interactive){
    pt <- plotly::ggplotly(p1) %>%
      plotly::layout(
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

  }else{

    return(p1)

  }

}
