#' Explore numeric columns with Density plot
#'
#' Plot Density distribution of all numeric variables from data frame.
#'
#' @param data A data.frame or tibble.
#' @param fill_var A categorical variable to group and fill.
#' @param fill character. Color of all Density and Histograms. Overrided if fill_var is specified.
#' @param ncol integer. Number of facet columns.
#' @param bins integer. Number of bins. Overrided if show_histogram = FALSE.
#' @param alpha numeric. Opacity. Range: (0, 1).
#' @param scales character. Control facet x & y-axis ranges. Options include "fixed", "free", "free_y", "free_x".
#' @param show_boxplot logical. Whether or not to include Boxplot Overrided if show_histogram = TRUE.
#' @param show_histogram logical. Whether or not to include histograms. Overrided if show_boxplot = TRUE.
#' @param adjust numeric. Multiplicate bandwidth adjustment.
#' @param trans_x character. X-axis transformation.
#' @param trans_y character. X-axis transformation.
#' @param title character. Title for the plot.
#' @param x_lab character. X-axis label for the plot.
#' @param y_lab character. Y-axis label for the plot.
#' @param legend_position character. Legend position. Options include "right", "left", "top", "bottom", "none".
#' @param interactive logical. Whether or not to return an interactive plot (plotly).
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
#'
#' @examples
#' plotDensity(iris, alpha = 0.5, ncol = 2)
#'
#' plotDensity(iris, alpha = 0.5, ncol = 2, show_boxplot = TRUE)
#'
#' plotDensity(iris, fill = "steelblue", alpha = 0.5, adjust = 2)
#'
#' plotDensity(iris, fill = "steelblue", alpha = 0.5, adjust = 2, show_boxplot = TRUE)
#'
#' plotDensity(iris, alpha = .5, show_histogram = TRUE, adjust = 2)
#'
#' plotDensity(iris, fill = "steelblue", alpha = .2, show_histogram = TRUE, adjust = 2)
#'
#' plotDensity(iris, alpha = .2, fill_var = Species)
#'
#' plotDensity(iris, alpha = .2, fill_var = Species, legend_position = "bottom", ncol=2)
#'
#' plotDensity(iris, alpha = .2, fill_var = Species, show_histogram = TRUE, adjust = 2)
#'
#'
#' @export
plotDensity <- function(data,
                        fill_var = NULL,
                        fill = NULL,
                        ncol = 3,
                        bins = 30,
                        alpha = 1,
                        scales = "free",
                        show_histogram = FALSE,
                        show_boxplot = FALSE,
                        adjust = 1,
                        trans_x = "identity",
                        trans_y = "identity",
                        legend_position = "right",
                        title = NULL,
                        x_lab = NULL,
                        y_lab = NULL,
                        interactive = FALSE) {

  if (!is.data.frame(data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  fill_var  <- rlang::enquo(fill_var)

  pal_discrete <-
    rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10)

  if (!rlang::quo_is_null(fill_var)) {

    df_cat <- data %>% dplyr::ungroup() %>% dplyr::select(!!fill_var) %>%
      dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

    if (length(df_cat)==0){
      stop(call. = FALSE, "Please supply a data-frame or tibble with a categorical column")
    }

    df_num <- data %>% dplyr::ungroup() %>%
      dplyr::select(tidyselect::where(~ is.numeric(.x)), !!fill_var)

    group_var <- rlang::quo_name(fill_var)

    data_formatted <- df_num %>%
      tidyr::pivot_longer(
        !dplyr::any_of(group_var),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::arrange(variable) %>%
      dplyr::filter(!is.na(value))


    p1 <- data_formatted %>%
      ggplot2::ggplot()

    if(!show_histogram){
      p1 <- p1 +
        ggplot2::geom_density(
          ggplot2::aes(x = value, color=!!fill_var, fill=!!fill_var),
          linewidth = 1, adjust = adjust, alpha = alpha
        )
    }else{
      p1 <- p1 +
        ggplot2::geom_histogram(
          ggplot2::aes(x = value,  y = ggplot2::after_stat(density), fill=!!fill_var),
          alpha = alpha, bins = bins, position = 'identity'
        )+
        ggplot2::geom_density(
          ggplot2::aes(x = value, color=!!fill_var),
          linewidth = 1, adjust = adjust
        )
    }

    p1 <- p1 +
      ggplot2::facet_wrap(dplyr::vars(variable), scales = scales, ncol = ncol) +
      ggplot2::scale_fill_manual(values = pal_discrete)+
      ggplot2::scale_color_manual(values = pal_discrete)

  }else{

    df_num <- data %>% dplyr::ungroup() %>%
      dplyr::select(tidyselect::where(~ is.numeric(.x)))

    data_formatted <- df_num %>%
      tibble::rownames_to_column() %>%
      tidyr::pivot_longer(
        !rowname, names_to = "variable", values_to = "value"
      ) %>%
      dplyr::select(-rowname) %>%
      dplyr::arrange(variable) %>%
      dplyr::filter(!is.na(value))

    p1 <- data_formatted %>%
      ggplot2::ggplot()

    if(!show_histogram){
      if(is.null(fill)){
        if(!show_boxplot){
          p1 <- p1 +
            ggplot2::geom_density(
              ggplot2::aes(x = value, fill=variable, color=variable), adjust = adjust,
              alpha = alpha, position = 'identity', linewidth=1
            ) +
            ggplot2::scale_fill_manual(values = pal_discrete, guide="none") +
            ggplot2::scale_color_manual(values = pal_discrete, guide="none")

        }else{
          p1 <- p1 +
            ggplot2::geom_boxplot(
              ggplot2::aes(x=value, y = -0.25, fill=variable, color=variable),
              alpha = alpha, linewidth=1, width = 0.20
            )+
            ggplot2::stat_boxplot(
              ggplot2::aes(x=value, y = -0.25, color=variable),
              geom = "errorbar", width = 0.15
            )+
            ggplot2::geom_density(
              ggplot2::aes(x = value, y = ggplot2::after_stat(scaled),fill=variable, color=variable),
              inherit.aes = FALSE, alpha = alpha, linewidth = 1, adjust = adjust
            )+
            ggplot2::scale_fill_manual(values = pal_discrete, guide="none") +
            ggplot2::scale_color_manual(values = pal_discrete, guide="none")
        }

      } else{

        if(!show_boxplot){
          p1 <- p1 +
            ggplot2::geom_density(
              ggplot2::aes(x = value), fill=fill, color=fill, adjust = adjust,
              alpha = alpha, position = 'identity', linewidth=1
            )
        }else{
          p1 <- p1 +
            ggplot2::geom_boxplot(
              ggplot2::aes(x=value, y = -0.25), fill=fill, color=fill,
              alpha = alpha, linewidth=1, width = 0.20
            )+
            ggplot2::stat_boxplot(
              ggplot2::aes(x=value, y = -0.25), color=fill,
              geom = "errorbar", width = 0.15
            )+
            ggplot2::geom_density(
              ggplot2::aes(x = value, y = ggplot2::after_stat(scaled)),
              fill=fill, color=fill, adjust = adjust,
              inherit.aes = FALSE, alpha = alpha, linewidth = 1
            )
        }
      }

    }else{
      if(is.null(fill)){
        p1 <- p1 +
          ggplot2::geom_histogram(
            ggplot2::aes(x = value,  y = ggplot2::after_stat(density), fill=variable),
            alpha = alpha, bins = bins, position = 'identity'
          )+
          ggplot2::geom_density(
            ggplot2::aes(x = value, color=variable),
            linewidth = 1, adjust = adjust
          ) +
          ggplot2::scale_fill_manual(values = pal_discrete, guide="none")+
          ggplot2::scale_color_manual(values = pal_discrete, guide="none")

      }else{
        p1 <- p1 +
          ggplot2::geom_histogram(
            ggplot2::aes(x = value,  y = ggplot2::after_stat(density)), fill=fill,
            alpha = alpha, bins = bins, position = 'identity'
          )+
          ggplot2::geom_density(
            ggplot2::aes(x = value), color=fill,
            linewidth = 1, adjust = adjust
          )
      }
    }
  }

  p1 <- p1 +
    ggplot2::scale_x_continuous(trans = trans_x) +
    ggplot2::scale_y_continuous(trans = trans_y) +
    ggplot2::facet_wrap(dplyr::vars(variable), scales = scales, ncol = ncol)+
    ggplot2::labs(x = x_lab, y = y_lab, title = title)+
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
        showlegend = T,
        title = list(text = title, size=1),
        font = list(family = "Segoe UI"),
        # margin = list(t = 50, b=40),
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
