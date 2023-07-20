#' Plot Raincloud Plot
#'
#' Plot Raincloud Plot in ggplot2
#'
#' @param data A data frame or tibble.
#' @param x A numerical column.
#' @param fill_var A categorical column to group and fill.
#' @param fill character. Color of the points. Overrided if fill_var is specified.
#' @param alpha numeric. Opacity of density curve. Range: (0, 1).
#' @param box_alpha numeric. Opacity of box plot.
#' @param adjust numeric. Multiplicate bandwidth adjustment..
#' @param jitter_sample integer. Number of rows to sample.
#' @param jitter_alpha numeric. Opacity. Range: (0, 1).
#' @param jitter_height numeric. Height.
#' @param jitter_width numeric. Width.
#' @param scales character. Control facet x & y-axis ranges. Options include "fixed", "free", "free_y", "free_x".
#' @param ncol integer. Number of facet columns.
#' @param title character. Title for the plot.
#' @param x_lab character. X-axis label for the plot.
#' @param y_lab character. Y-axis label for the plot.
#' @param interactive logical. Whether or not to return an interactive plot (plotly).
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
#'
#' @examples
#'
#' raincloud_plot(iris, x = Sepal.Length)
#'
#' raincloud_plot(iris, x = Sepal.Length, fill = "orange")
#'
#' raincloud_plot(iris, x = Sepal.Length, fill_var = Species, ncol = 1, fill = "steelblue")
#'
#' raincloud_plot(ggplot2::diamonds, x = carat, fill_var = cut, ncol = 2, adjust = 2)
#'
#'
#' @export
raincloud_plot <- function(data,
                           x,
                           fill_var = NULL,
                           fill = NULL,
                           alpha = 0.8,
                           box_alpha = 0.5,
                           adjust = 1,
                           jitter_sample = 100,
                           jitter_alpha = 0.5,
                           jitter_height = 0.2,
                           jitter_width = 0.2,
                           scales = "free",
                           ncol = 1,
                           title = NULL,
                           x_lab = NULL,
                           y_lab = NULL,
                           interactive = FALSE) {

  x <- rlang::enquo(x)

  fill_var <- rlang::enquo(fill_var)

  if (!is.data.frame(data)) {
    stop(call. = FALSE, "data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  if (rlang::quo_is_missing(x)) {
    stop(call. = FALSE, "plotRaincloud(x) is missing. Please supply a numeric column.")
  }

  df_x <- data %>% dplyr::select(!!x) %>% dplyr::select(c(tidyselect::where(~is.numeric(.))))

  if (length(df_x)==0){
    stop(call. = FALSE, "Please supply a data-frame or tibble with a numeric column")
  }

  pal_discrete <- rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10)


  if (!rlang::quo_is_null(fill_var)) {

    df_cat <- data %>% dplyr::select(!!fill_var) %>%
      dplyr::select(c(tidyselect::where(~ is.character(.)|is.factor(.)|is.ordered(.))))

    if (length(df_cat)==0){
      stop(call. = FALSE, "Please supply a data-frame or tibble with a categorical column")
    }

    data_group <- data %>% dplyr::select(!!x, !!fill_var) %>% dplyr::group_by(!!fill_var)

    df_num <- data %>% dplyr::select(!!x, !!fill_var)

    box <-  data_group %>%
      dplyr::summarise(
        median = stats::median(!!x, na.rm = TRUE),
        mean = mean(!!x, na.rm = TRUE),
        p25 = stats::quantile(!!x, .25, na.rm = TRUE),
        p75 = stats::quantile(!!x, .75, na.rm = TRUE),
        dens_height = max(stats::density(!!x, na.rm = TRUE)$y),
        jitter_height = min(stats::density(!!x, na.rm = TRUE)$y),
        iqr = p75 - p25,
        lower_whisker =  p25 - 1.5*iqr,
        upper_whisker =  p75 + 1.5*iqr
      )

    if(is.null(fill)){
      p1 <- df_num %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(
          data = df_num,
          ggplot2::aes(x = !!x, y = ggplot2::after_stat(scaled), fill = !!fill_var, color = !!fill_var),
          alpha = alpha, linewidth = 1, adjust = adjust
        ) +
        ggplot2::geom_jitter(
          data = df_num %>% dplyr::sample_n(jitter_sample, replace = TRUE),
          ggplot2::aes(x = !!x, y = -0.2, fill = !!fill_var, color = !!fill_var),
          height = jitter_height, width = jitter_width, alpha = jitter_alpha
        ) +
        #errorbar
        ggplot2::stat_boxplot(
          data = df_num,
          ggplot2::aes(x = !!x, y = -0.2, color=!!fill_var),
          geom = "errorbar", width = 0.10, linewidth = 1
        ) +
        # box
        ggplot2::geom_rect(
          data = box,
          ggplot2::aes(xmin = p25, xmax = median, ymin = - 0.3, ymax = - 0.1, fill = !!fill_var, color = !!fill_var),
          alpha = box_alpha, linewidth = 1
        )+
        ggplot2::geom_rect(
          data = box,
          ggplot2::aes(xmin = median, xmax = p75, ymin = - 0.3, ymax = - 0.1, fill = !!fill_var, color = !!fill_var),
          alpha = box_alpha, linewidth = 1
        )+
        # vertical line
        ggplot2::geom_segment(
          data = box,
          ggplot2::aes(x = median, xend = median, y = - 0.3, yend = - 0.1),
          color = "white", linewidth = 1, alpha = 1
        )+
        ggplot2::facet_wrap(dplyr::vars(!!fill_var), scales = scales, ncol = ncol)+
        ggplot2::scale_fill_manual(values = pal_discrete, guide="none")+
        ggplot2::scale_color_manual(values = pal_discrete, guide="none")

    }else{

      p1 <- df_num %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(
          data = df_num,
          ggplot2::aes(x = !!x, y = ggplot2::after_stat(scaled)), fill = fill, color = fill,
          alpha = alpha, linewidth = 1, adjust = adjust
        ) +
        ggplot2::geom_jitter(
          data = df_num %>% dplyr::sample_n(jitter_sample, replace = TRUE),
          ggplot2::aes(x = !!x, y = -0.2), fill = fill, color = fill,
          height = jitter_height, width = jitter_width, alpha = jitter_alpha
        ) +
        #errorbar
        ggplot2::stat_boxplot(
          data = df_num,
          ggplot2::aes(x = !!x, y = -0.2), color = fill,
          geom = "errorbar", width = 0.10, linewidth = 1
        ) +
        # box
        ggplot2::geom_rect(
          data = box,
          ggplot2::aes(xmin = p25, xmax = median, ymin = - 0.3, ymax = - 0.1), fill = fill, color = fill,
          alpha = box_alpha, linewidth = 1
        )+
        ggplot2::geom_rect(
          data = box,
          ggplot2::aes(xmin = median, xmax = p75, ymin = - 0.3, ymax = - 0.1), fill = fill, color = fill,
          alpha = box_alpha, linewidth = 1
        )+
        # vertical line
        ggplot2::geom_segment(
          data = box,
          ggplot2::aes(x = median, xend = median, y = - 0.3, yend = - 0.1),
          color = "white", linewidth = 1, alpha = 1
        )+
        ggplot2::facet_wrap(dplyr::vars(!!fill_var), scales = scales, ncol = ncol)
    }


    p1 <- p1 +
      ggplot2::labs(title = title, x = x_lab, y = y_lab)+
      ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.text =  ggplot2::element_text(size = 8),
        axis.text.y = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 14, face="bold", hjust = 0),
        panel.spacing.y = ggplot2::unit(1, "lines"),
        panel.grid.minor = ggplot2::element_blank()
      )


  } else{

    if(is.null(fill)){fill <- '#4e79a7'}else{fill}

    df_num <- data %>% dplyr::select(!!x)

    box <-  df_num %>%
      dplyr::summarise(
        median = stats::median(!!x, na.rm = TRUE),
        mean = mean(!!x, na.rm = TRUE),
        p25 = stats::quantile(!!x, .25, na.rm = TRUE),
        p75 = stats::quantile(!!x, .75, na.rm = TRUE),
        iqr = p75 - p25,
        lower_whisker =  p25 - 1.5*iqr,
        upper_whisker =  p75 + 1.5*iqr
      )

    p1 <- df_num %>%
      ggplot2::ggplot()+
      ggplot2::geom_density(
        data = df_num,
        ggplot2::aes(x = !!x, y = ggplot2::after_stat(scaled)),
        fill = fill, color = fill, alpha = alpha, linewidth = 1
      ) +
      ggplot2::geom_jitter(
        data = df_num %>% dplyr::sample_n(100, replace = TRUE),
        ggplot2::aes(x = !!x, y = -0.2),
        fill = fill, color = fill, height = jitter_height, width = jitter_width, alpha = jitter_alpha
      ) +
      #errorbar
      ggplot2::stat_boxplot(
        data = df_num,
        ggplot2::aes(x = !!x, y = -0.2),
        geom = "errorbar", width = 0.10, size = 1, color = fill
      ) +
      # box
      ggplot2::geom_rect(
        data = box,
        ggplot2::aes(xmin = p25, xmax = median, ymin = - 0.3, ymax = - 0.1),
        fill = fill, color = fill, alpha = box_alpha, linewidth = 1
      )+
      ggplot2::geom_rect(
        data = box,
        ggplot2::aes(xmin = median, xmax = p75, ymin = - 0.3, ymax = - 0.1),
        fill = fill, color = fill, alpha = box_alpha, linewidth = 1
      ) +
      # vertical line
      ggplot2::geom_segment(
        data = box,
        ggplot2::aes(x = median, xend = median, y = - 0.3, yend = - 0.1),
        color = "white", linewidth = 1, alpha = 1
      )+
      ggplot2::labs(title = title, x = x_lab, y = y_lab)+
      ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.text =  ggplot2::element_text(size = 8),
        axis.text.y = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 14, face="bold", hjust = 0),
        panel.spacing.y = ggplot2::unit(1, "lines"),
        panel.grid.minor = ggplot2::element_blank()
      )
  }

  if(!interactive){
    return(p1)
  }else{

    p <- plotly::ggplotly(p1, tooltip = c("x", "y", "fill")) %>%
      plotly::layout(
        showlegend = F,
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
