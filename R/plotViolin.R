#' Explore numeric and categorical columns with Violin plot
#'
#' Plot Violin of all variables fixing on a selected variable.
#'
#' @param data A data frame or tibble.
#' @param target A categorical column to group and fill
#' @param fill character. Color of all Violin. Overrided if fill_var is specified.
#' @param ncol integer. Number of facet columns.
#' @param scales character. Control facet x & y-axis ranges. Options include "fixed", "free", "free_y", "free_x".
#' @param alpha numeric. Opacity. Range: (0, 1).
#' @param adjust numeric. Multiplicate bandwidth adjustment.
#' @param trim A logical. Whether or not to trim tails.
#' @param title character. Title for the plot.
#' @param x_lab character. X-axis label for the plot.
#' @param y_lab character. Y-axis label for the plot.
#' @param boxplot_show logical. Whether or not to show boxplot.
#' @param flip logical. whether or not to flip cartesian coordinates.
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
#'
#' @examples
#'
#' plotViolin(iris, target = Species)
#'
#' plotViolin(iris, target = Species, boxplot_show = TRUE)
#'
#' plotViolin(iris, target=Species, boxplot_show=TRUE, flip=TRUE)
#'
#' plotViolin(iris, target=Species, fill="steelblue")
#'
#' mtcars <- mtcars %>% dplyr::as_tibble() %>%
#'   dplyr::mutate(dplyr::across(c(am, carb, cyl, gear, vs), as.factor))
#'
#' plotViolin(mtcars, target = mpg)
#'
#' plotViolin(mtcars, target = mpg, boxplot_show=TRUE)
#'
#' plotViolin(mtcars, target = mpg, fill = "steelblue")
#'
#' @export
plotViolin <- function(data,
                       target,
                       fill = NULL,
                       ncol = 3,
                       scales = "free",
                       alpha = 0.7,
                       adjust = 2,
                       trim = FALSE,
                       title = NULL,
                       x_lab = NULL,
                       y_lab = NULL,
                       boxplot_show = FALSE,
                       flip = FALSE){

  if (!is.data.frame(data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  target  <- rlang::enquo(target)

  pal_discrete <- rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10)


  df_target_num <- data %>% dplyr::ungroup() %>% dplyr::select(!!target) %>%
    dplyr::select(c(tidyselect::where(~is.numeric(.x))))

  df_target_cat <- data %>% dplyr::ungroup() %>% dplyr::select(!!target) %>%
    dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

  if (length(df_target_cat)==1) {

    df_num <- data %>% dplyr::ungroup() %>%
      dplyr::select(tidyselect::where(~ is.numeric(.x)), !!target)

    target_var <- rlang::quo_name(target)

    data_formatted <- df_num %>%
      tidyr::pivot_longer(
        !dplyr::any_of(target_var),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::arrange(variable) %>%
      dplyr::filter(!is.na(value))

    if(is.null(fill)){
      p1 <- ggplot2::ggplot(data_formatted, ggplot2::aes(x = value, y = !!target)) +
        ggplot2::geom_violin(
          ggplot2::aes(fill = !!target, color = !!target),
          alpha = alpha,
          linewidth = 1,
          trim = trim,
          adjust = adjust
        ) +
        ggplot2::scale_fill_manual(values = pal_discrete, guide = "none") +
        ggplot2::scale_color_manual(values = pal_discrete, guide = "none")

    }else{
      p1 <- ggplot2::ggplot(data_formatted, ggplot2::aes(x = value, y = !!target)) +
        ggplot2::geom_violin(
          fill = fill,
          color = fill,
          alpha = alpha,
          linewidth = 1,
          trim = trim,
          adjust = adjust
        )
    }

    p1 <- p1 +
      ggplot2::facet_wrap(ggplot2::vars(variable), scales = scales, ncol = ncol) +
      ggplot2::labs(title = title, x = x_lab, y = y_lab) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 14, face="bold", hjust = 0),
        panel.spacing.y = ggplot2::unit(1, "lines"),
        panel.grid.minor = ggplot2::element_blank()
      )

    if(boxplot_show){
      p1 <- p1 +
        ggplot2::geom_boxplot(width=0.1, outlier.shape = NA)
    }

    if(flip) {
      p1 <- p1 +
        ggplot2::coord_flip()
    }


  }else if(length(df_target_num)==1){

    df_cat <- data %>% dplyr::ungroup() %>%
      dplyr::select(!!target, tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x)))

    target_var <- rlang::quo_name(target)

    data_formatted <- df_cat %>%
      dplyr::mutate(dplyr::across(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x)), as.character)) %>%
      tidyr::pivot_longer(
        !dplyr::any_of(target_var),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::arrange(variable) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(variable = paste(target_var,"vs",variable))


    if(is.null(fill)){
      p1 <- ggplot2::ggplot(data_formatted, ggplot2::aes(x = !!target, y = value)) +
        ggplot2::geom_violin(
          ggplot2::aes(fill = variable, color = variable),
          alpha = alpha,
          linewidth = 1,
          trim = trim,
          adjust = adjust
        ) +
        ggplot2::scale_fill_manual(values = pal_discrete, guide = "none") +
        ggplot2::scale_color_manual(values = pal_discrete, guide = "none")

    }else{
      p1 <- ggplot2::ggplot(data_formatted, ggplot2::aes(x = !!target, y = value)) +
        ggplot2::geom_violin(
          fill = fill,
          color = fill,
          alpha = alpha,
          linewidth = 1,
          trim = trim,
          adjust = adjust
        )
    }

    p1 <- p1 +
      ggplot2::facet_wrap(ggplot2::vars(variable), scales = scales, ncol = ncol) +
      ggplot2::labs(title = title, x = x_lab, y = y_lab) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 14, face="bold", hjust = 0),
        panel.spacing.y = ggplot2::unit(1, "lines"),
        panel.grid.minor = ggplot2::element_blank()
      )

    if(boxplot_show){
      p1 <- p1 +
        ggplot2::geom_boxplot(width=0.1, outlier.shape = NA)
    }

    if(flip) {
      p1 <- p1 +
        ggplot2::coord_flip()
    }


  }else{
    stop(call. = FALSE, "plotViolin(target) is missing. Please supply a data-frame or tibble with a categorical or numerical column")
  }

  return(p1)

}
