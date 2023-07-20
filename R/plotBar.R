#' Explore categorical columns with bar charts
#'
#' Plot Bar Chart of all categorical columns from data frame.
#'
#' @param data A data frame or tibble.
#' @param fill_var A categorical column to group and fill.
#' @param fill character. Color of bars. Overrided if fill_var is specified.
#' @param width numeric. Width of the bars.
#' @param top integer. Top n levels.
#' @param pct logical. Whether or not to display proportions instead of counts. Overrided if fill_var is specified.
#' @param position character. Position adjustment. Options include "stack", fill" or "dodge".
#' @param ncol integer. Number of facet columns.
#' @param scales character. Control facet x & y-axis ranges. Options include "fixed", "free", "free_y", "free_x"
#' @param alpha numeric. Opacity. Range: (0, 1).
#' @param title character. Title for the plot.
#' @param x_lab character. X-axis label for the plot.
#' @param y_lab character. Y-axis label for the plot.
#' @param legend_position character. Legend position. Options include "right", "left", "top", "bottom", "none".
#' @param interactive logical. Whether or not to return an interactive plot (plotly).
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
#'
#' @examples
#' plotBar(ggplot2::diamonds)
#'
#' plotBar(ggplot2::diamonds, fill = "steelblue")
#'
#' plotBar(ggplot2::diamonds, fill = "steelblue", pct = TRUE)
#'
#' plotBar(ggplot2::diamonds, fill = "steelblue", top = 5)
#'
#' plotBar(ggplot2::diamonds, fill = "steelblue", top = 5, ncol = 2)
#'
#' plotBar(ggplot2::diamonds, fill_var = cut)
#'
#' plotBar(ggplot2::diamonds, fill_var = cut, position = "stack")
#'
#' plotBar(ggplot2::diamonds, fill_var = cut, position = "dodge")
#'
#' plotBar(ggplot2::diamonds, fill_var = cut, position = "fill")
#'
#' @export
plotBar <- function(data,
                    fill_var = NULL,
                    fill = NULL,
                    width = 0.7,
                    top = 10,
                    pct = FALSE,
                    position = NULL,
                    ncol = 3,
                    scales = "free",
                    alpha = 1,
                    title = NULL,
                    x_lab = NULL,
                    y_lab = NULL,
                    legend_position = "right",
                    interactive = FALSE) {

  if (!is.data.frame(data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  fill_var <- rlang::enquo(fill_var)

  pal_discrete <-
    rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10)

  if (!rlang::quo_is_null(fill_var)) {

    df_fill <- data %>% dplyr::ungroup() %>% dplyr::select(!!fill_var) %>%
      dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

    if (length(df_fill)==0){
      stop(call. = FALSE, "Please supply a data-frame or tibble with a categorical column")
    }

    df_cat <- data %>% dplyr::ungroup() %>%
      dplyr::select(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x)), !!fill_var)

    top_levels <- df_cat %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      tibble::rownames_to_column() %>%
      tidyr::gather(key = "variable", value = "levels", -rowname) %>%
      dplyr::select(-rowname) %>%
      dplyr::count(variable, levels) %>%
      dplyr::group_by(variable) %>%
      dplyr::arrange(dplyr::desc(n), .by_group=TRUE) %>%
      dplyr::slice_head(n = top) %>%
      dplyr::mutate(levels = tidytext::reorder_within(levels, n, variable)) %>%
      dplyr::ungroup() %>%
      dplyr::select(levels) %>%
      dplyr::pull()

    data_formatted <- df_cat %>%
      tidyr::gather(key = "variable", value = "levels", -!!fill_var) %>%
      dplyr::count(!!fill_var, variable, levels) %>%
      dplyr::group_by(variable, levels) %>%
      dplyr::arrange(dplyr::desc(n), .by_group=TRUE) %>%
      dplyr::mutate(levels = tidytext::reorder_within(levels, n, variable)) %>%
      dplyr::filter(levels %in% top_levels) %>% suppressWarnings()


    if(is.null(position)){position <- "fill"}else{position <- position}

    p1 <- data_formatted %>%
      ggplot2::ggplot(ggplot2::aes(x=stats::reorder(levels, n), y=n, fill=!!fill_var))+
      ggplot2::geom_bar(stat = "identity", alpha = alpha, position = position, width = width)+
      ggplot2::scale_fill_viridis_d() +
      ggplot2::facet_wrap(ggplot2::vars(variable), scales = scales, ncol = ncol)+
      ggplot2::coord_flip()+
      tidytext::scale_x_reordered()+
      ggplot2::labs(title = title, y = y_lab, x = x_lab)+
      ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 14, face="bold", hjust = 0),
        panel.spacing.y = ggplot2::unit(1, "lines"),
        panel.grid = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        legend.position = legend_position
      )


  } else {

    df_cat <- data %>% dplyr::ungroup() %>%
      dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

    data_formatted <- df_cat %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      tibble::rownames_to_column() %>%
      tidyr::gather(key = "variable", value = "levels", -rowname) %>%
      dplyr::select(-rowname) %>%
      dplyr::count(variable, levels) %>%
      dplyr::group_by(variable) %>%
      dplyr::arrange(dplyr::desc(n), .by_group=TRUE) %>%
      dplyr::slice_head(n = top) %>%
      dplyr::mutate(levels = tidytext::reorder_within(levels, n, variable)) %>%
      dplyr::mutate(pct = n/sum(n))

    if(is.null(position)){position <- "stack"}

    if(pct){

      if(is.null(fill)){
        p1 <- data_formatted %>%
          ggplot2::ggplot() +
          ggplot2::geom_bar(
            ggplot2::aes(x=levels, y=pct, fill=variable), width = width,
            stat="identity", alpha = alpha, position = position
          ) +
          ggplot2::scale_fill_manual(values = pal_discrete, guide="none")

      }else{
        p1 <- data_formatted %>%
          ggplot2::ggplot(ggplot2::aes(x=levels, y=pct)) +
          ggplot2::geom_bar(
            ggplot2::aes(x=levels, y=pct), fill = fill, width = width,
            stat="identity", alpha = alpha, position = position
          )
      }

    } else{
      if(is.null(fill)){
        p1 <- data_formatted %>%
          ggplot2::ggplot(ggplot2::aes(x=levels, y=n, fill=variable)) +
          ggplot2::geom_bar(
            ggplot2::aes(x=levels, y=n, fill=variable), width = width,
            stat="identity", alpha = alpha, position = position
          ) +
          ggplot2::scale_fill_manual(values = pal_discrete, guide="none")

      }else{
        p1 <- data_formatted %>%
          ggplot2::ggplot(ggplot2::aes(x=levels, y=n)) +
          ggplot2::geom_bar(
            ggplot2::aes(x=levels, y=n), fill = fill, width = width,
            stat="identity", alpha = alpha, position = position
          )
      }
    }

    p1 <- p1 +
      ggplot2::coord_flip() +
      tidytext::scale_x_reordered() +
      ggplot2::facet_wrap(dplyr::vars(variable), scales = scales, ncol = ncol) +
      ggplot2::labs(title = title, y = y_lab, x = x_lab)+
      ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 14, face="bold", hjust = 0),
        panel.spacing.y = ggplot2::unit(1, "lines"),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        legend.position = legend_position
      )

  }

  if(!interactive){
    return(p1)
  }else{
    p <- plotly::ggplotly(p1) %>%
      plotly::layout(
        showlegend = T,
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
