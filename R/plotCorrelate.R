#' Explore correlations of numeric columns with bar charts.
#'
#' Explore correlations of numeric columns with bar charts.
#'
#' @param data A data frame or tibble
#' @param method character. Correlation coefficient method. Options include "pearson", "kendall" or "spearman".
#' @param width numeric. Width of the bars.
#' @param label_show logical. Whether or not to display labels.
#' @param label_color character. The color of the labels.
#' @param label_size character. The size of the labels.
#' @param palette_gradient character. Divergent color palette. Color palettes used in Tableau (ggthemes).
#' @param title character. Title for the plot.
#' @param x_lab character. X-axis label for the plot.
#' @param y_lab character. Y-axis label for the plot.
#' @param expand_mult A numeric vector of length 2 indicating the expansion multiplier for the x-axis.
#' @param interactive logical. Whether or not to return an interactive plot (plotly).
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
#'
#' @examples
#'
#' plotCorrelate(iris)
#'
#' @importFrom stats cor
#' @export
plotCorrelate <- function(data,
                          method = "pearson",
                          width = 0.8,
                          label_show = TRUE,
                          label_size = 3.1,
                          label_color = "grey20",
                          palette_gradient = "Red-Green Diverging",
                          title = NULL,
                          x_lab = NULL,
                          y_lab = NULL,
                          expand_mult = c(0.20, 0.20),
                          interactive = FALSE){

  if (!is.data.frame(data)) {
    stop(call. = FALSE, "data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  df_num <- data %>% dplyr::ungroup() %>% dplyr::select(c(tidyselect::where(~ is.numeric(.x))))

  if (length(df_num)==0){
    stop(call. = FALSE, "Please supply a data-frame or tibble with a numeric column")
  }

  matrix_cor <- stats::cor(
    x = df_num,
    method = method,
    use = "pairwise.complete.obs"
  ) %>% round(3)

  matrix_cor[lower.tri(matrix_cor, diag = TRUE)] <- ""

  df_cor <- matrix_cor %>% dplyr::as_tibble() %>% dplyr::mutate_if(is.character, as.numeric)

  df <- df_cor %>%
    dplyr::mutate(var1 = names(df_cor)) %>%
    tidyr::gather(key = "var2", value = "correlation", -var1) %>%
    dplyr::filter(correlation != is.na(correlation)) %>%
    dplyr::mutate(
      correlation = round(correlation, 2),
      variables = paste(var1, "-", var2)
    ) %>%
    dplyr::arrange(dplyr::desc(correlation)) %>%
    dplyr::mutate(variables = stats::reorder(variables, correlation))

  p1 <-
    ggplot2::ggplot(
      df, ggplot2::aes(x = correlation, y = variables, fill = correlation)
    ) +
    ggplot2::geom_bar(stat = "identity", width = width)

  if(label_show){
    p1 <- p1 +
      ggplot2::geom_text(
        ggplot2::aes(label = correlation, hjust = dplyr::if_else(correlation < 0, 1.25, -0.25)),
        size = label_size, color = label_color
      )

  }

  p1 <- p1 +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = expand_mult)) +
    ggthemes::scale_fill_gradient2_tableau(palette = palette_gradient, guide = "none") +
    ggplot2::labs(title = title, y = y_lab, x = x_lab)+
    ggplot2::theme_minimal()+
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 14, face="bold", hjust = 0, margin = ggplot2::margin(b=10)
      )
    )

  if(!interactive){
    return(p1)
  }else{
    p <- plotly::ggplotly(p1, tooltip = c("fill")) %>%
      plotly::layout(
        showlegend = T,
        font = list(family = "Segoe UI"),
        hoverlabel = list(
          font = list(
            color = "white",
            family = "Consolas"
          ),
          bordercolor = "transparent"
        )
      )

    return(p)
  }

}
