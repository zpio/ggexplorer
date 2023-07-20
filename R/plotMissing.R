#' Explore Missing values
#'
#' Explore Missing values
#'
#' @param data A data frame or tibble.
#' @param fill character. Color of the bars. Overrided if fill_var is specified.
#' @param width numeric. Width of the bars.
#' @param prop logical. Whether or not to display proportions instead of counts.
#' @param title character. Title for the plot.
#' @param x_lab character. X-axis label for the plot.
#' @param y_lab character. Y-axis label for the plot.
#' @param expand_mult_x character. A numeric vector of length 2 indicating the expansion multiplier for the x-axis.
#'
#' @return A static `ggplot2` plot
#'
#' @examples
#'
#' plotMissing(iris)
#'
#' @export
plotMissing <- function(data,
                        fill = "steelblue",
                        width = 0.7,
                        prop = TRUE,
                        title = NULL,
                        x_lab = NULL,
                        y_lab = NULL,
                        expand_mult_x = c(0, 0.15)) {

  if (!is.data.frame(data)) {
    stop(call. = FALSE, "data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  dfDescription <- data %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        list(
          type = ~ class(.x)[1],
          n = ~ length(.x),
          NAs = ~ sum(is.na(.x) | as.character(.x)=="" | as.character(.x)==" "),
          NAsPct = ~ round(sum(is.na(.x) | as.character(.x)=="" | as.character(.x)==" ")/length(.x),3),
          unique = ~ length(unique(.x)),
          uniquePct = ~ round(length(unique(.x))/length(.x),3)
        ),
        .names = "{.col}%%{.fn}"
      )
    ) %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(
      !c(rowname),
      names_to = c("set", ".value"),
      names_pattern = "(.*)%%(.*)"
    ) %>%
    dplyr::select(variable = set, dplyr::everything(), -rowname) %>%
    dplyr::arrange(NAsPct) %>%
    dplyr::mutate(
      variable = stats::reorder(variable, -NAsPct),
      pct = sprintf("%.2f%%", NAsPct*100)
    )

  if(is.null(x_lab)){x_lab <- "Missing Rows"}


  if(prop){
    p1 <-
      ggplot2::ggplot(
        data = dfDescription,
        ggplot2::aes(x = NAsPct, y = stats::reorder(variable, NAsPct))
      ) +
      ggplot2::scale_x_continuous(
        labels = scales::percent,
        expand = ggplot2::expansion(mult = expand_mult_x)
      )

  }else{
    p1 <-
      ggplot2::ggplot(
        data = dfDescription,
        ggplot2::aes(x = NAs, y = stats::reorder(variable, NAs))
      ) +
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = expand_mult_x)
      )


  }

  p1 <- p1 + ggplot2::geom_bar(stat = "identity", width = width, fill = fill)


  p1 <- p1 +
    ggplot2::labs(title = title, x = x_lab, y = y_lab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 9),
      plot.title = ggplot2::element_text(
        size = 14, face="bold", hjust = 0, margin = ggplot2::margin(b=10)
      )
    )

  if(prop){
    p1 <- p1 +
      ggplot2::geom_text(
        ggplot2::aes(label = pct),
        hjust = -0.4, colour = "gray20", size=3.5
      )
  }else{
    p1 <- p1 +
      ggplot2::geom_text(
        ggplot2::aes(label = NAs),
        hjust = -0.4, colour = "gray20", size=3.5
      )
  }

  return(p1)

}
