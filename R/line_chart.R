#' Line Chart
#'
#' Plot Line Chart in ggplot2
#'
#' @param data A data-frame or tibble
#' @param x A numeric variable
#' @param y A numeric variable
#' @param color_var A categorical variable to group and fill.
#' @param color character. Line color. Overrided if color_var is specified.
#' @param label_var A categorical variable to label.
#' @param label_color character. Label color.
#' @param label_size numeric. Size of labels.
#' @param label_hjust numeric. Horizontal justification of label.
#' @param label_vjust numeric. Vertical justification of label.
#' @param facet_var A categorical variable used to create facet panels.
#' @param ncol integer. Number of facet columns.
#' @param scales character. Control facet x & y-axis ranges. Options include "fixed", "free", "free_y", "free_x"
#' @param title character. Title for the plot.
#' @param x_lab character. X-axis label for the plot.
#' @param y_lab character. Y-axis label for the plot.
#' @param linewidth numeric. Line width.
#' @param legend_position character. Legend position. Options include "right", "left", "top", "bottom", "none".
#' @param highlight A vector of categories to highlight. Overrided if fill_var is specified.
#' @param compare logical. Whether or not to compare a group with the others.
#' @param expand_mult_x character. The expansion multiplier for the x-axis.
#' @param show_label logical. Whether or not to display labels.
#' @param check_overlap logical. Whether or not to stroke the text to overlap
#' @param interactive logical. Whether or not to return an interactive plot (plotly).
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
#'
#' @examples
#'
#' library(dplyr)
#'
#' line_chart(ggplot2::economics, x = date, y = unemploy)
#'
#' economics_long2 <-
#'   dplyr::filter(
#'     ggplot2::economics_long,
#'     variable %in% c("pop", "uempmed", "unemploy")
#'   )
#'
#' line_chart(economics_long2, x = date, y = value01, color_var = variable)
#'
#' line_chart(economics_long2, x = date, y = value01, color_var = variable,
#'            highlight = "unemploy")
#'
#' line_chart(economics_long2, x = date, y = value01, color_var = variable,
#'            facet_var = variable)
#'
#' line_chart(economics_long2, x = date, y = value01, color_var = variable,
#'            facet_var = variable,  scales = "fixed")
#'
#' line_chart(economics_long2, x = date, y = value01, color_var = variable,
#'            facet_var = variable, compare = TRUE)
#'
#' @export
line_chart <- function(data, x, y,
                       color_var = NULL,
                       color = "#4E79A7",
                       label_var = NULL,
                       label_color = NULL,
                       label_size = 3,
                       label_hjust = -0.1,
                       label_vjust = 0.5,
                       facet_var = NULL,
                       ncol = 2,
                       scales = "free_y",
                       title = NULL,
                       x_lab = NULL,
                       y_lab = NULL,
                       linewidth = 1,
                       legend_position = "none",
                       highlight = NULL,
                       compare = FALSE,
                       expand_mult_x = c(0.04, 0.20),
                       check_overlap = FALSE,
                       interactive = FALSE,
                       show_label = TRUE){

  x  <- rlang::enquo(x)
  y  <- rlang::enquo(y)
  color_var  <- rlang::enquo(color_var)
  label_var  <- rlang::enquo(label_var)
  facet_var  <- rlang::enquo(facet_var)

  x_name <- rlang::quo_name(x)
  y_name <- rlang::quo_name(y)

  if (!is.data.frame(data)) {
    stop(call. = FALSE, ".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
  }

  if (rlang::quo_is_missing(x)) {
    stop(call. = FALSE, "line_chart(x) is missing. Please supply a date or date-time column.")
  }

  if (rlang::quo_is_missing(y)) {
    stop(call. = FALSE, "line_chart(y) is missing. Please supply a numeric column.")
  }


  type_x <- class(data %>% dplyr::select(!!x) %>% dplyr::pull())[1]

  pal_discrete <-
    rev(rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 10`$value, 10))


  # color var
  if (!rlang::quo_is_null(color_var)){

    color_var_ch <- data %>% dplyr::select(!!color_var) %>%
      dplyr::select(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x)) )

    if (length(color_var_ch)==0){
      stop(call. = FALSE, "Please supply a color_var with a categorical column")
    }

    if (!is.null(highlight)){

      tmp <- data %>%
        dplyr::filter(!!color_var %in% highlight)

      p1 <-
        ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y, color = !!color_var)) +
        ggplot2::geom_line(ggplot2::aes(group = !!color_var), color = "lightgrey", linewidth = linewidth) +
        ggplot2::geom_line(data = tmp, linewidth = linewidth) +
        ggplot2::scale_color_manual(values = rev(pal_discrete))

    } else{

      p1 <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y, color = !!color_var)) +
        ggplot2::geom_line(linewidth = linewidth) +
        ggplot2::scale_color_manual(values = rev(pal_discrete))
    }


    ## Facet
    if (!rlang::quo_is_null(facet_var)){

      facet_var_ch <- data %>% dplyr::select(!!facet_var) %>%
        dplyr::select(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x)) )

      if (length(facet_var_ch)==0){
        stop(call. = FALSE, "Please supply a facet_var with a categorical column")
      }

      if (compare){

        tmp <- data %>%
          dplyr::mutate(color_var_ = !!color_var) %>%
          dplyr::select(-!!color_var)

        p1 <-
          ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y, color = !!color_var)) +
          ggplot2::geom_line(data = tmp, ggplot2::aes(group = color_var_), color = "lightgrey") +
          ggplot2::geom_line(linewidth = linewidth) +
          ggplot2::scale_color_manual(values = rev(pal_discrete)) +
          ggplot2::facet_wrap(dplyr::vars(!!facet_var), scales = scales, ncol = ncol)

      } else{

        p1 <- p1 +
          ggplot2::facet_wrap(dplyr::vars(!!facet_var), scales = scales, ncol = ncol)
      }

    }

    ## Label
    if (!rlang::quo_is_null(label_var)){

      if(!is.null(highlight)){
        data_h <- data %>%
          dplyr::filter(!!color_var %in% highlight)
      }else{
        data_h <- data
      }

      if(show_label){

        if(is.null(label_color)){
          p1 <- p1 +
            ggplot2::geom_text(
              data = data_h,
              ggplot2::aes(label = !!label_var),
              fontface = "bold", size = label_size, hjust = label_hjust, vjust = label_vjust,
              check_overlap = check_overlap
            )
        }else{
          p1 <- p1 +
            ggplot2::geom_text(
              data = data_h,
              ggplot2::aes(label = !!label_var), color = label_color,
              fontface = "bold", size = label_size, hjust = label_hjust, vjust = label_vjust,
              check_overlap = check_overlap
            )
        }

      }


    } else{

      if (!rlang::quo_is_null(facet_var)){

        facet_var_ch <- data %>% dplyr::select(!!facet_var) %>%
          dplyr::select(is.character | is.factor | is.ordered)

        if (length(facet_var_ch)==0){
          stop(call. = FALSE, "Please supply a facet_var with a categorical column")
        }

        data_label <- data %>%
          dplyr::mutate(label = ifelse(!!x == max(!!x), as.character(!!color_var), NA_character_)) %>%
          dplyr::filter(label != "NA") %>%
          dplyr::mutate(
            y_abr = ifelse(
              abs(!!y) >= 1e6, paste0(round(!!y/1e6, 1), "M"),
              ifelse(abs(!!y) >= 1e3, paste0(round(!!y/1e3, 1), "K"), round(!!y,2))
            ),
            label = paste("\n ", y_abr)
          )

      } else{
        data_label <- data %>%
          dplyr::mutate(label = ifelse(!!x == max(!!x), as.character(!!color_var), NA_character_)) %>%
          dplyr::filter(label != "NA") %>%
          dplyr::mutate(
            y_abr = ifelse(
              abs(!!y) >= 1e6, paste0(round(!!y/1e6, 1), "M"),
              ifelse(abs(!!y) >= 1e3, paste0(round(!!y/1e3, 1), "K"), round(!!y,2))
            ),
            label = paste(as.character(!!color_var),"\n ", y_abr)
          )
      }

      if (!is.null(highlight)){

        tmp <- data %>%
          dplyr::filter(!!color_var %in% highlight)

        if (!rlang::quo_is_null(facet_var)){

          facet_var_ch <- data %>% dplyr::select(!!facet_var) %>%
            dplyr::select(is.character | is.factor | is.ordered)

          if (length(facet_var_ch)==0){
            stop(call. = FALSE, "Please supply a facet_var with a categorical column")
          }

          data_label <- tmp %>%
            dplyr::mutate(label = ifelse(!!x == max(!!x), as.character(!!color_var), NA_character_)) %>%
            dplyr::filter(label != "NA") %>%
            dplyr::mutate(
              y_abr = ifelse(
                abs(!!y) >= 1e6, paste0(round(!!y/1e6, 1), "M"),
                ifelse(abs(!!y) >= 1e3, paste0(round(!!y/1e3, 1), "K"), round(!!y,2))
              ),
              label = paste("\n ", y_abr)
            )
        }else{
          data_label <- tmp %>%
            dplyr::mutate(label = ifelse(!!x == max(!!x), as.character(!!color_var), NA_character_)) %>%
            dplyr::filter(label != "NA") %>%
            dplyr::mutate(
              y_abr = ifelse(
                abs(!!y) >= 1e6, paste0(round(!!y/1e6, 1), "M"),
                ifelse(abs(!!y) >= 1e3, paste0(round(!!y/1e3, 1), "K"), round(!!y,2))
              ),
              label = paste(as.character(!!color_var),"\n ", y_abr)
            )
        }

        if(show_label){
          if(is.null(label_color)){
            p1 <- p1 +
              ggplot2::geom_text(
                data = data_label, #highlight
                ggplot2::aes(label = label),
                fontface = "bold", size = label_size, hjust = label_hjust, vjust = label_vjust,
                check_overlap = check_overlap
              )
          }else{
            p1 <- p1 +
              ggplot2::geom_text(
                data = data_label, #highlight
                ggplot2::aes(label = label), color = label_color,
                fontface = "bold", size = label_size, hjust = label_hjust, vjust = label_vjust,
                check_overlap = check_overlap
              )
          }
        }


      } else{

        if(show_label){
          if(is.null(label_color)){
            p1 <- p1 +
              ggplot2::geom_text(
                data = data_label,
                ggplot2::aes(label = label),
                fontface = "bold", size = label_size, hjust = label_hjust, vjust = label_vjust,
                check_overlap = check_overlap
              )
          }else{
            p1 <- p1 +
              ggplot2::geom_text(
                data = data_label,
                ggplot2::aes(label = label), color = label_color,
                fontface = "bold", size = label_size, hjust = label_hjust, vjust = label_vjust,
                check_overlap = check_overlap
              )
          }
        }

      }

    }

    if(interactive){
      p1 <- p1
    }else{
      p1 <- p1 +
        ggplot2::guides(
          color = ggplot2::guide_legend(
            override.aes = ggplot2::aes(label = "")
          )
        ) #remove letter "a" in legend

    }


    # Not color_var
  } else{

    if (!rlang::quo_is_null(label_var)){
      p1 <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y)) +
        ggplot2::geom_line(linewidth = linewidth, color = color)

      if(show_label){

        if(is.null(label_color)){
          p1 <- p1 +
            ggplot2::geom_text(
              ggplot2::aes(label = !!label_var), color = "black",
              fontface = "bold", size = label_size, hjust = label_hjust, vjust = label_vjust,
              check_overlap = check_overlap
            )
        }else{
          p1 <- p1 +
            ggplot2::geom_text(
              ggplot2::aes(label = !!label_var), color = label_color,
              fontface = "bold", size = label_size, hjust = label_hjust, vjust = label_vjust,
              check_overlap = check_overlap
            )
        }

      }


    } else{

      p1 <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y)) +
        ggplot2::geom_line(linewidth = linewidth, color = color)

      if(show_label){

        if (!rlang::quo_is_null(facet_var)){

          facet_var_ch <- data %>% dplyr::select(!!facet_var) %>%
            dplyr::select(is.character | is.factor | is.ordered)

          if (length(facet_var_ch)==0){
            stop(call. = FALSE, "Please supply a facet_var with a categorical column")
          }

          data_label <- data %>%
            dplyr::mutate(label = ifelse(!!x == max(!!x), as.character(!!facet_var), NA_character_)) %>%
            dplyr::filter(label != "NA") %>%
            dplyr::mutate(
              y_abr = ifelse(
                abs(!!y) >= 1e6, paste0(round(!!y/1e6, 1), "M"),
                ifelse(abs(!!y) >= 1e3, paste0(round(!!y/1e3, 1), "K"), round(!!y,2))
              ),
              label = paste("\n ", y_abr)
            )
        }else{
          data_label <- data %>%
            dplyr::mutate(label = ifelse(!!x == max(!!x), rlang::as_name(y), NA_character_)) %>%
            dplyr::filter(label != "NA") %>%
            dplyr::mutate(
              y_abr = ifelse(
                abs(!!y) >= 1e6, paste0(round(!!y/1e6, 1), "M"),
                ifelse(abs(!!y) >= 1e3, paste0(round(!!y/1e3, 1), "K"), round(!!y,0))
              ),
              label = paste(rlang::as_name(y), "\n ", y_abr)
            )
        }

        if(is.null(label_color)){
          p1 <- p1 +
            ggplot2::geom_text(
              data = data_label,
              ggplot2::aes(x = !!x, y = !!y, label = label), color = "black",
              fontface = "bold", size = label_size, hjust = label_hjust, vjust = label_vjust,
              check_overlap = check_overlap
            )
        }else{
          p1 <- p1 +
            ggplot2::geom_text(
              data = data_label,
              ggplot2::aes(x = !!x, y = !!y, label = label), color = label_color,
              fontface = "bold", size = label_size, hjust = label_hjust, vjust = label_vjust,
              check_overlap = check_overlap
            )
        }

      }

    }


    ## Facet
    if (!rlang::quo_is_null(facet_var)){

      facet_var_ch <- data %>% dplyr::select(!!facet_var) %>%
        dplyr::select(is.character | is.factor | is.ordered)

      if (length(facet_var_ch)==0){
        stop(call. = FALSE, "Please supply a var_x with a categorical column")
      }

      p1 <- p1 +
        ggplot2::facet_wrap(dplyr::vars(!!facet_var), scales = scales, ncol = ncol)

    }

  }

  # plot

  min = data %>% dplyr::select(!!x) %>% dplyr::pull() %>% min()
  max = data %>% dplyr::select(!!x) %>% dplyr::pull() %>% max()

  if(show_label){
    expand_mult_x <- expand_mult_x
  }else{
    expand_mult_x <- c(0.04, 0.04)
  }

  if(type_x == "Date"){

    x_breaks <- seq.Date(min, max, length.out = 5)

    p1 <- p1 +
      ggplot2::scale_x_date(
        expand = ggplot2::expansion(mult = expand_mult_x),
        breaks = x_breaks,
        date_labels = "%Y"
      )
  }else{

    x_breaks <- round(seq(min, max, length.out = 4),0)

    p1 <- p1 +
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = expand_mult_x),
        breaks = x_breaks
      )
  }


  if(is.null(x_lab)){x_lab <- x_name}else{x_lab <- x_lab}
  if(is.null(y_lab)){y_lab <- y_name}else{y_lab <- y_lab}


  p1 <- p1 +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::labs(title = title, x = x_lab, y = y_lab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 14, face="bold", hjust = 0,
        margin = ggplot2::margin(b=10)
      ),
      legend.position = legend_position,
      legend.margin = ggplot2::margin(0, 0, 0, 0),
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

