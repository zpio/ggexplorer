#' Bar Chart
#'
#' Plot a Bar Chart in ggplot2
#'
#' @param data A data-frame or tibble.
#' @param x A categorical variable.
#' @param y A numeric variable when `stat = "count"`.
#' @param stat character. How to calculate bar heights. Options include "count" or "identity".
#' @param width numeric. Width of bars.
#' @param prop logical. Whether or not to display proportions instead of counts. Overrided if stat = "identity".
#' @param top integer. Top n levels.
#' @param fill character. Color of the bars. Overrided if fill_var is specified.
#' @param fill_var A categorical variable to group and fill.
#' @param facet_var A categorical variable used to create facet panels.
#' @param scales character. control facet x & y-axis ranges. Options include "fixed", "free", "free_y", "free_x"
#' @param ncol integer. Number of facet columns.
#' @param position character. Position adjustment. Options include "stack", fill" or "dodge".
#' @param flip logical. whether or not to flip the chart horizontally.
#' @param legend_position A character indicating legend position. Options include "right", "left", "top", "bottom", "none".
#' @param title character. Title for the plot.
#' @param x_lab character. X-axis label for the plot.
#' @param y_lab character. Y-axis label for the plot.
#' @param highlight A vector of categories to highlight. Overrided if fill_var is specified.
#' @param palette_gradient character. Color palette when fill_var is a numeric variable.
#' @param label_var The categorical or numeric variable used to label the bars.
#' @param label_color character. Label color.
#' @param label_size numeric. Label Size.
#' @param expand_mult character. The expansion multiplier for the y-axis.
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
#'
#' @examples
#' bar_chart(ggplot2::diamonds, x = clarity)
#'
#' bar_chart(ggplot2::diamonds, x = clarity, prop = TRUE)
#'
#' bar_chart(ggplot2::diamonds, x = clarity, flip = TRUE)
#'
#' bar_chart(ggplot2::diamonds, x = clarity, top = 5)
#'
#' bar_chart(ggplot2::diamonds, x = clarity, fill_var = cut, flip = TRUE)
#'
#' bar_chart(ggplot2::diamonds, x = clarity, fill_var = cut, flip = TRUE,
#'           position = "stack")
#'
#' bar_chart(ggplot2::diamonds, x = clarity, fill_var = cut,
#'           position = "fill", flip = TRUE)
#'
#' bar_chart(ggplot2::diamonds, x = clarity, fill_var = cut, position = "fill",
#'           flip = TRUE, facet_var = color, legend_position = "bottom")
#'
#' @importFrom rlang :=
#' @export
bar_chart <- function(data,
                      x,
                      y = NULL,
                      stat = "count",
                      width = 0.7,
                      prop = FALSE,
                      top = NULL,
                      fill = "#4E79A7",
                      fill_var = NULL,
                      facet_var = NULL,
                      scales = "free",
                      ncol = 3,
                      position = "dodge",
                      flip = FALSE,
                      legend_position = "right",
                      title = NULL,
                      x_lab = NULL,
                      y_lab = NULL,
                      highlight = NULL,
                      palette_gradient = 'Green-Gold',
                      label_var = NULL,
                      label_color = "gray20",
                      label_size = 3.1,
                      expand_mult = c(0.05, 0.10)) {

  x  <- rlang::enquo(x)
  y  <- rlang::enquo(y)
  fill_var  <- rlang::enquo(fill_var)
  facet_var  <- rlang::enquo(facet_var)
  label_var  <- rlang::enquo(label_var)

  x_name <- rlang::quo_name(x)
  y_name <- rlang::quo_name(y)

  x_cat <- data %>% dplyr::ungroup() %>% dplyr::select(!!x) %>%
    dplyr::select(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x)) )

  if (length(x_cat)==0){
    stop(call. = FALSE, "bar_chart(x) is missing. Please supply a categorical column.")
  }

  fill_var_cat <- data %>% dplyr::ungroup() %>% dplyr::select(!!fill_var) %>%
    dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

  fill_var_num <- data %>% dplyr::ungroup() %>% dplyr::select(!!fill_var) %>%
    dplyr::select(c(tidyselect::where(~ is.numeric(.x))))

  pal_discrete <- rev(rep(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Classic Cyclic`$value, 10))

  if(stat == "count"){

    data <- data %>% dplyr::ungroup()

    if(!is.null(top)){
      top_n <- data %>%
        dplyr::count(!!x) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        dplyr::slice_head(n = top) %>%
        dplyr::select(!!x) %>%
        dplyr::pull()

      data <- data %>% dplyr::filter(!!x %in% top_n)
    }

    if(rlang::quo_is_null(fill_var)){

      if(!is.null(highlight)){
        data <- data %>%
          dplyr::mutate(Highlight = ifelse(!!x %in% highlight, "yes", "no"))
      }

      if(flip){

        p1 <- ggplot2::ggplot(data, ggplot2::aes(x = stats::reorder(!!x, table(!!x)[!!x])) )+
          ggplot2::coord_flip()

      }else{

        p1 <- ggplot2::ggplot(data, ggplot2::aes(x = stats::reorder(!!x, -table(!!x)[!!x])) )

      }

      if(prop){
        p1 <- p1 +
          ggplot2::geom_bar(
            ggplot2::aes(y = ggplot2::after_stat(count/sum(count))),
            stat = stat, width = width, fill = fill
          )
      }else{
        p1 <- p1 +
          ggplot2::geom_bar(
            ggplot2::aes(y = ggplot2::after_stat(count)),
            stat = stat, width = width, fill = fill
          )
      }

      if(!is.null(highlight)){
        p1 <- p1 +
          ggplot2::geom_bar(ggplot2::aes(fill = Highlight), stat = stat, width = width) +
          ggplot2::scale_fill_manual(
            values = c("yes" = fill, "no" = "lightgray"),
            guide = "none"
          )
      }


    }else{


      if(flip){

        p1 <- ggplot2::ggplot(
          data, ggplot2::aes(x = stats::reorder(!!x, table(!!x)[!!x]), fill = !!fill_var)
        ) +
          ggplot2::coord_flip()

      }else{

        p1 <- ggplot2::ggplot(
          data, ggplot2::aes(x = stats::reorder(!!x, -table(!!x)[!!x]), fill = !!fill_var)
        )

      }

      p1 <- p1 +
        ggplot2::geom_bar(stat = stat, width = width, position = position) +
        ggplot2::scale_fill_manual(values = rev(pal_discrete))

    }

  }else if(stat == "identity"){

    data <- data %>% dplyr::ungroup()

    y_num <- data %>% dplyr::select(!!y) %>%
      dplyr::select(c(tidyselect::where(~ is.numeric(.x))))

    if (length(y_num)==0){
      stop(call. = FALSE, "bar_chart(y) is missing. Please supply a numeric column.")
    }

    if(!is.null(top)){
      top_n <- data %>%
        dplyr::group_by(!!x) %>%
        dplyr::summarise(sum = sum(!!y)) %>%
        dplyr::arrange(dplyr::desc(sum)) %>%
        dplyr::slice_head(n = top) %>%
        dplyr::select(!!x) %>%
        dplyr::pull()

      data <- data %>% filter(!!x %in% top_n)
    }

    if(!rlang::quo_is_null(facet_var)){
      data <- data %>%
        dplyr::group_by(!!facet_var) %>%
        dplyr::arrange( dplyr::desc(!!y), .by_group = TRUE) %>%
        dplyr::mutate(!!x := tidytext::reorder_within(!!x, !!y, !!facet_var))
    }

    if(rlang::quo_is_null(fill_var)){

      if(!is.null(highlight)){
        data <- data %>%
          dplyr::mutate(Highlight = ifelse(!!x %in% highlight, "yes", "no"))
      }

      if(flip){

        p1 <- ggplot2::ggplot(data, ggplot2::aes(x = stats::reorder(!!x, !!y), y = !!y) ) +
          ggplot2::coord_flip()

      }else{

        p1 <- ggplot2::ggplot(data, ggplot2::aes(x = stats::reorder(!!x, -!!y), y = !!y) )

      }

      if(is.null(highlight)){

        p1 <- p1 +
          ggplot2::geom_bar(stat = stat, width = width, fill = fill)

      }else{

        p1 <- p1 +
          ggplot2::geom_bar(ggplot2::aes(fill = Highlight), stat = stat, width = width) +
          ggplot2::scale_fill_manual(
            values = c("yes" = fill, "no" = "lightgray"),
            guide = "none"
          )
      }


    }else{

      fill_var_cat <- data %>% dplyr::ungroup() %>% dplyr::select(!!fill_var) %>%
        dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

      fill_var_num <- data %>% dplyr::ungroup() %>% dplyr::select(!!fill_var) %>%
        dplyr::select(c(tidyselect::where(~ is.numeric(.x))))


      if(flip){

        p1 <- ggplot2::ggplot(
          data, ggplot2::aes(x = stats::reorder(!!x, !!y), y = !!y, fill = !!fill_var)
        ) +
          ggplot2::coord_flip()

      }else{

        p1 <- ggplot2::ggplot(
          data, ggplot2::aes(x = stats::reorder(!!x, -!!y), y = !!y, fill = !!fill_var)
        )

      }

      if (length(fill_var_cat)==1){

        p1 <- p1 +
          ggplot2::geom_bar(stat = stat, width = width, position = position) +
          ggplot2::scale_fill_manual(values = rev(pal_discrete))

      }else if(length(fill_var_num)==1){

        p1 <- p1 +
          ggplot2::geom_bar(stat = stat, width = width, position = position) +
          ggthemes::scale_fill_gradient_tableau(palette = palette_gradient)

      }else{

        stop(call. = FALSE, "Please supply fill_var with a categorical or numeric column")

      }

    }

    # label

    if(!rlang::quo_is_null(label_var)){

      label_var_cat <- data %>% dplyr::ungroup() %>% dplyr::select(!!label_var) %>%
        dplyr::select(c(tidyselect::where(~ is.character(.x)|is.factor(.x)|is.ordered(.x))))

      label_var_num <- data %>% dplyr::ungroup() %>% dplyr::select(!!label_var) %>%
        dplyr::select(c(tidyselect::where(~ is.numeric(.x))))

      if(flip){

        if(position == "dodge"){
          label_position = ggplot2::position_dodge(width = width)
          label_hjust = ifelse(data %>% dplyr::select(!!y) < 0, 1.10, -0.15)
        }else if(position == "stack"){
          label_position = ggplot2::position_stack(vjust = 0.5)
          label_hjust = 0.5
        }else if(position == "fill"){
          label_position = ggplot2::position_fill(vjust = 0.5)
          label_hjust = 0.5
        }

        if(length(label_var_cat)==1){

          p1 <- p1 +
            ggplot2::geom_text(
              ggplot2::aes(
                label= !!label_var,
                hjust = label_hjust
              ),
              position = label_position,
              color = label_color, size = label_size
            )

        }else if(length(label_var_num)==1){
          p1 <- p1 +
            ggplot2::geom_text(
              ggplot2::aes(
                label= ifelse(
                  abs(!!label_var) >= 1e6, paste0(round(!!label_var/1e6, 1), "M"),
                  ifelse(abs(!!label_var) >= 1e3, paste0(round(!!label_var/1e3, 1), "K"), round(!!label_var,3))
                ),
                hjust = label_hjust
              ),
              position = label_position,
              color = label_color, size = label_size
            )
        }else{
          stop(call. = FALSE, "Please supply label_var with a categorical or numeric column")
        }

      }else{

        if(position == "dodge"){
          label_position = ggplot2::position_dodge(width = width)
          label_vjust = ifelse(data %>% dplyr::select(!!y) < 0, 1.5, -0.5)
        }else if(position == "stack"){
          label_position = ggplot2::position_stack(vjust = 0.5)
          label_vjust = 0.5
        }else if(position == "fill"){
          label_position = ggplot2::position_fill(vjust = 0.5)
          label_vjust = 0.5
        }

        if(length(label_var_cat)==1){
          p1 <- p1 +
            ggplot2::geom_text(
              ggplot2::aes(
                label = !!label_var,
                vjust = label_vjust
              ),
              position = label_position,
              color = label_color, size = label_size
            )
        }else if(length(label_var_num)==1){
          p1 <- p1 +
            ggplot2::geom_text(
              ggplot2::aes(
                label= ifelse(
                  abs(!!label_var) >= 1e6, paste0(round(!!label_var/1e6, 1), "M"),
                  ifelse(abs(!!label_var) >= 1e3, paste0(round(!!label_var/1e3, 1), "K"), round(!!label_var,3))
                ),
                vjust = label_vjust
              ),
              position = label_position,
              color = label_color, size = label_size
            )
        }else{
          stop(call. = FALSE, "Please supply label_var with a categorical or numeric column")
        }

      }


    }


  }else{

    stop(call. = FALSE, "bar_chart(stat) is missing. Please supply 'identity' or 'count'")

  }

  # facet
  if(!rlang::quo_is_null(facet_var)){

    p1 <- p1 + ggplot2::facet_wrap(dplyr::vars(!!facet_var), scales = scales, ncol = ncol) +
      tidytext::scale_x_reordered()

  }

  # scale

  p1 <- p1 +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      expand = ggplot2::expansion(mult = expand_mult)
    )

  if(stat == "identity"){
    if(is.null(x_lab)){x_lab <- x_name}
    if(is.null(y_lab)){y_lab <- y_name}
  }

  if(stat == "count"){
    if(is.null(x_lab)){x_lab <- x_name}
    if(is.null(y_lab)){y_lab <- "count"}
  }


  # theme
  p1 <- p1 +
    ggplot2::labs(title = title, x = x_lab, y = y_lab ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 14, face="bold", hjust = 0,
        margin = ggplot2::margin(b=10)
      ),
      legend.position = legend_position
    )

  if(flip){

    p1 <- p1 +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank()
      )

  }else{

    p1 <- p1 +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank()
      )

  }

  return(p1)

}

