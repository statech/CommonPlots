#' Bar plots
#'
#' Creates a bar plot with vertical or horizontal bars
#'
#' This function relies on \href{http://ggplot2.org/}{ggplot2} package to
#' create a bar plot with vertical or horizontal bars. The function supports
#' panel plot layout. Counts information can be added to the corresponding
#' bars. By default black-and-white theme is used.
#'
#' @param data Data frame: default dataset to use for plot
#' @param var Character: name of a \code{data} column whose counts provided as
#'  the height of bars
#' @param var_levels Vector/List: a named vector/list that specifies the
#'  levels and labels of \code{var}
#' @param group Character: name of a \code{data} column mapped to the fill of
#'  bars
#' @param group_levels Vector/List: a named vector/list that specifies the
#'  levels and labels of \code{group}
#' @param facet_r Character: name of a \code{data} column mapped to the facet
#'  row in panel plot layout. Check \code{\link[ggplot2]{facet_grid}} for more
#'  details
#' @param facet_c Character: name of a \code{data} column mapped to the facet
#'  column in panel plot layout. Check \code{\link[ggplot2]{facet_grid}} for
#'  more details
#' @param facet_r_levels Vector/List: a named vector/list that specifies the
#'  levels and labels of \code{facet_r}
#' @param facet_c_levels Vector/List: a named vector/list that specifies the
#'  levels and labels of \code{facet_c}
#' @param facet_scale Character: Are scales shared across all facets. Refer to
#'  the `scale` argument in \code{\link[ggplot2]{facet_grid}}. Default `free`
#'  means that scales are not shared
#' @param facet_space Character: Refer to the `space` argument in
#'  \code{\link[ggplot2]{facet_grid}}. Default `free` means both height and
#'  width will vary
#' @param x_lab Character: x-axis label
#' @param y_lab Character: y-axis label
#' @param group_lab Character: group variable label
#' @param title Character: barplot title
#' @param add_counts Logical: If `TRUE` (default), counts will be added to the
#'  bars
#' @param counts_pos Character: position to add counts. If `inside-bar`
#'  (default), counts will be inside the bars; if `outside-bar`, counts be
#'  placed outside the bars. When \code{group} is present and
#'  \code{group_bar_position} is specified as `stack`, counts will be placed
#'  in the center of the bars
#' @param bar_label_align Character: alignment of the bar labels. If `left`
#'  (default), labels will be left-aligned; if `center`, labels will be
#'  center-aligned; if `right`, labels will be right-aligned
#' @param bar_label_angle Numeric: the orientation angle (in [0, 360]) of the
#'  bar labels on the axis. By default, the label will be perpendicular to the
#'  axis
#' @param group_bar_position Character: position adjustment for bars of same
#'  group when \code{group} is present. If `dodge` (default), bars of same
#'  group are placed side by side; if `stack`, bars of same group are stacked
#'  one on top of another.
#' @param grids Character: control the grids. If `on` (default), grids will be
#'  drawn; if `x`, only grids on x-axis will be drawn; if `y`, only grids on
#'  y-axis will be drawn; if `off`, no grids will be drawn
#' @param bw_theme Logical: If `TRUE` (default), black-and-white theme will be
#'  used. Refer to \code{\link[ggplot2]{theme_bw}} for more details
#' @param horizontal Logical: If `TRUE` (default), horizontal barplot is drawn;
#'  if `FALSE`, vertical barplot is drawn
#'
#' @return An object of class ggplot. Can be directly sent to plot with
#'  \code{\link{print}}
#'
#' @examples
#' gg_barplot(mpg, var = 'class', x_lab = '', horizontal = FALSE)
#' gg_barplot(mpg, var = 'class', group = 'cyl', group_lab = 'Cylinders')
#' var_levels <- c('Mid-size' = 'midsize', 'Minivan' = 'minivan', 'SUV' = 'suv')
#' gg_barplot(mpg, var = 'class', var_levels = var_levels,
#'            group = 'cyl', group_lab = 'Cylinders', add_counts = FALSE)
#' gg_barplot(mpg, var = 'class', group = 'cyl', group_bar_position = 'stack')
#'
#' @import dplyr
#' @export
#'
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_barplot <- function(data, var, var_levels = NULL,
                       group = NULL, group_levels = NULL,
                       facet_r = NULL, facet_c = NULL,
                       facet_r_levels = NULL, facet_c_levels = NULL,
                       facet_scale = 'free', facet_space = 'free',
                       x_lab = NULL, y_lab = NULL, group_lab = group,
                       title = NULL, add_counts = TRUE, counts_pos = 'inside-bar',
                       bar_label_align = 'left', bar_label_angle = NULL,
                       group_bar_position = 'dodge', grids = 'on',
                       bw_theme = TRUE, horizontal = TRUE) {

    #---------------------------
    # argument match
    #---------------------------
    check_var_class(data, is.data.frame)
    column_in_dataframe(data, var)
    if(!is_blank(group)) column_in_dataframe(data, group)
    if(!is_blank(facet_r)) column_in_dataframe(data, facet_r)
    if(!is_blank(facet_c)) column_in_dataframe(data, facet_c)
    arg_in_choices(facet_scale, c('free', 'free_x', 'free_y', 'fixed'))
    arg_in_choices(facet_space, c('free', 'free_x', 'free_y', 'fixed'))
    arg_in_choices(counts_pos, c('inside-bar', 'outside-bar'))
    arg_in_choices(bar_label_align, c('right', 'center', 'left'))
    arg_in_choices(group_bar_position, c('dodge', 'stack'))
    arg_in_choices(grids, c('on', 'x', 'y', 'off'))
    if(!is.null(bar_label_angle)) check_var_class(bar_label_angle, is.numeric)
    add_counts <- isTRUE(add_counts)
    bw_theme <- isTRUE(bw_theme)
    horizontal <- isTRUE(horizontal)

    #---------------------------
    # data manipulation
    #---------------------------

    # sort the data
    group_by_vars <- c()
    if(!is_blank(facet_r)) group_by_vars <- c(group_by_vars, facet_r)
    if(!is_blank(facet_c)) group_by_vars <- c(group_by_vars, facet_c)
    group_by_vars <- c(group_by_vars, var)
    if(!is_blank(group)) group_by_vars <- c(group_by_vars, group)
    group_dots <- lapply(group_by_vars, as.symbol)
    data <- group_by_(data, .dots = group_dots)

    # make `var` a factor
    var_levels <- unlist(var_levels)
    if(is.null(var_levels)) var_levels <- sort(unique(data[[var]]))
    if(horizontal) var_levels <- rev(var_levels)
    data[[var]] <- factor(data[[var]], levels = var_levels)
    data <- data[!is.na(data[[var]]), , drop = F]
    if(!is.null(names(var_levels))) levels(data[[var]]) <- names(var_levels)

    if(is.null(x_lab)) x_lab <- ifelse(horizontal, 'Counts', var)
    if(is.null(y_lab)) y_lab <- ifelse(horizontal, var, 'Counts')
    if(is.null(title)) title <- paste('Barplot of', var)
    if(horizontal) {
        temp <- x_lab
        x_lab <- y_lab
        y_lab <- temp
    }

    if(!is_blank(facet_r)) {
        facet_r_levels <- unlist(facet_r_levels)
        if(is.null(facet_r_levels))
            facet_r_levels <- sort(unique(data[[facet_r]]))
        data[[facet_r]] <- factor(data[[facet_r]], levels = facet_r_levels)
        data <- data[!is.na(data[[facet_r]]), , drop = F]
        if(!is.null(names(facet_r_levels))) {
            levels(data[[facet_r]]) <- names(facet_r_levels)
        }
    }
    if(!is_blank(facet_c)) {
        facet_c_levels <- unlist(facet_c_levels)
        if(is.null(facet_c_levels))
            facet_c_levels <- sort(unique(data[[facet_c]]))
        data[[facet_c]] <- factor(data[[facet_c]], levels = facet_c_levels)
        data <- data[!is.na(data[[facet_c]]), , drop = F]
        if(!is.null(names(facet_c_levels))) {
            levels(data[[facet_c]]) <- names(facet_c_levels)
        }
    }
    if(!is_blank(group)) {
        group_levels <- unlist(group_levels)
        if(is.null(group_levels)) group_levels <- sort(unique(data[[group]]))
        data[[group]] <- factor(data[[group]], levels = group_levels)
        data <- data[!is.na(data[[group]]), , drop = F]
        if(!is.null(names(group_levels))) {
            levels(data[[group]]) <- names(group_levels)
        }
    }


    #---------------------------
    # make the plot
    #---------------------------
    plot_ <- gg_wrapper(
        data = data, aes_string(x = paste0('`', var, '`')),
        facet_r = facet_r, facet_c = facet_c,
        facet_scale = facet_scale, facet_space = facet_space,
        x_lab = x_lab, y_lab = y_lab, title = title,
        fill_var = group, fill_lab = group_lab,
        bw_theme = bw_theme, grids = grids
    )

    if(is_blank(group)) position <- position_identity()
    else if(group_bar_position == 'dodge') position <- position_dodge(width=0.9)
    else position <- position_stack(reverse = TRUE)
    plot_ <- plot_ + geom_bar(position = position)

    if(add_counts) {
        adjust <- ifelse(counts_pos == 'inside-bar', 1.3, -0.3)
        if(is_blank(group) || group_bar_position == 'dodge') {
            if(horizontal) {
                plot_ <- plot_ +
                    geom_text(stat = 'count', aes(label = ..count..),
                              hjust = adjust, position = position)
            } else {
                plot_ <- plot_ +
                    geom_text(stat = 'count', aes(label = ..count..),
                              vjust = adjust, position = position)
            }
        } else {
            label_group_by <- setdiff(group_by_vars, group)
            label_data <- data %>%
                arrange_(.dots = group_dots) %>%
                group_by_(.dots = group_dots) %>%
                summarise(counts = n()) %>%
                group_by_(.dots = lapply(label_group_by, as.symbol)) %>%
                mutate(y = cumsum(counts) - counts / 2)
            label_data[[group]] <- factor(label_data[[group]])
            if(horizontal) {
                plot_ <- plot_ +
                    geom_text(data = label_data, aes(y = y, label = counts))
            } else {
                plot_ <- plot_ +
                    geom_text(data = label_data, aes(y = y, label = counts))
            }
        }
    }

    if(!is.null(bar_label_angle)) angle <- bar_label_angle
    else angle <- ifelse(horizontal, 0, 90)
    adjust <- list('right' = 1, 'center' = 0.5, 'left' = 0)[[bar_label_align]]
    if(horizontal) {
        plot_ <- plot_ +
            theme(axis.text.y = element_text(angle = angle, hjust = adjust)) +
            coord_flip()
    } else {
        plot_ <- plot_ +
            theme(axis.text.x = element_text(angle = angle,
                                             hjust = adjust,
                                             vjust = 0.5))
    }

    return(plot_)

}



































