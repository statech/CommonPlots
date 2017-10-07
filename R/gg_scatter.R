#' Scatter plots
#'
#' Creates a scatter plot
#'
#' This function relies on \href{http://ggplot2.org/}{ggplot2} package to
#' create a scatter plot with ability to label points within specific region.
#' Point labelling has the option to repel text overlapping with
#' \href{https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html}{ggrepel}.
#' The function supports panel plot layout. By default black-and-white theme is
#' used.
#'
#' @param data Data frame: default dataset to use for plot
#' @param x Character: name of a \code{data} column mapped to x-axis
#'  variable, i.e. time
#' @param y Character: name of a \code{data} column mapped to y-axis
#'  variable
#' @param label Character: name of a \code{data} column mapped to point labels
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
#' @param color_var Character: name of a \code{data} column mapped to the color
#'  of the points
#' @param all_colors Vector: a vector of valid color representations for all
#'  the point colors. See \code{\link{is.color}} for valid color definition
#' @param color_lab Character: title of color legend
#' @param shape_var Character name of a \code{data} column mapped to the shape
#'  of the points
#' @param all_shapes Vector: a vector of all the point shapes. See
#'  \href{http://sape.inf.usi.ch/quick-reference/ggplot2/shape}{ggplot2 Quick Reference: shape}
#' @param shape_lab Character: title of shape legend
#' @param add_label Logical: whether or not to add labels for the points
#'  Default is set to `FALSE`
#' @param is_repel Logical: whether or not to avoid text overlapping using
#'  \href{https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html}{ggrepel}
#' @param label_xlim Numeric vector of length 2: it defines x-axis range to
#'  select points for labelling. See \code{label_xloc} for more details of point
#'  labelling rule. Default `c(-Inf, Inf)` includes all the points over x-axis
#' @param label_ylim Numeric vector of length 2: it defines y-axis range to
#'  select points for labelling. See \code{label_yloc} for more details of point
#'  labelling rule. Default `c(-Inf, Inf)` includes all the points over x-axis
#' @param label_xloc Character: Either 'middle' or 'sides'. Together with
#'  \code{label_xlim}, it defines the rule of point labelling. Only points,
#'  whose x-axis coordinates fall within (if 'middle') or beyond (if 'sides')
#'  the x-axis range defined by \code{label_xlim}, are to be labelled.
#' @param label_yloc Character: Either 'middle' or 'sides'. Together with
#'  \code{label_ylim}, it defines the rule of point labelling. Only points,
#'  whose y-axis coordinates fall within (if 'middle') or beyond (if 'sides')
#'  the y-axis range defined by \code{label_ylim}, are to be labelled.
#' @param x_lab Character: x-axis label
#' @param y_lab Character: y-axis label
#' @param title Character: barplot title
#' @param x_limit Numeric vector of length 2: limits for x-axis, e.g.
#'  \code{c(0, 10)}.
#' @param y_limit Numeric vector of length 2: limits for y-axis, e.g.
#'  \code{c(-5, 5)}
#' @param x_log Logical: whether or not to use log scale for x-axis. Default is
#'  set to `TRUE`
#' @param y_log Logical: whether or not to use log scale for y-axis. Default is
#'  set to `TRUE`
#' @param add_legend Logical: \code{TRUE} (default) to show legend and
#'  \code{FALSE} otherwise
#' @param legend_pos Character: dictates the location where to place the legend.
#'  By default, the legend will be place beneath the actual plot
#' @param reference_hline Numeric vector: locations of horizontal reference
#'  line(s) if there is any
#' @param reference_vline Numeric vector: locations of vertical reference
#'  line(s) if there is any
#' @param smooth_line Character: smoothing line type to be added to the points.
#'  Valid smoothing line types include `NULL` (default), "identity", "lm",
#'  "glm", "gam", "loess", "rlm". See also `method` argument in
#'  \code{\link[ggplot2]{geom_smooth}}. When `NULL`, no line is added
#' @param smooth_line_ci Logical: display confidence interval around smooth. By
#'  default (`FALSE`), confidence interval is not displayed
#' @param bw_theme Logical: If `TRUE` (default), black-and-white theme will be
#'  used. Refer to \code{\link[ggplot2]{theme_bw}} for more details
#' @param grids Character: grids option. Must be one of `c('on', 'major',
#'  'off')` with 'on' having both major and minor grids, 'major' having only
#'  major grids, and 'off' having no grids
#'
#' @return An object of class ggplot. Can be directly sent to plot with
#'  \code{\link{print}}
#'
#' @examples
#' data <- mtcars
#' data$model <- row.names(data)
#' data$cyl <- factor(data$cyl)
#' gg_scatter(data, x = 'wt', y = 'mpg', label = 'model', color_var = 'cyl',
#'            color_lab = 'Cylinder', add_label = TRUE, is_repel = TRUE,
#'            label_xlim = c(2, 5), label_xloc = 'sides',
#'            x_lab = 'Weight (1000 lbs)', y_lab = 'Miles/(US) gallon')
#'
#' gg_scatter(data, x = 'wt', y = 'mpg', label = 'model', facet_c = 'cyl',
#'            facet_c_levels = c('4 Cylinders' = '4', '6 Cylinders' = '6',
#'                               '8 Cylinders' = '8'),
#'            add_label = TRUE, is_repel = TRUE,
#'            x_lab = 'Weight (1000 lbs)', y_lab = 'Miles/(US) gallon')
#'
#' @export
#'
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)

gg_scatter <- function(data, x, y, label = NULL,
                       facet_r = NULL, facet_c = NULL,
                       facet_r_levels = NULL, facet_c_levels = NULL,
                       facet_scale = 'free', facet_space = 'free',
                       color_var = NULL, all_colors = NULL, color_lab = color_var,
                       shape_var = NULL, all_shapes = NULL, shape_lab = shape_var,
                       add_label = FALSE, is_repel = FALSE,
                       label_xlim = c(-Inf, Inf), label_ylim = c(-Inf, Inf),
                       label_xloc = 'middle', label_yloc = 'middle',
                       x_lab = x, y_lab = y, title = '',
                       x_limit = NULL, y_limit = NULL,
                       x_log = FALSE, y_log = FALSE,
                       add_legend = TRUE, legend_pos = 'bottom',
                       reference_hline = NULL, reference_vline = NULL,
                       smooth_line = NULL, smooth_line_ci = FALSE,
                       bw_theme = TRUE, grids = 'on') {

    #-----------------------------
    # argument match & error catch
    #-----------------------------
    if(!is.data.frame(data)) {
        tryCatch(
            data <- as.data.frame(data),
            error = function(err) {stop(
                'data must be a data frame or data frame convertable'
            )}
        )
    }
    column_in_dataframe(data, x)
    column_in_dataframe(data, y)
    if(!is_blank(label)) column_in_dataframe(data, label)
    if(!is_blank(facet_r)) column_in_dataframe(data, facet_r)
    if(!is_blank(facet_c)) column_in_dataframe(data, facet_c)
    if(!is_blank(color_var)) column_in_dataframe(data, color_var)
    if(!is_blank(shape_var)) column_in_dataframe(data, shape_var)
    add_label <- isTRUE(add_label)
    is_repel <- isTRUE(is_repel)
    if(add_label && !is_blank(label_xlim))
        check_var_class(label_xlim, is.numeric, 'numeric')
    if(add_label && !is_blank(label_ylim))
        check_var_class(label_ylim, is.numeric, 'numeric')
    if(add_label) {
        if(is_blank(label_xloc)) label_xloc <- 'middle'
        else arg_in_choices(label_xloc, c('middle', 'sides'))
        if(is_blank(label_yloc)) label_yloc <- 'middle'
        else arg_in_choices(label_yloc, c('middle', 'sides'))
    }
    if(!is.null(x_lab)) check_var_class(x_lab, is.character, 'character')
    if(!is.null(y_lab)) check_var_class(y_lab, is.character, 'character')
    if(!is.null(title)) check_var_class(title, is.character, 'character')
    if(!is_blank(x_limit)) check_var_class(x_limit, is.numeric, 'numeric')
    if(!is_blank(y_limit)) check_var_class(y_limit, is.numeric, 'numeric')
    x_log <- isTRUE(x_log)
    y_log <- isTRUE(y_log)
    add_legend <- isTRUE(add_legend)
    if(add_legend) arg_in_choices(legend_pos, c('left', 'right', 'bottom', 'up'))
    if(!is_blank(reference_hline))
        check_var_class(reference_hline, is.numeric, 'numeric')
    if(!is_blank(reference_vline))
        check_var_class(reference_vline, is.numeric, 'numeric')
    bw_theme <- isTRUE(bw_theme)
    arg_in_choices(grids, c('on', 'x', 'y', 'off'))


    #---------------------------
    # data manipulation
    #---------------------------
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

    if(!is_blank(shape_var)) {
        num_shapes <- length(unique(na.omit(data[[shape_var]])))
        all_shapes <- c(0:25, 32:255)[seq_len(num_shapes)]
    } else all_shapes <- NULL


    #-----------------------------
    # make the plot
    #-----------------------------
    plot_ <- gg_wrapper(
        data = data,
        aes_string(x = paste0('`', x, '`'), y = paste0('`', y, '`')),
        facet_r = facet_r, facet_c = facet_c,
        facet_scale = facet_scale, facet_space = facet_space,
        is_x_continuous = TRUE, is_y_continuous = TRUE,
        x_lab = x_lab, y_lab = y_lab, title = title,
        x_limit = x_limit, y_limit = y_limit,
        x_log = x_log, y_log = y_log,
        add_legend = add_legend, legend_pos = legend_pos,
        color_var = color_var, all_colors = all_colors, color_lab = color_lab,
        shape_var = shape_var, all_shapes = all_shapes, shape_lab = shape_lab,
        reference_hline = reference_hline, reference_vline = reference_vline,
        bw_theme = bw_theme, grids = grids
    )

    # add points
    plot_ <- plot_ + geom_point()

    # add smoothing line
    if(!is.null(smooth_line)) {
        if(smooth_line == 'identity') {
            plot_ <- geom_abline(intercept = 0, slope = 1)
        } else {
            plot_ <- plot_ + geom_smooth(method = smooth_line,
                                         se = smooth_line_ci)
        }
    }

    # add labels
    data_label <- data
    if(!is_blank(label_xlim)) {
        if(label_xloc == 'middle') {
            cond_x <- data_label[[x]] >= label_xlim[1] &
                data_label[[x]] <= label_xlim[2]
        } else {
            cond_x <- data_label[[x]] <= label_xlim[1] |
                data_label[[x]] >= label_xlim[2]
        }
        data_label <- data_label[cond_x, , drop = FALSE]
    }
    if(!is_blank(label_xlim)) {
        if(label_yloc == 'middle') {
            cond_y <- data_label[[y]] >= label_ylim[1] &
                data_label[[y]] <= label_ylim[2]
        } else {
            cond_y <- data_label[[y]] <= label_ylim[1] |
                data_label[[y]] >= label_ylim[2]
        }
        data_label <- data_label[cond_y, , drop = FALSE]
    }
    if(add_label && !is_blank(label)) {
        if(is_repel) {
            plot_ <- plot_ +
                ggrepel::geom_text_repel(
                    data = data_label,
                    aes_string(label = paste0('`', label, '`')),
                    show.legend = FALSE
                )
        } else {
            plot_ <- plot_ +
                geom_text(data = data_label,
                          aes_string(label = paste0('`', label, '`')),
                          hjust = 'inward', vjust = 0.5, show.legend = FALSE)
        }
    }

    return(plot_)
}
