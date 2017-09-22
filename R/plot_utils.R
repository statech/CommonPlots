#' @title Emulates ggplot2 Default Color Palette
#'
#' @param n Integer: number of output colors
#' @return A character vector of HEX colors of length \code{n}
#' @examples
#' gg_color_hue(n = 3)
#' @export
gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    return(hcl(h = hues, l = 65, c = 100)[1:n])
}


#' @title Makes Color(s) Transparent
#'
#' @param color Color vector: vector of any of the three kinds of R color
#'  specifications, i.e., either a color name (as listed by
#'  \code{\link{colors}()}), a hexadecimal string of the form \code{"#rrggbb"}
#'  or \code{"#rrggbbaa"} (see \code{\link{rgb}}), or a positive integer
#'  \code{i} meaning \code{\link{palette}()[i]}.
#' @param alpha Numeric: amount of transparency in `[0, 1]`
#' @return A character vector of HEX colors of same length as \code{color}
#' @examples
#' add_alpha(c('red', 'blue'), alpha = 0.5)
#' @export
#' @seealso \code{\link[ggplot2]{scale_alpha}}
add_alpha <- function(color, alpha = 1) {
    if(alpha < 0 | alpha > 1)
        stop('alpha value must be in [0, 1]')
    return(apply(unname(sapply(color, grDevices::col2rgb)) / 255, 2,
                 FUN = function(x) {grDevices::rgb(x[1], x[2], x[3],
                                                   alpha = alpha)}))
}


#' @title Compute Pretty Axis Ticks of Log-scale
#'
#' @description A wrapper function of \code{\link[grDevices]{axisTicks}} that
#'  uses log scale.
#' @param n Integer: value indicating (approximately) the desired number of
#'  intervals. See also \code{nint} in \code{\link[grDevices]{axisTicks}}
#' @return A function that taks a numeric vector and computes log-scale axis
#'  tick locations
#' @examples
#' tick_func <- base_log_breaks(n = 10)
#' tick_func(runif(100, 0, 10))
#' @export
#' @seealso \code{\link[grDevices]{axisTicks}}
base_log_breaks <- function(n = 10){
    function(x) {
        grDevices::axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
}


#' @title Create A Log-scale And Reverse Order Transformation Object
#'
#' @description A transformation created with \code{\link[scales]{trans_new}}
#'  by setting \code{transform} to log-scale and \code{inverse} to
#'  reverse-order function
#'
#' @param base Integer: the base with respect to which logarithms are computed
#' @return A transformation object
#' @examples
#' \dontrun{
#' revlog_trans <- reverselog_trans(base = 10)
#' }
#' @seealso \code{\link[scales]{trans_new}}
reverselog_trans <- function(base = 10) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    scales::trans_new(paste0('reverselog-', format(base)), trans, inv,
                      breaks = base_log_breaks(),
                      domain = c(.Machine$double.xmin, Inf))
}


#' @title Check If Valid Color Representation
#'
#' @description A valid color representation is any of the three kinds of R
#'  color specifications, i.e., either a color name (as listed by
#'  \code{\link{colors}()}), a hexadecimal string of the form \code{"#rrggbb"}
#'  or \code{"#rrggbbaa"} (see \code{\link{rgb}}), or a positive integer
#'  \code{i} meaning \code{\link{palette}()[i]}.
#' @param obj Object: object to be tested
#' @return Logical: \code{TRUE} if the object is a valid color representation,
#'  \code{FALSE} otherwise
#' @examples
#' is.color(c('red', 'blue'))
#' is.color(1)
#' is.color('abcd')
#' @export
#' @seealso \code{\link[grDevices]{col2rgb}}
is.color <- function(obj) {
    sapply(obj, function(X) {
        tryCatch(is.matrix(grDevices::col2rgb(X)),
                 error = function(e) FALSE)
    })
}



#' @title ggplot position_jitter with random seed
gg_jitter <- function(x, width = NULL, randseed = NULL) {
    if(!is.null(randseed)) set.seed(randseed)
    if(is.null(width)) width <- ggplot2::resolution(x, zero = FALSE) * 0.4
    trans_x <- base::jitter(x, amount = width)
    return(trans_x)
}


#' @title imitate ggplot position_dodge
gg_dodge <- function(x, group, width = NULL) {
    if(any(is.null(x), is.null(group)))
        stop('both `x` and `group` must be present')
    if(is.null(width)) width <- ggplot2::resolution(x, zero = FALSE) * 0.75
    if(!is.factor(group)) group <- factor(group)
    ngroups <- nlevels(group)
    x <- x + (as.integer(group) - median(seq_len(ngroups))) / ngroups * width
    return(x)
}


#' @title Set/Modify ggplot2 Components
#'
#' @description Helper function that allows the user to easily set/modify
#'  ggplot2 object components, such as title, x-/y-axis log scale, grids and
#'  etc..
gg_wrapper <- function(..., facet_r = NULL, facet_c = NULL,
                       facet_scale = 'fixed', facet_space = 'fixed',
                       is_x_continuous = TRUE, is_y_continuous = TRUE,
                       x_lab = NULL, y_lab = NULL, title = NULL,
                       x_limit = NULL, y_limit = NULL,
                       x_log = FALSE, y_log = FALSE,
                       x_reverse = FALSE, y_reverse = FALSE,
                       x_axis_breaks = NULL, y_axis_breaks = NULL,
                       x_tick_labels = x_axis_breaks,
                       y_tick_labels = y_axis_breaks,
                       x_tick_angle = 0, y_tick_angle = 0,
                       x_hjust = 0.5, y_hjust = 0.5,
                       add_legend = TRUE, legend_pos = 'bottom',
                       color_var = NULL, color_lab = color_var, all_colors = NULL,
                       fill_var = NULL, fill_lab = fill_var, all_fills = NULL,
                       size_var = NULL, size_lab = size_var, all_sizes = NULL,
                       shape_var = NULL, shape_lab = shape_var, all_shapes = NULL,
                       linetype_var = NULL, linetype_lab = linetype_var,
                       all_linetypes = NULL,
                       alpha_var = NULL, alpha_lab = alpha_var, all_alphas = NULL,
                       reference_hline = NULL, reference_vline = NULL,
                       grids = 'on', bw_theme = TRUE,
                       strip_background_color = NULL,
                       x_expand = NULL, y_expand = NULL) {

    #---------------------------
    # argument match
    #---------------------------
    is_x_continuous <- isTRUE(is_x_continuous)
    is_y_continuous <- isTRUE(is_y_continuous)
    x_log <- isTRUE(x_log)
    y_log <- isTRUE(y_log)
    x_reverse <- isTRUE(x_reverse)
    y_reverse <- isTRUE(y_reverse)
    bw_theme <- isTRUE(bw_theme)
    add_legend <- isTRUE(add_legend)
    legend_pos <- match.arg(tolower(legend_pos),
                            c('left', 'right', 'bottom', 'top'))
    if(is.null(x_lab)) x_lab <- waiver()
    if(is.null(y_lab)) y_lab <- waiver()
    if(is_blank(x_limit)) x_limit <- NULL
    if(is_blank(y_limit)) y_limit <- NULL
    if(!grids %in% c('on', 'x', 'y', 'off')) {
        stop('grids must be one of `c("on", "x", "y", "off")`')
    }

    # `environment = parent.frame()` sets the environment to the calling env
    # so that ggplot is able to access the local variable defined in the
    # function that calls `gg_wrapper`
    base_plot <- ggplot2::ggplot(..., environment = parent.frame())
    if(bw_theme) base_plot <- base_plot + theme_bw()


    # add aesthetics - color
    if(!is_blank(color_var)) {
        base_plot <- base_plot + aes_string(colour = paste0('`', color_var, '`'))
        if(!is_blank(all_colors)) {
            if(!all(is.color(all_colors)))
                stop('all_colors must be valid color representation')
            base_plot <- base_plot + scale_colour_manual(values = all_colors)
        }
    } else {
        if(!is_blank(all_colors)) {
            if(!all(is.color(all_colors)))
                stop('all_colors must be valid color representation')
            base_plot <- base_plot + aes(colour = factor(1)) +
                scale_colour_manual(values = all_colors, guide = FALSE)
        }
    }

    # add aesthetics - fill
    if(!is_blank(fill_var)) {
        base_plot <- base_plot + aes_string(fill = paste0('`', fill_var, '`'))
        if(!is_blank(all_fills)) {
            if(!all(is.color(all_fills)))
                stop('all_fills must be valid color representation')
            base_plot <- base_plot + scale_fill_manual(values = all_fills)
        }
    } else {
        if(!is_blank(all_fills)) {
            if(!all(is.color(all_fills)))
                stop('all_fills must be valid color representation')
            base_plot <- base_plot + aes(fill = factor(1)) +
                scale_fill_manual(values = all_fills, guide = FALSE)
        }
    }

    # add aesthetics - size
    if(!is_blank(size_var)) {
        base_plot <- base_plot + aes_string(size = paste0('`', size_var, '`'))
        if(!is_blank(all_sizes)) {
            if(!all(all_sizes >= 0))
                stop('all_sizes must be >= 0')
            base_plot <- base_plot + scale_size_manual(values = all_sizes)
        }
    } else {
        if(!is_blank(all_sizes)) {
            if(!all(all_sizes >= 0))
                stop('all_sizes must be >= 0')
            base_plot <- base_plot + aes(size = factor(1)) +
                scale_size_manual(values = all_sizes, guide = FALSE)
        }
    }

    # add aesthetics - shape
    if(!is_blank(shape_var)) {
        base_plot <- base_plot + aes_string(shape = paste0('`', shape_var, '`'))
        if(!is_blank(all_shapes)) {
            if(!(all(all_shapes %% 1 == 0 & all_shapes >= 0)))
                stop('all_shapes must be an integer >= 0')
            base_plot <- base_plot + scale_shape_manual(values = all_shapes)
        }
    } else {
        if(!is_blank(all_shapes)) {
            if(!(all(all_shapes %% 1 == 0 & all_shapes >= 0)))
                stop('all_shapes must be an integer >= 0')
            base_plot <- base_plot + aes(shape = factor(1)) +
                scale_shape_manual(values = all_shapes, guide = FALSE)
        }
    }

    # add aesthetics - linetype
    if(!is_blank(linetype_var)) {
        base_plot <- base_plot +
            aes_string(linetype = paste0('`', linetype_var, '`'))
        if(!is_blank(all_linetypes)) {
            valid_linetypes <- c(
                'blank' = 0, 'solid' = 1, 'dashed' = 2,
                'dotted' = 3, 'dotdash' = 4, 'longdash' = 5,
                'twodash' = 6
            )
            if(!(all(all_linetypes %in% names(valid_linetypes)) |
                 all(all_linetypes %in% valid_linetypes)))
                stop('all_linetypes must be valid linetype value')
            base_plot <- base_plot + scale_linetype_manual(values = all_linetypes)
        }
    } else {
        if(!is_blank(all_linetypes)) {
            valid_linetypes <- c(
                'blank' = 0, 'solid' = 1, 'dashed' = 2,
                'dotted' = 3, 'dotdash' = 4, 'longdash' = 5,
                'twodash' = 6
            )
            if(!(all(all_linetypes %in% names(valid_linetypes)) |
                 all(all_linetypes %in% valid_linetypes)))
                stop('all_linetypes must be valid linetype value')
            base_plot <- base_plot + aes(linetype = factor(1)) +
                scale_linetype_manual(values = all_linetypes, guide = FALSE)
        }
    }

    # add aesthetics - alpha
    if(!is_blank(alpha_var)) {
        base_plot <- base_plot + aes_string(alpha = paste0('`', alpha_var, '`'))
        if(!is_blank(all_alphas)) {
            if(!(all(all_alphas >= 0 & all_alphas <= 1)))
                stop('all_alphas must be a number in [0, 1]')
            base_plot <- base_plot + scale_alpha_manual(values = all_alphas)
        }
    } else {
        if(!is_blank(all_alphas)) {
            if(!(all(all_alphas >= 0 & all_alphas <= 1)))
                stop('all_alphas must be a number in [0, 1]')
            base_plot <- base_plot + aes(alpha = factor(1)) +
                scale_alpha_manual(values = all_alphas, guide = FALSE)
        }
    }

    # add faceting
    if(!is_blank(facet_r) | !is_blank(facet_c)) {
        facets <- paste(ternary(is_blank(facet_r), '.', facet_r), '~',
                        ternary(is_blank(facet_c), '.', facet_c))
        base_plot <- base_plot +
            facet_grid(facets, scales = facet_scale, space = facet_space)
    }

    # draw x-/y-axis
    if(!is_x_continuous && x_log) {
        stop('discrete x-axis does not take log scale')
    }
    if(!is_x_continuous && x_reverse) {
        stop('discrete x-axis does not take descending order')
    }
    if(!is_y_continuous && y_log) {
        stop('discrete y-axis does not take log scale')
    }
    if(!is_y_continuous && y_reverse) {
        stop('discrete y-axis does not take descending order')
    }
    if(!x_log) {
        if(any(!is_blank(x_axis_breaks), !is.null(x_tick_labels))) {
            if(is_blank(x_axis_breaks)) x_axis_breaks <- waiver()
            if(is.null(x_tick_labels)) x_tick_labels <- waiver()
            x_axis_draw <- ternary(
                x_reverse, scale_x_reverse,
                ternary(is_x_continuous, scale_x_continuous, scale_x_discrete)
            )
            if(is_blank(x_expand)) {
                x_expand <- ternary(is_x_continuous, c(0.017, 0), c(0, 0.6))
            }
            base_plot <- base_plot +
                x_axis_draw(name = x_lab, breaks = x_axis_breaks,
                            labels = x_tick_labels, expand = x_expand)
        }
    } else {
        force(x_tick_labels)
        trans <- ternary(x_reverse, reverselog_trans(10), 'log10')
        if(is.null(x_axis_breaks)) {
            x_axis_breaks <- ternary(x_reverse, waiver(), base_log_breaks())
        }
        if(is.null(x_tick_labels)) x_tick_labels <- base::prettyNum
        if(is_blank(x_expand)) x_expand <- c(0.017, 0)
        base_plot <- base_plot +
            scale_x_continuous(name = x_lab, breaks = x_axis_breaks,
                               labels = x_tick_labels, trans = trans,
                               expand = x_expand)
    }
    if(!y_log) {
        if(any(!is_blank(y_axis_breaks), !is.null(y_tick_labels))) {
            if(is_blank(y_axis_breaks)) y_axis_breaks <- waiver()
            if(is.null(y_tick_labels)) y_tick_labels <- waiver()
            y_axis_draw <- ternary(
                y_reverse, scale_y_reverse,
                ternary(is_y_continuous, scale_y_continuous, scale_y_discrete)
            )
            if(is_blank(y_expand)) {
                y_expand <- ternary(is_y_continuous, c(0.025, 0), c(0, 0.6))
            }
            base_plot <- base_plot +
                y_axis_draw(name = y_lab, breaks = y_axis_breaks,
                            labels = y_tick_labels, expand = y_expand)
        }
    } else {
        force(y_tick_labels)
        trans <- ternary(y_reverse, reverselog_trans(10), 'log10')
        if(is.null(y_axis_breaks)) {
            y_axis_breaks <- ternary(y_reverse, waiver(), base_log_breaks())
        }
        if(is.null(y_tick_labels)) {
            y_tick_labels <- base::prettyNum
        }
        if(is_blank(y_expand)) y_expand <- c(0.025, 0)
        base_plot <- base_plot +
            scale_y_continuous(name = y_lab, breaks = y_axis_breaks,
                               labels = y_tick_labels, trans = trans,
                               expand = y_expand)
    }

    # add horizontal reference line
    if(!is_blank(reference_hline)) {
        if(is.numeric(reference_hline)) {
            base_plot <- base_plot +
                geom_hline(yintercept = reference_hline, linetype = 'dashed')
        }
        if(isTRUE(reference_hline)) {
            base_plot <- base_plot +
                geom_hline(yintercept = 0, linetype = 'dashed')
        }
    }

    # add vertical reference line
    if(!is_blank(reference_vline)) {
        if(is.numeric(reference_vline)) {
            base_plot <- base_plot +
                geom_vline(xintercept = reference_vline, linetype = 'dashed')
        }
        if(isTRUE(reference_vline)) {
            base_plot <- base_plot +
                geom_hline(xintercept = 0, linetype = 'dashed')
        }
    }

    # add title
    if(!is.null(title)) base_plot <- base_plot + labs(title = title)
    if(!is.null(x_lab)) base_plot <- base_plot + labs(x = x_lab)
    if(!is.null(y_lab)) base_plot <- base_plot + labs(y = y_lab)
    if(!is_blank(color_var)) base_plot <- base_plot + labs(colour = color_lab)
    if(!is_blank(fill_var)) base_plot <- base_plot + labs(fill = fill_lab)
    if(!is_blank(size_var)) base_plot <- base_plot + labs(size = size_lab)
    if(!is_blank(shape_var)) base_plot <- base_plot + labs(shape = shape_lab)
    if(!is_blank(linetype_var))
        base_plot <- base_plot + labs(linetype = linetype_lab)
    if(!is_blank(alpha_var)) base_plot <- base_plot + labs(alpha = alpha_lab)

    # specify theme elements
    if(grids == 'on') {
        panel.grid.major.x = element_line()
        panel.grid.major.y = element_line()
        panel.grid.minor = element_blank()
    } else if(grids == 'x') {
        panel.grid.major.x = element_line()
        panel.grid.major.y = element_blank()
        panel.grid.minor = element_blank()
    } else if(grids == 'y') {
        panel.grid.major.x = element_blank()
        panel.grid.major.y = element_line()
        panel.grid.minor = element_blank()
    } else {
        panel.grid.major.x = element_blank()
        panel.grid.major.y = element_blank()
        panel.grid.minor = element_blank()
    }
    base_plot <- base_plot +
        theme(axis.text.x = element_text(angle = x_tick_angle,
                                         hjust = x_hjust, vjust = 0.5),
              axis.text.y = element_text(angle = y_tick_angle,
                                         hjust = y_hjust, vjust = 0.5),
              panel.grid.major.x = panel.grid.major.x,
              panel.grid.major.y = panel.grid.major.y,
              panel.grid.minor = panel.grid.minor,
              legend.position = legend_pos,
              plot.title = element_text(hjust = 0.5))
    if(!add_legend) base_plot <- base_plot + theme(legend.position = 'none')
    if(all(is.color(strip_background_color))) {
        base_plot <- base_plot +
            theme(strip.background = element_rect(fill = strip_background_color))
    }

    if(!is.null(x_limit)) base_plot <- base_plot + coord_cartesian(xlim = x_limit)
    if(!is.null(y_limit)) base_plot <- base_plot + coord_cartesian(ylim = y_limit)

    return(base_plot)
}










