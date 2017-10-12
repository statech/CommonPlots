#' Time-profiling Plots
#'
#' Creats time-profiling plots with
#' \href{https://en.wikipedia.org/wiki/Panel_data}{longitudinal data}
#'
#' This function relies on \href{http://ggplot2.org/}{ggplot2} package to
#' create time-profiling plots including: Mean + SD/SE plot, Mean + Median
#' plot, Boxplot, Spaghetti plot and their mixture. Labels can be added to the
#' end of each line using
#' \href{http://directlabels.r-forge.r-project.org/}{directlabels}.
#' Panel plot layout is supported. By default black-and-white theme is used.
#'
#' @param data Data frame: default dataset to use for plot
#' @param x Character: name of a \code{data} column mapped to x-axis
#'  variable, i.e. time
#' @param y Character: name of a \code{data} column mapped to y-axis
#'  variable
#' @param subject Character: name of a \code{data} column mapped to subject IDs
#'  or other types of IDs that define membership of repeated measurements in
#'  the data
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
#' @param x_limit Numeric vector of length 2: limits for x-axis, e.g.
#'  \code{c(0, 10)}.
#' @param y_limit Numeric vector of length 2: limits for y-axis, e.g.
#'  \code{c(-5, 5)}
#' @param x_tick_label Vector: x-axis tick marks
#' @param all_xticks Logical: \code{TRUE} (default) to display all marks
#'  specified as the unique values of \code{x_tick_label}
#' @param x_tick_angle Numeric: the orientation angle (in [0, 360]) of the
#'  x-axis tick marks. By default, the label will be horizontal.
#' @param y_tick_angle Numeric: the orientation angle (in [0, 360]) of the
#'  y-axis tick marks. By default, the label will be horizontal.
#' @param geoms Character vector: specify geometric components to be included
#'  in the plot. Valid geoms are 'boxplot', 'line', 'point', 'sumline'. For
#'  example, if the user needs to create a boxplot with raw data points,
#'  specify \code{geoms = c('boxplot', 'point')}; if the user wants to create a
#'  Mean + SE plot, let \code{geoms = c('sumline', 'point')} together with
#'  specification of \code{avg_method} and \code{var_method}
#' @param avg_method Character: dicates whether to use \code{\link[base]{mean}}
#'  (\code{avg_method = 'mean'}) or \code{\link[stats]{median}}. Be noted that
#'  only these two options are permitted and both methods will remove \code{NA}s
#'  in calculation
#' @param var_method Character: dicates whether to use standard error
#'  (\code{var_method = 'se'}) or standard deviation (\code{var_method = 'sd'}).
#'  Be noted that only these two options are permitted and both methods will
#'  remove \code{NA}s in calculation
#' @param y_log Logical: \code{TRUE} to use log-scale for y-axis and
#'  \code{FALSE} (default) otherwise.
#' @param sample_size Logical: \code{TRUE} (default) to place sample size
#'  annotation along x-axis tick marks and \code{FALSE} otherwise
#' @param sample_size_font_size Numeric: font size of sample size annotation
#' @param reference_hline Numeric vector: locations of horizontal reference
#'  line(s) if there is any
#' @param reference_vline Numeric vector: locations of vertical reference
#'  line(s) if there is any
#' @param subject_show Logical: \code{TRUE} to show subject IDs (or other types)
#'  and \code{FALSE} (default) otherwise
#' @param add_legend Logical: \code{TRUE} (default) to show legend and
#'  \code{FALSE} otherwise
#' @param legend_pos Character: dictates the location where to place the legend.
#'  Possible locations are 'left', 'right', 'bottom', 'top'. By default, the
#'  legend will be place beneath the actual plot, i.e. `legend_pos = 'bottom'`
#' @param all_colors Vector of valid color representation: when \code{group} is
#'  present, categories of \code{group} will take different color for specified
#'  geometric objects. This argument allows the user to overwrite default color
#'  theme. See \code{col} argument in \code{\link[grDevices]{col2rgb}} for types
#'  of valid color representation
#' @param all_linetypes Numeric vector: by default, lines of different
#'  categories of \code{group}, when present, will be solid lines. This argument
#'  allows the user to overwrite the default line types. See
#'  \href{http://sape.inf.usi.ch/quick-reference/ggplot2/linetype}{linetypes}
#'  for valid ggplot2 linetype specification
#' @param bw_theme Logical: if `TRUE` (default), black-and-white theme will be
#'  used. Refer to \code{\link[ggplot2]{theme_bw}} for more details
#' @param jitter_factor Numeric: determines how much the points are jittered
#'  over x-axis when `geoms` includes 'point'. By default, `jitter_factor = 1`
#' @param grids Character: control the grids. If `on` (default), grids will be
#'  drawn; if `x`, only grids on x-axis will be drawn; if `y`, only grids on
#'  y-axis will be drawn; if `off`, no grids will be drawn
#' @param randseed Numeric: random seed can be set in producing jittered points
#'  when \code{geoms} includes `point`. By default, no random seed is set,
#'  i.e. `randseed = NULL`
#' @param return_data Logical: \code{TRUE} to return both plot and the data
#'  used to produce the plot and \code{FALSE} (default) to return only plot
#'
#' @return An object of class ggplot (if \code{return_data = FALSE}) or a list
#'  of two components: an object of class ggplot and a data frame that used to
#'  generate the plot
#' @examples
#' library(CommonPlots)
#' data(koopLME)
#'
#' # Mean + SE plot
#' data <- koopLME
#' data$siblings <- factor(with(data, ifelse(SIBLINGS >= 3, '>= 3', SIBLINGS)),
#'                         levels = c(0, 1, 2, '>= 3'))
#' mean_se_plot <- gg_time_profiling(
#'     data, x = 'TIMETRND', y = 'LOGWAGE', group = 'siblings',
#'     geoms = c('sumline'), avg_method = 'mean',
#'     var_method = 'se', x_lab = 'Time trend',
#'     y_lab = 'Log of hourly wage', group_lab = 'Number of siblings',
#'     title = 'Mean + SE plot'
#' )
#'
#' # Median + IQR plot
#' data <- koopLME
#' data$siblings <- factor(with(data, ifelse(SIBLINGS >= 3, '>= 3', SIBLINGS)),
#'                         levels = c(0, 1, 2, '>= 3'))
#' sib_labels <- paste('Number of siblings:', levels(data$siblings))
#' median_iqr_plot <- gg_time_profiling(
#'     data, x = 'TIMETRND', y = 'LOGWAGE', facet_c = 'siblings',
#'     facet_c_levels = setNames(levels(data$siblings), sib_labels),
#'     geoms = c('sumline'), avg_method = 'median',
#'     x_lab = 'Time trend', y_lab = 'Log of hourly wage',
#'     title = 'Median + IQR plot'
#' )
#'
#' # Spaghetti plot
#' data <- koopLME
#' data <- with(data, data[PERSONID %in% unique(PERSONID)[1:100], ])
#' spaghetti_plot <- gg_time_profiling(
#'     data, x = 'TIMETRND', y = 'LOGWAGE', subject = 'PERSONID',
#'     geoms = c('line'), x_lab = 'Time trend',
#'     y_lab = 'Log of hourly wage', title = 'Spaghetti plot',
#'     subject_show = TRUE
#' )
#'
#' # Box plot over time
#' data <- koopLME
#' data$siblings <- factor(with(data, ifelse(SIBLINGS >= 3, '>= 3', SIBLINGS)),
#'                         levels = c(0, 1, 2, '>= 3'))
#' sib_labels <- paste('Number of siblings:', levels(data$siblings))
#' boxplot <- gg_time_profiling(
#'     data, x = 'TIMETRND', y = 'LOGWAGE', facet_r = 'siblings',
#'     facet_r_levels = setNames(levels(data$siblings), sib_labels),
#'     geoms = c('boxplot', 'point'), x_lab = 'Time trend',
#'     y_lab = 'Log of hourly wage', title = 'Box plot',
#'     sample_size = FALSE, all_xticks = TRUE
#' )
#'
#'
#' @import dplyr
#' @import directlabels
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_time_profiling <- function(data, x, y, subject = NULL,
                              group = NULL, group_levels = NULL,
                              facet_r = NULL, facet_c = NULL,
                              facet_r_levels = NULL, facet_c_levels = NULL,
                              facet_scale = 'free', facet_space = 'fixed',
                              x_lab = x, y_lab = y, group_lab = group,
                              title = '', x_limit = NULL, y_limit = NULL,
                              x_tick_label = x, all_xticks = FALSE,
                              x_tick_angle = 0, y_tick_angle = 0,
                              geoms = c('point', 'line', 'sumline', 'boxplot'),
                              avg_method = 'mean', var_method = 'se',
                              y_log = FALSE, sample_size = TRUE,
                              sample_size_font_size = 3,
                              reference_hline = NULL, reference_vline = NULL,
                              subject_show = FALSE, subject_label_size = 0.8,
                              add_legend = TRUE, legend_pos = 'bottom',
                              all_colors = NULL, all_linetypes = NULL,
                              jitter_factor = 1, grids = 'on', bw_theme = TRUE,
                              randseed = NULL, return_data = FALSE) {

    #---------------------------
    # error-catch
    #---------------------------
    if(!is.data.frame(data)) stop('data must be a data frame')
    all_columns <- names(data)
    if(!all(x %in% all_columns, y %in% all_columns))
        stop('x & y must both be columns of data')
    if(!is_blank(group)) {
        if(!(group %in% all_columns))
            stop('group must be a column of data')
    }
    if(!is_blank(subject)) {
        if(!(subject %in% all_columns))
            stop('subject must be a column of data')
    }
    if(!is_blank(facet_r)) {
        if(!(facet_r %in% all_columns)) stop('facet_r must be a column of data')
    }
    if(!is_blank(facet_c)) {
        if(!(facet_c %in% all_columns)) stop('facet_c must be a column of data')
    }
    x_is_numeric <- TRUE
    tryCatch(
        data[[x]] <- as.numeric(data[[x]]),
        warning = function(w) {
            stop('x column must be numeric or numeric-convertable')
        }
    )

    #---------------------------
    # argument match
    #---------------------------
    y_log <- isTRUE(y_log)
    sample_size <- isTRUE(sample_size)
    add_legend <- isTRUE(add_legend)
    return_data <- isTRUE(return_data)
    all_xticks <- isTRUE(all_xticks)
    subject_show <- isTRUE(subject_show)
    bw_theme <- isTRUE(bw_theme)
    if(!is.null(avg_method))
        avg_method <- match.arg(tolower(avg_method),choices = c('mean', 'median'))
    if(!is.null(var_method))
        var_method <- match.arg(tolower(var_method), choices = c('sd', 'se'))
    geoms <- tolower(geoms)
    legend_pos <- match.arg(tolower(legend_pos),
                            c('left', 'right', 'bottom', 'top'))
    arg_in_choices(grids, c('on', 'x', 'y', 'off'))

    #---------------------------
    # define constants (those could be moved to the function arguments)
    #---------------------------
    point_shape <- 19
    point_size <- 1
    point_alpha <- 0.5
    dodge_boxplot_factor <- 0.75
    dodge_line_factor <- 0.4
    errorbar_factor <-0.6
    x_point_var <- 'x_point'

    #---------------------------
    # data manipulation
    #---------------------------
    data <- data[!is.na(data[[x]]) & !is.na(data[[y]]), ]
    if(!is.null(x_limit)) {
        data <- data[data[[x]] >= x_limit[1] & data[[x]] <= x_limit[2], ]
    }
    unique_x <- sort(unique_na(data[[x]]))
    group_list <- NULL
    nrows <- 1
    if(!is_blank(facet_r)) {
        facet_r_levels <- unlist(facet_r_levels)
        if(is.null(facet_r_levels))
            facet_r_levels <- sort(unique(data[[facet_r]]))
        data[[facet_r]] <- factor(data[[facet_r]], levels = facet_r_levels)
        data <- data[!is.na(data[[facet_r]]), , drop = F]
        if(!is.null(names(facet_r_levels))) {
            levels(data[[facet_r]]) <- names(facet_r_levels)
        }
        group_list <- c(group_list, facet_r)
        nrows <- nlevels(data[[facet_r]])
    }
    ncols <- 1
    if(!is_blank(facet_c)) {
        facet_c_levels <- unlist(facet_c_levels)
        if(is.null(facet_c_levels))
            facet_c_levels <- sort(unique(data[[facet_c]]))
        data[[facet_c]] <- factor(data[[facet_c]], levels = facet_c_levels)
        data <- data[!is.na(data[[facet_c]]), , drop = F]
        if(!is.null(names(facet_c_levels))) {
            levels(data[[facet_c]]) <- names(facet_c_levels)
        }
        group_list <- c(group_list, facet_c)
        ncols <- nlevels(data[[facet_c]])
    }
    ngroups <- 1
    if(!is_blank(group)) {
        group_levels <- unlist(group_levels)
        if(is.null(group_levels)) group_levels <- sort(unique(data[[group]]))
        data[[group]] <- factor(data[[group]], levels = group_levels)
        data <- data[!is.na(data[[group]]), , drop = F]
        if(!is.null(names(group_levels))) {
            levels(data[[group]]) <- names(group_levels)
        }
        group_list <- c(group_list, group)
        ngroups <- nlevels(data[[group]])
    }
    group_list <- c(group_list, x)

    # create jitter for geom_point manually if present
    resolution_x <- ggplot2::resolution(unique_x, zero = FALSE)
    dodge_line <- min(diff(range(unique_x))/50, resolution_x*dodge_line_factor)
    dodge_boxplot <- resolution_x * dodge_boxplot_factor
    dodge_geom <- ifelse('boxplot' %in% geoms, dodge_boxplot, dodge_line)
    dodge_ <- position_dodge(dodge_geom)
    if('point' %in% geoms) {
        if(ngroups > 1) {
            shift <- (as.integer(data[[group]]) - median(1:ngroups)) / ngroups
            data[[x_point_var]] <- data[[x]] + shift * dodge_geom
        } else data[[x_point_var]] <- data[[x]]
        data[[x_point_var]] <- gg_jitter(
            data[[x_point_var]], dodge_geom * jitter_factor * 0.25, randseed
        )
    }

    # define expand in ggplot2 axis
    if(sample_size) x_expand <- c(0.01, 0) else x_expand <- NULL


    #---------------------------
    # make the plot
    #---------------------------
    plot_ <- gg_wrapper(
        data, aes_string(x = paste0('`', x, '`'),
                         y = paste0('`', y, '`')),
        facet_r = facet_r, facet_c = facet_c,
        y_log = y_log, x_lab = x_lab, y_lab = y_lab, title = title,
        x_limit = NULL, y_limit = y_limit,
        x_tick_angle = x_tick_angle, y_tick_angle = y_tick_angle,
        facet_scale = facet_scale, facet_space = facet_space,
        add_legend = add_legend, legend_pos = legend_pos,
        color_var = group, all_colors = all_colors, color_lab = group_lab,
        linetype_var = if(is_blank(all_linetypes)) NULL else group,
        all_linetypes = all_linetypes, linetype_lab = group_lab,
        reference_hline = reference_hline,
        reference_vline = reference_vline,
        x_expand = x_expand, bw_theme = bw_theme, grids = grids
    )

    if('boxplot' %in% geoms) {
        group_cols <- data[, group_list, drop = FALSE]
        data_boxplot <- data %>%
            mutate(x_factor = do.call(interaction, as.list(group_cols)))
        outlier_shape <- ifelse('point' %in% geoms, NA, point_shape)
        plot_ <- plot_ +
            geom_boxplot(data = data_boxplot, position = dodge_,
                         aes_string(x = x, group = 'x_factor'),
                         outlier.shape = outlier_shape, alpha = 0.5)
    }
    if('line' %in% geoms) {
        if(is.null(subject)) stop('please provide subject')
        plot_ <- plot_ + geom_line(aes_string(group = subject),
                                   show.legend = FALSE)
        if(subject_show) {
            plot_ <- plot_ + geom_dl(aes_string(label = subject),
                                     method = list(dl.combine('last.points'),
                                                   cex = subject_label_size))
        }
    }
    if('sumline' %in% geoms) {
        if(avg_method == 'median') {
            fun_y_ <- stats::median
            fun_data_ <- median_iqr
        } else if(avg_method == 'mean') {
            fun_y_ <- mean_na
            if(var_method == 'se') fun_data_ <- mean_se
            else if(var_method == 'sd') fun_data_ <- mean_sd
        }
        plot_ <- plot_ +
            stat_summary(fun.y = fun_y_, geom = 'line', position = dodge_) +
            stat_summary(fun.y = fun_y_, geom = 'point', position = dodge_) +
            stat_summary(fun.data = fun_data_, geom = 'errorbar',
                         width = dodge_geom * errorbar_factor,
                         position = dodge_, linetype = 'solid')
    }
    if('point' %in% geoms) {
        plot_ <- plot_ + geom_point(aes(x = x_point), shape = point_shape,
                                    size = point_size, alpha = point_alpha)
    }


    #---------------------------
    # add sample size
    #---------------------------
    if(sample_size) {

        # for sample size annotation
        ss_factor <- 0.04 * sample_size_font_size / 3
        fnote_size_ss <- sample_size_font_size / (1 + 0.5 * (nrows - 1))
        center_aligned <- 0.5
        slightly_right_aligned <- 0.7

        # calculate x-/y-axis range
        if(any(c('point', 'line', 'boxplot') %in% geoms)) {
            fun_yrange <- range_na
        } else if('sumline' %in% geoms) {
            fun_yrange <- function(x) {
                res <- fun_data_(x)
                if(is.na(res$ymin) || is.na(res$ymax)) return(c(res$y, res$y))
                return(c(res$ymin, res$ymax))
            }
        }
        group_list_ss <- c()
        if(!is_blank(facet_r)) group_list_ss <- c(group_list_ss, facet_r)
        if(!is_blank(facet_c)) group_list_ss <- c(group_list_ss, facet_c)
        yrange <- data %>%
            group_by_(.dots = lapply(group_list, as.symbol)) %>%
            do(res = grDevices::extendrange(
                if(y_log) log(fun_yrange(.[[y]]), base = 10)
                else fun_yrange(.[[y]])
            )) %>%
            mutate(ymin = unlist(res)[1], ymax = unlist(res)[2]) %>%
            ungroup() %>%
            group_by_(.dots = lapply(group_list_ss, as.symbol)) %>%
            summarise(ymin = min(ymin), ymax = max(ymax))

        dots_ss <- setNames(list(
            lazyeval::interp(~n_nna(var), var = as.name(y)),
            lazyeval::interp(~unique_na(var), var = as.name(x))
        ), c('n', 'x'))
        data_ss <- data %>%
            group_by_(.dots = lapply(group_list, as.symbol)) %>%
            summarise_(.dots = dots_ss)

        dots_y_pos <- list(y = lazyeval::interp(
            ~min_y-ss_factor*(max_y-min_y)*(as.integer(unique_na(var_g))-1),
            min_y = as.name('ymin'), max_y = as.name('ymax'),
            var_g = if(is_blank(group)) 1 else as.name(group)
        ))
        if(length(group_list_ss) == 0) {
            data_ss$ymin <- yrange$ymin
            data_ss$ymax <- yrange$ymax
        } else {
            data_ss <- suppressMessages(left_join(data_ss, yrange))
        }
        data_ss <- data_ss %>% mutate_(.dots = dots_y_pos)

        # align y's in different panels
        if(!is_blank(facet_c)) {
            group_list_y <- c()
            if(!is_blank(facet_r)) group_list_y <- c(group_list_y, facet_r)
            if(!is_blank(group)) group_list_y <- c(group_list_y, group)
            data_ss <- data_ss %>%
                ungroup() %>%
                group_by_(.dots = lapply(group_list_y, as.symbol)) %>%
                mutate(y = min(y))
            if(facet_scale %in% c('fixed', 'free_x')) {
                data_ss <- data_ss %>%
                    ungroup() %>%
                    group_by_(group) %>%
                    mutate(y = min(y))
            }
        }

        if(y_log) data_ss$y <- 10^data_ss$y
        x_1 <- sort(unique_na(data_ss$x))[1]
        data_ss_1 <- filter(data_ss, x == x_1)
        plot_ <- plot_ +
            geom_text(data = data_ss_1, show.legend = FALSE,
                      aes(label = paste0('N=', data_ss_1$n), x = x, y = y),
                      size = fnote_size_ss, hjust = slightly_right_aligned)
        if(length(unique_na(data_ss$x)) > 1) {
            data_ss_rest <- filter(data_ss, x != x_1)
            plot_ <- plot_ +
                geom_text(data = data_ss_rest, show.legend = FALSE,
                          aes(label = data_ss_rest$n, x = x, y = y),
                          size = fnote_size_ss, hjust = center_aligned)
        }
    }

    if(!is_blank(x_tick_label) && (all_xticks || x_tick_label != x)) {
        x_ticks <- sort(unique_na(data[[x]]))
        x_tick_labels <- unique_na(data[[x_tick_label]][order(data[[x]])])
        plot_ <- plot_ +
            scale_x_continuous(breaks = x_ticks, labels = x_tick_labels)
    }

    if(return_data) {
        return(list(plot = plot_, data = data))
    } else return(plot_)

}






























