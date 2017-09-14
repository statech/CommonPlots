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
#' @param group Character: name of a \code{data} column mapped to the fill of
#'  bars
#' @param group_levels Vector/List: a named vector/list that specifies the
#'  levels and labels of \code{group}
#' @param label Character: name of a \code{data} column used to label the points
#'  when both \code{add_points} and \code{add_label} are \code{TRUE}
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
#' @param y_log Logical: \code{TRUE} to use log-scale for y-axis and
#'  \code{FALSE} (default) otherwise.
#' @param add_points Logical: \code{TRUE} (default) to add original data points,
#'  jittered along x-axis
#' @param point_shape integer: valid point shape when \code{add_points} is
#'  \code{TRUE}. Refer to \href{http://sape.inf.usi.ch/quick-reference/ggplot2/shape}{ggplot2 Quick Reference: shape}
#'  for the details of valid point shape in \href{http://ggplot2.org/}{ggplot2}
#' @param add_legend Logical: \code{TRUE} (default) to show legend and
#'  \code{FALSE} otherwise
#' @param legend_pos Character: dictates the location where to place the legend.
#'  By default, the legend will be place beneath the actual plot
#' @param reference_hline Numeric vector: locations of horizontal reference
#'  line(s) if there is any
#' @param reference_vline Numeric vector: locations of vertical reference
#'  line(s) if there is any
#' @param test_func function: the function to be applied to perform statistical
#'  test of \code{y} variable against \code{group}, if present, or \code{x}, if
#'  \code{group} is not present but \code{x} is. Typically, equality test is of
#'  interest and includes
#'   * Student's t-Test (`test_func = t.test`)
#'   * Wilcoxon Rank Sum and Signed Rank Tests (`test_func = wilcox.test`)
#'   * ANOVA test (`test_func = CommonPlots::anova.test`)
#'   * Kruskal-Wallis Rank Sum Test (`test_func = kruskal.test`)
#'  In fact, arbitrary test function which takes 'formula' and 'data' as its
#'  first two arguments can be passed through \code{test_func}
#' @param test_result (named) String/List: Specify which component from the
#'  test, \code{test_func}, to be displayed in the box plot. Note that only one
#'  component is allowed and \code{test_result} must be a named component of the
#'  test result. When \code{test_func} is specified and \code{test_result} is
#'  omitted, the function will guess if the test result includes a component
#'  named 'p.value'. If there is, p value will be displayed; otherwise nothing
#'  gets displayed. As an example, if `test_result = c('P value:' = 'p.value')`,
#'  the string 'P value: {pvalue}' will be displayed in the box plot, where
#'  {pvalue} is the numeric p value. In the case of panel plot, the specified
#'  test will be carried out in each panel and result gets displayed
#'  respectively
#' @param x_tick_angle Numeric: the orientation angle (in [0, 360]) of the
#'  x-axis tick marks. By default, the label will be horizontal.
#' @param sample_size Logical: \code{TRUE} (default) to place sample size
#'  annotation along x-axis tick marks and \code{FALSE} otherwise
#' @param sample_size_font_size Numeric: font size of sample size annotation
#' @param add_label Logical: whether or not to add labels for the points
#'  Default is set to `FALSE`
#' @param is_repel Logical: whether or not to avoid text overlapping using
#'  \href{https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html}{ggrepel}
#' @param grids Character: grids option. Must be one of `c('on', 'major',
#'  'off')` with 'on' having both major and minor grids, 'major' having only
#'  major grids, and 'off' having no grids
#' @param randseed Numeric: random seed can be set in producing jittered points
#'  when \code{add_points} is \code{TRUE}
#'
#' @return An object of class ggplot (if \code{return_data = FALSE}) or a list
#'  of two components: an object of class ggplot and a data frame that used to
#'  generate the plot
#' @examples
#' library(CommonPlots)
#' data <- mpg
#' data$cyl <- factor(data$cyl)
#' boxplot_ <- gg_boxplot(
#'     data, x = 'cyl', y = 'displ', group = 'year',
#'     x_lab = 'Number of cylinders', y_lab = 'Engine displacement (L)',
#'     group_lab = 'Year', title = ''
#' )
#' # boxplot with Student's t-Test performed on engine displacement against year
#' boxplot_ <- gg_boxplot(
#'     data, x = 'cyl', y = 'displ', group = 'year', test_func = t.test,
#'     test_result = c('T test p value:' = 'p.value'),
#'     x_lab = 'Number of cylinders', y_lab = 'Engine displacement (L)',
#'     group_lab = 'Year', title = ''
#' )
#' boxplot_ <- gg_boxplot(
#'     diamonds, x = 'cut', y = 'price', group = 'clarity',
#'     group_levels = c('SI2', 'VS2', 'VVS2'), facet_r = 'color',
#'     facet_r_levels = c('Color = D' = 'D', 'Color = G' = 'G', 'Color = J' = 'J'),
#'     x_lab = 'Cut', y_lab = 'Price', group_lab = 'Clarity', title = '',
#'     sample_size_font_size = 4
#' )
#' # boxplot with ANOVA test on diamond price against clarify
#' boxplot_ <- gg_boxplot(
#'     diamonds, x = 'cut', y = 'price', group = 'clarity',
#'     group_levels = c('SI2', 'VS2', 'VVS2'), facet_r = 'color',
#'     facet_r_levels = c('Color = D' = 'D', 'Color = G' = 'G', 'Color = J' = 'J'),
#'     test_func = anova.test, test_result = c('ANOVA p value:' = 'p.value'),
#'     x_lab = 'Cut', y_lab = 'Price', group_lab = 'Clarity', title = '',
#'     sample_size_font_size = 4
#' )
#'
#'
#' @import dplyr
#' @export
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_boxplot <- function(data, x = NULL, y, group = NULL, group_levels = NULL,
                       label = NULL, facet_r = NULL, facet_c = NULL,
                       facet_r_levels = NULL, facet_c_levels = NULL,
                       facet_scale = 'free', facet_space = 'free',
                       x_lab = x, y_lab = y, group_lab = group, title = '',
                       x_limit = NULL, y_limit = NULL, y_log = FALSE,
                       add_points = TRUE, point_shape = 19,
                       add_legend = TRUE, legend_pos = 'bottom',
                       reference_hline = NULL, reference_vline = NULL,
                       test_func = NULL, test_result = c('p value' = 'p.value'),
                       x_tick_angle = 0, sample_size = TRUE,
                       sample_size_font_size = 3, add_label = FALSE,
                       is_repel = FALSE, bw_theme = TRUE, grids = 'on',
                       randseed = 12345) {

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
    has_x_var <- !is_blank(x)
    if(has_x_var) column_in_dataframe(data, x)
    column_in_dataframe(data, y)
    if(!is_blank(label)) column_in_dataframe(data, label)
    if(!is_blank(facet_r)) column_in_dataframe(data, facet_r)
    if(!is_blank(facet_c)) column_in_dataframe(data, facet_c)
    if(!is_blank(group)) column_in_dataframe(data, group)
    if(!is_blank(x_limit)) check_var_class(x_limit, is.numeric, 'numeric')
    if(!is_blank(y_limit)) check_var_class(y_limit, is.numeric, 'numeric')
    y_log <- isTRUE(y_log)
    add_points <- isTRUE(add_points)
    add_legend <- isTRUE(add_legend)
    sample_size <- isTRUE(sample_size)
    add_label <- isTRUE(add_label)
    bw_theme <- isTRUE(bw_theme)
    arg_in_choices(facet_scale, c('free', 'free_x', 'free_y', 'fixed'))
    arg_in_choices(facet_space, c('free', 'free_x', 'free_y', 'fixed'))
    if(add_legend) arg_in_choices(legend_pos, c('left', 'right', 'bottom', 'up'))
    if(!is_blank(reference_hline))
        check_var_class(reference_hline, is.numeric, 'numeric')
    if(!is_blank(reference_vline))
        check_var_class(reference_vline, is.numeric, 'numeric')
    arg_in_choices(grids, c('on', 'x', 'y', 'off'))


    #-----------------------------
    # define constants and vars
    #-----------------------------
    alignment_dict <- list(
        'left' = 0, 'center' = 0.5, 'right' = 1,
        'up' = 1, 'bottom' = 0
    )
    # create x as constant 1L if not specified
    if(!has_x_var) {
        x <- 'x_var'
        data[[x]] <- factor(1)
    }
    # convert x to numeric if not
    if(!is.numeric(data[[x]])) {
        data[[x]] <- factor(data[[x]])
        x_tick_labels <- levels(data[[x]])
        data[[x]] <- as.numeric(data[[x]])
        x_axis_breaks <- sort(unique_na(data[[x]]))
        # default expand on discrete x-axis
        x_expand <- c(0, 0.3)
    } else {
        x_axis_breaks <- NULL
        x_tick_labels <- NULL
        # default expand on continuous x-axis
        x_expand <- c(0.05, 0)
    }
    group_list <- NULL
    if(!is_blank(facet_r)) group_list <- c(group_list, facet_r)
    if(!is_blank(facet_c)) group_list <- c(group_list, facet_c)
    if(!is_blank(group)) group_list <- c(group_list, group)
    group_list <- unique(c(group_list, x))
    group_cols <- data[, group_list, drop = FALSE]
    data <- mutate(data, x_var_factor = do.call(interaction, as.list(group_cols)))
    dodge_boxplot_factor <- 0.75
    resolution_x <- ggplot2::resolution(
        sort(unique_na(data[[x]])), zero = FALSE
    )
    dodge_width <- resolution_x * dodge_boxplot_factor
    outlier_shape <- ifelse(add_points, NA, point_shape)
    x_point_var <- 'x_point_'
    data[[x_point_var]] <- as.integer(data[[x]])


    #---------------------------
    # data manipulation
    #---------------------------
    ngroups <- 1
    if(!is_blank(group)) {
        group_levels <- unlist(group_levels)
        if(is.null(group_levels)) group_levels <- sort(unique(data[[group]]))
        data[[group]] <- factor(data[[group]], levels = group_levels)
        data <- data[!is.na(data[[group]]), , drop = F]
        if(!is.null(names(group_levels))) {
            levels(data[[group]]) <- names(group_levels)
        }
        ngroups <- nlevels(data[[group]])
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

    if(ngroups > 1) {
        shift <- (as.integer(data[[group]]) - median(1:ngroups)) / ngroups
        data[[x_point_var]] <- data[[x_point_var]] + shift * dodge_width
    }
    if(add_points) {
        jitter_factor <- 0.2
        data[[x_point_var]] <- gg_jitter(
            data[[x_point_var]], dodge_width * jitter_factor, randseed
        )
    }


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
        x_log = FALSE, y_log = y_log,
        x_axis_breaks = x_axis_breaks, x_tick_labels = x_tick_labels,
        x_tick_angle = x_tick_angle,
        add_legend = add_legend, legend_pos = legend_pos,
        color_var = group, color_lab = group_lab,
        reference_hline = reference_hline, reference_vline = reference_vline,
        bw_theme = bw_theme, grids = grids, x_expand = x_expand
    )

    plot_ <- plot_ +
        geom_boxplot(aes(group = x_var_factor), outlier.shape = outlier_shape,
                     position = position_dodge(dodge_width), alpha = 0.5)

    # add points
    if(add_points) {
        point_size <- 1
        point_alpha <- 0.5
        plot_ <- plot_ +
            geom_point(aes_string(x = x_point_var), shape = point_shape,
                       size = point_size, alpha = point_alpha)

        # add labels to the points
        if(add_label && !is_blank(label)) {

            if(is_repel) {
                plot_ <- plot_ +
                    ggrepel::geom_text_repel(
                        aes_string(x = x_point_var, label = label),
                        show.legend = FALSE
                    )
            } else {
                plot_ <- plot_ +
                    geom_text(aes_string(x = x_point_var, label = label),
                              vjust = 'inward', hjust = 'inward',
                              show.legend = FALSE)
            }
        }
    }

    # add statistical test result
    cond_test <- (!is.null(x) && length(unique_na(data[[x]])) > 1) ||
        (!is.null(group) && length(unique_na(data[[group]])) > 1)
    add_test_result <- FALSE
    if(cond_test && !is_blank(test_func)) {
        if(!is.function(test_func))
            print('Warning: `test_func` must be a valid function')
        else {
            group_test <- NULL
            if(!is_blank(facet_r)) group_test <- c(group_test, facet_r)
            if(!is_blank(facet_c)) group_test <- c(group_test, facet_c)
            test_on_x <- FALSE
            if(length(unique_na(data[[x]])) > 1 &&
               !is_blank(group) && length(unique_na(data[[group]])) > 1) {
                group_test <- c(group_test, x)
                test_on_x <- TRUE
            }
            factor_test <- ifelse(
                !is.null(group) && length(unique_na(data[[group]])) > 1, group, x
            )
            formula_test <- formula(paste(y, '~', factor_test))
            results_test <- data %>%
                group_by_(.dots = lapply(group_test, as.symbol)) %>%
                do(test = tryCatch(
                    suppressMessages(test_func(formula_test, data = .)),
                    error = function(e) {NA}
                ))
            if(length(test_result) > 1) {
                print('Warning: `test_result` can only be of length 1')
            } else {
                if(is.null(test_result)) {
                    test_result <- setNames(
                        'p.value',
                        paste(as.character(substitute(test_func)), 'p value:')
                    )
                }
                if(is_blank(group_test)) {
                    res_expr <- lazyeval::interp(
                        ~tryCatch(var[[1]][[test_result]], error = function(e){NA}),
                        var = as.name('test')
                    )
                } else {
                    res_expr <- lazyeval::interp(
                        ~tryCatch(var[[test_result]], error = function(e){NA}),
                        var = as.name('test')
                    )
                }
                if(test_on_x)
                    x_expr <- lazyeval::interp(~as.integer(var), var = as.name(x))
                else x_expr <- ~(-Inf)
                dots_summarise <- setNames(c(
                    as.list(group_test), list(res_expr, x_expr, ~(Inf))
                ), c(group_test, 'result_', 'x_', 'y_'))
                results_test <-  results_test %>%
                    summarise_(.dots = dots_summarise)
                if(all(is.na(results_test$result_))) {
                    print('Warning: `test_result` is not found in the test results')
                } else {
                    results_test <- mutate(results_test, result_ = trimws(paste(
                        names(test_result), round(result_, 3)
                    )))
                    add_test_result <- TRUE
                }
            }
        }
    }
    if(add_test_result) {
        if(test_on_x) hjust = 0.5 else hjust = -0.2
        plot_ <- plot_ +
            geom_text(data = results_test,
                      aes(x_, y_, label = result_),
                      inherit.aes = FALSE,
                      hjust = hjust, vjust = 1.5)
    }


    # add sample size
    if(sample_size) {
        nrows <- 1
        if(!is_blank(facet_r)) nrows <- nlevels(factor(data[[facet_r]]))

        # for sample size annotation
        ss_factor <- 0.04 * sample_size_font_size / 3
        fnote_size_ss <- sample_size_font_size / (1 + 0.5 * (nrows - 1))
        center_aligned <- 0.5
        slightly_right_aligned <- 0.7

        group_list_ss <- c()
        if(!is_blank(facet_r)) group_list_ss <- c(group_list_ss, facet_r)
        if(!is_blank(facet_c)) group_list_ss <- c(group_list_ss, facet_c)
        yrange <- data %>%
            group_by_(.dots = lapply(group_list, as.symbol)) %>%
            do(res = grDevices::extendrange(
                if(y_log) log(range_na(.[[y]]), base = 10)
                else range_na(.[[y]])
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
            ~min_y-ss_factor * (max_y-min_y) * (as.integer(unique_na(var_g))-1),
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

    if(!has_x_var) {
        plot_ <- plot_ +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
    }

    return(plot_)
}






















