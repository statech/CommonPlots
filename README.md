# CommonPlots

Overview
--------

CommonPlots is a package that allows the user to easily create a wide range of plots commonly used in statistical analysis in [ggplot2](http://ggplot2.org/) style.

Installation
------------
```r
# Install from GitHub:
# install.packages("devtools")
devtools::install_github(repo = "CommonPlots", username = "statech")
```

Usage
-----

Most of the plots supported in CommonPlots can be easily created through one function call. The created plot function can be further customized to meet specific needs with [ggplot2](http://ggplot2.org/) functions.

#### Barplot

```r
library(CommonPlots)
gg_barplot(mpg, var = 'class', group = 'cyl', add_counts = FALSE)
```
![Barplot](man/figures/barplot.png)

#### Gantt Chart

```r
test_df <- data.frame(
    project = rep(paste('Project', 1:10), each = 2),
    time = c(rbind(sample(1:5, 10, replace = TRUE),
                   sample(6:8, 10, replace = TRUE))),
    status = factor(c(rbind(rep('start', 10),
                            c(rep('end', 5), rep('ongoing', 5)))),
                    levels = c('start', 'ongoing', 'end'))
)
gg_gantt_chart(test_df, var = 'project', time = 'time',
               status = 'status', grids = 'y')
```
![Barplot](man/figures/gantt_chart.png)

Getting help
------------

The help page of CommonPlot package can be accessed by `help(package = 'CommonPlots')`. Most of the functions are documented and examples are provided to ease learning.


Contact
-------

If you have any questions or encounter any bugs, please contact the author (Feiyang Niu, Feiyang.Niu@gilead.com)
