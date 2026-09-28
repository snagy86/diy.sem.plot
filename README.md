
<!-- README.md is generated from README.Rmd. Please edit that file -->

# diy.sem.plot

<!-- badges: start -->

[![R-CMD-check](https://github.com/snagy86/diy.sem.plot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/snagy86/diy.sem.plot/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/diy.sem.plot)](https://CRAN.R-project.org/package=diy.sem.plot)
[![Downloads](https://cranlogs.r-pkg.org/badges/diy.sem.plot)](https://cran.r-project.org/package=diy.sem.plot)
[![Total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/diy.sem.plot)](https://cran.r-project.org/package=diy.sem.plot)
<!-- badges: end -->

`diy.sem.plot` allows users to manually plot fully customisable path
diagrams for structural equation models (SEM) in R. It avoids the
inflexibility of automated SEM plotting tools and the tedium of drawing
diagrams in external applications.

Users manually specify node positions using x-y coordinates, and where
on the perimeter of each node (top, bottom, left, or right) a paths
should begin and end. From there, the primary function `diyPaths()`
renders the diagram, automatically inserting estimates centered on the
midpoint of paths and adjusting each node’s shape to match its variable
type. A range of fine-tuning options are included, facilitating the
creation of a path diagram exactly as envisioned, entirely within R.

## Installation

Install the released version of `diy.sem.plot` from CRAN:

``` r
install.packages("diy.sem.plot")
```

Or install the development version from GitHub to get the latest
features, including the `template()` helper function (not yet on CRAN):

``` r
install.packages("remotes")
remotes::install_github("snagy86/diy.sem.plot", build_vignettes = TRUE)
```

The vignettes in the development version have not been updated yet.

## Example

Below is a brief example of using the package. For more examples, plus a
full explanation of the workflow and available arguments, see the
package’s vignette.

In this example, I used `template()` to generate the starting code,
saving time and avoiding syntax errors. It takes the fitted `lavaan`
model and prints the node list, path list and `diyPaths()` call with
default values. I then edited these values to position everything. The
original printed output is omitted here to save space.

The arguments in `template()` let you specify how much code is printed.
For example, `diyPaths_arguments = "min"` prints only the necessary
`diyPaths()` arguments, `"max"` prints all of them, and
`include_variance_paths = FALSE` leaves out the variance paths.

``` r
library(lavaan)
library(diy.sem.plot)

sem_model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9

  speed ~ visual + textual
  visual ~~ textual
'

fit_sem <- sem(sem_model, data = HolzingerSwineford1939)

invisible(capture.output( # hiding default output
  template(fit = fit_sem, include_variance_paths = TRUE, diyPaths_arguments = "max"))) # template() prints its code to the console with cat()

node_list <- list(
    node(name = "visual", x = 1, y = 1, label = "Visual"),
    node(name = "textual", x = 1, y = 3, label = "Textual"),
    node(name = "speed", x = 4, y = 2, label = "Speed"),
    node(name = "x1", x = 0, y = -0.5, label = "Visual\nPerception"),
    node(name = "x2", x = 1, y = -0.5, label = "Cubes"),
    node(name = "x3", x = 2, y = -0.5, label = "Lozenges"),
    node(name = "x4", x = 0, y = 4.5, label = "Paragraph\nComprehension"),
    node(name = "x5", x = 1, y = 4.5, label = "Sentence\nCompletion"),
    node(name = "x6", x = 2, y = 4.5, label = "Word\nMeaning"),
    node(name = "x7", x = 6, y = 1.25, label = "Speeded\nAddition"),
    node(name = "x8", x = 6, y = 2, label = "Speeded\nCounting"),
    node(name = "x9", x = 6, y = 2.75, label = "Speeded\nDiscrimination")
)

path_list <- list(
    # regression paths
    path(from = "visual", to = "speed", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),
    path(from = "textual", to = "speed", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),

    # loading paths
    path(from = "visual", to = "x1", side_from = "bottom", side_to = "top", nudge_text_x = -0.1, nudge_text_y = 0),
    path(from = "visual", to = "x2", side_from = "bottom", side_to = "top", nudge_text_x = 0, nudge_text_y = 0),
    path(from = "visual", to = "x3", side_from = "bottom", side_to = "top", nudge_text_x = 0.1, nudge_text_y = 0),
    path(from = "textual", to = "x4", side_from = "top", side_to = "bottom", nudge_text_x = -0.1, nudge_text_y = 0),
    path(from = "textual", to = "x5", side_from = "top", side_to = "bottom", nudge_text_x = 0, nudge_text_y = 0),
    path(from = "textual", to = "x6", side_from = "top", side_to = "bottom", nudge_text_x = 0.1, nudge_text_y = 0),
    path(from = "speed", to = "x7", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = -0.1),
    path(from = "speed", to = "x8", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0),
    path(from = "speed", to = "x9", side_from = "right", side_to = "left", nudge_text_x = 0, nudge_text_y = 0.1),

    # covariance/correlation paths
    path(from = "textual", to = "visual", side_from = "left", side_to = "left", nudge_text_x = 0, nudge_text_y = 0, cov_curve = -0.6),

    # variance paths
    path(from = "x1", to = "x1", nudge_text_x = 0, nudge_text_y = 0, variance_position = "bottom"),
    path(from = "x2", to = "x2", nudge_text_x = 0, nudge_text_y = 0, variance_position = "bottom"),
    path(from = "x3", to = "x3", nudge_text_x = 0, nudge_text_y = 0, variance_position = "bottom"),
    path(from = "x4", to = "x4", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
    path(from = "x5", to = "x5", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
    path(from = "x6", to = "x6", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
    path(from = "x7", to = "x7", nudge_text_x = 0, nudge_text_y = 0, variance_position = "right"),
    path(from = "x8", to = "x8", nudge_text_x = 0, nudge_text_y = 0, variance_position = "right"),
    path(from = "x9", to = "x9", nudge_text_x = 0, nudge_text_y = 0, variance_position = "right"),
    path(from = "visual", to = "visual", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top"),
    path(from = "textual", to = "textual", nudge_text_x = 0, nudge_text_y = 0, variance_position = "bottom"),
    path(from = "speed", to = "speed", nudge_text_x = 0, nudge_text_y = 0, variance_position = "top")
)

my_sem_diagram <- diyPaths(fit = fit_sem, node_positions = node_list, path_positions = path_list,
                           standardised = TRUE,
                           digits = 3,
                           est_stars = TRUE,
                           est_p = FALSE,
                           est_ci = TRUE,
                           variance_stars = FALSE,
                           variance_p = FALSE,
                           variance_ci = FALSE,
                           sig_linetype = TRUE,
                           p_threshold = 0.05,
                           show_variances = TRUE,
                           show_grid = TRUE,
                           grid_axis_scale = 1,
                           non_transparent_text = TRUE,
                           latent_node_text_size = 6,
                           observed_node_text_size = 4.5,
                           path_text_size = 4.5,
                           line_thickness = 0.6,
                           arrow_size = 0.2,
                           node_width = 1.5, node_height = 1,
                           latent_node_size_adjust = 1,
                           observed_node_size_adjust = 0.5,
                           show_group_labels = FALSE,
                           panel_titles = NULL,
                           panel_cols = NULL,
                           margin_x = 0.3,
                           margin_y = 0.3,
                           look_up_table = TRUE)

print(my_sem_diagram)
```

## Coming soon

A larger update and fully updated vignettes for the `template()`
workflow are on the way. After that, I plan to add support for Bayesian
SEM models fitted with `blavaan`.
