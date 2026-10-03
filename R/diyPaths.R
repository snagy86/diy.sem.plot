utils::globalVariables(c(
  "xmin", "xmax", "ymin", "ymax", "x", "y", "label",
  "a", "b", "xend", "yend", "linetype", "mid_x", "mid_y",
  "label_text", "note_text", ".data"
))

#' Create a template for use in diyPaths
#'
#' @description
#'
#' Takes your fitted model and prints a ready-to-run starting template (node list, path list, title list, and
#' `diyPaths()` call) to the console. Every variable and path from your model is already filled in
#' with the correct names, so the code will render a diagram as soon as you run it. All arguments will be at their default values,
#' so your job is to fine-tune: copy the output into your script, then edit the lines marked `#EDIT ME!`.
#'
#' The template is also returned invisibly as a character string, so it can be stored as an object then viewed
#' with `writeLines()` (see examples).
#'
#' @param fit the `lavaan` object of fitted model
#' @param include_variance_paths Logical. Whether to include specification for variance/residual paths in paths_lists
#' @param diyPaths_arguments Use either "max" (the printed call shows every [diyPaths()] argument) or "min" (only the required arguments). Default is "max"
#' @return Invisibly returns the template as a single character string, and prints it to the console.
#' @seealso [diyPaths()] to render the diagram from the template;
#'   [node()], [path()], [panel_title()], and [panel_note()] for the helper functions the template uses.
#' @export
#'
#' @examples
#'
#' library(lavaan)
#' library(ggplot2)
#'
#' data(HolzingerSwineford1939, package = "lavaan")
#'
#'
#' #specify the model
#'
#' sem_model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#'
#'   speed ~ visual + textual
#'   visual ~~ textual
#' '
#'
#' #fit the model
#'
#' fit <- sem(sem_model, data = HolzingerSwineford1939)
#'
#' #print the template to the console
#'
#' # template(fit = fit, include_variance_paths = TRUE, diyPaths_arguments = "max")
#'
#' #store the template, then view it properly formatted
#'
#' tmpl <- template(fit = fit, diyPaths_arguments = "min")
#' writeLines(tmpl)

template <- function(fit, include_variance_paths = TRUE, diyPaths_arguments = c("max", "min")) {

  pe_full <- lavaan::parameterestimates(fit)
  pe_full <- unique(pe_full[, colnames(pe_full) %in% c("lhs", "op", "rhs")])

  fit_name <- deparse(substitute(fit))
  diyPaths_arguments <- match.arg(diyPaths_arguments)

  nodes <- data.frame(node = unique(pe_full$lhs))

  node_paste <- data.frame(output = paste0(
    '\t', 'node(',
    'name = "', nodes$node, '"', ', ',
    'x = 0, ',
    'y = 0, ',
    'label = "', nodes$node, '"', ')',
    ifelse(seq_len(nrow(nodes)) < nrow(nodes), ',', ''),
    ' #EDIT ME!'
  ))

  node_string <- paste0(
    "#### BEGIN diyPaths CODE ####\n\n",
    "node_list <- list(\n",
    paste(node_paste$output, collapse = "\n"),
    "\n)"
  )

  is_loading <- pe_full$op == "=~"
  is_reg <- pe_full$op == "~"
  is_cov <- pe_full$op == "~~" & pe_full$lhs != pe_full$rhs
  is_var <- pe_full$op == "~~" & pe_full$lhs == pe_full$rhs

  reg_paths <- data.frame(
    from = rep(NA, sum(is_reg)),
    to = rep(NA, sum(is_reg))
  )

  reg_paths$from <- pe_full$rhs[is_reg]
  reg_paths$to <- pe_full$lhs[is_reg]

  reg_paste <- data.frame(output = paste0('\t',
                                          'path(', 'from = "', reg_paths$from, '"', ', ', 'to = "', reg_paths$to, '"', ', ',
                                          'side_from = "right" ', ', ', 'side_to = "left"', ', ', "nudge_text_x = 0", ', ', "nudge_text_y = 0", ')',
                                          ',',
                                          ' #EDIT ME!'
  ))


  loading_paths <- data.frame(
    from = rep(NA, sum(is_loading)),
    to = rep(NA, sum(is_loading))
  )

  loading_paths$from <- pe_full$lhs[is_loading]
  loading_paths$to <- pe_full$rhs[is_loading]

  loading_paste <- data.frame(output = paste0('\t',
                                              'path(', 'from = "', loading_paths$from, '"', ', ', 'to = "', loading_paths$to, '"', ', ',
                                              'side_from = "right" ', ', ', 'side_to = "left"', ', ', "nudge_text_x = 0", ', ', "nudge_text_y = 0", ')',
                                              ',',
                                              ' #EDIT ME!'
  ))

  cov_paths <- data.frame(
    from = rep(NA, sum(is_cov)),
    to = rep(NA, sum(is_cov))
  )

  cov_paths$from <- pe_full$rhs[is_cov]
  cov_paths$to <- pe_full$lhs[is_cov]

  cov_paste <- data.frame(output = paste0('\t',
                                          'path(', 'from = "', cov_paths$from, '"', ', ', 'to = "', cov_paths$to, '"', ', ',
                                          'side_from = "right" ', ', ', 'side_to = "left"', ', ', "nudge_text_x = 0", ', ', "nudge_text_y = 0", ', ', 'cov_curve = 0.4', ')',
                                          ',',
                                          ' #EDIT ME!'
  ))

  variance_paths <- data.frame(
    from = rep(NA, sum(is_var)),
    to = rep(NA, sum(is_var))
  )

  variance_paths$from <- pe_full$rhs[is_var]
  variance_paths$to <- pe_full$lhs[is_var]

  variance_paste <- data.frame(output = paste0('\t',
                                               'path(', 'from = "', variance_paths$from, '"', ', ', 'to = "', variance_paths$to, '"', ', ',
                                               "nudge_text_x = 0", ', ', "nudge_text_y = 0", ', ', 'variance_position = "top"', ')',
                                               ',',
                                               ' #EDIT ME!'
  ))

  reg_section <- if (nrow(reg_paths) > 0) paste0('\t',"# regression paths\n", paste(reg_paste$output, collapse = "\n"), "\n") else ""

  loading_section <- if (nrow(loading_paths) > 0) paste0('\t',"# loading paths\n", paste(loading_paste$output, collapse = "\n"), "\n") else ""

  cov_section <- if (nrow(cov_paths) > 0) paste0('\t',"# covariance/correlation paths\n", paste(cov_paste$output, collapse = "\n"), "\n") else ""

  variance_section <- if (include_variance_paths && nrow(variance_paths) > 0) paste0('\t',"# variance paths REMEMBER TO SET show_variances IN diyPaths CALL = TRUE!\n", paste(variance_paste$output, collapse = "\n"), "\n") else ""

  path_string <- paste0("\n",
                      "path_list <- list(\n",
                      reg_section, "\n",
                      loading_section, "\n",
                      cov_section, "\n",
                      variance_section,
                      "\n)"
  )


  path_string <- sub(",(\\s*#EDIT ME!\\s*\\n\\))", "\\1", path_string)

  n_groups <- lavaan::lavInspect(fit, "ngroups")

  title_lines <- paste0(
    '\t', 'panel_title(panel_num = ', seq_len(n_groups), ', title = "My SEM Plot")',
    ifelse(seq_len(n_groups) < n_groups, ',', ''),
    ' #EDIT ME!'
  )

  title_string <- paste0(
    "title_list <- list(\n",
    paste(title_lines, collapse = "\n"),
    "\n)"
  )

  note_lines <- paste0(
    '\t', 'panel_note(panel_num = ', seq_len(n_groups), ', note_text = NULL, x = 0, y = 0)',
    ifelse(seq_len(n_groups) < n_groups, ',', ''),
    ' #EDIT ME!'
  )

  note_string <- paste0(
    "note_list <- list(\n",
    paste(note_lines, collapse = "\n"),
    "\n)"
  )

  diyPaths_arguments_all_args <- paste0(
    "my_path_diagram <- diyPaths(fit = ", fit_name, ", ", "node_positions = node_list, path_positions = path_list,
                           standardised = FALSE,
                           digits = 3,
                           est_stars = FALSE,
                           est_p = FALSE,
                           est_ci = FALSE,
                           show_variances = FALSE,
                           variance_stars = FALSE,
                           variance_p = FALSE,
                           variance_ci = FALSE,
                           sig_linetype = FALSE,
                           p_threshold = 0.05,
                           node_width = 1.5, node_height = 1,
                           latent_node_size_adjust = 1,
                           observed_node_size_adjust = 1,
                           latent_node_text_size = 4,
                           observed_node_text_size = 4,
                           path_transparent_text = FALSE,
                           path_text_size = 3.5,
                           line_thickness = 0.6,
                           arrow_size = 0.2,
                           panel_cols = NULL,
                           show_group_labels = FALSE,
                           title_text_size = 25,
                           panel_titles = title_list,
                           panel_notes = note_list,
                           note_text_size = 4,
                           note_transparent_text = FALSE,
                           note_outline = TRUE,
                           show_grid = FALSE,
                           grid_axis_scale = 1,
                           margin_x = 0.5,
                           margin_y = 0.5,
                           look_up_table = FALSE)
my_path_diagram
#### END diyPaths CODE :) ####"
  )


  diyPaths_arguments_minimal_args <- paste0(
    "my_path_diagram <- diyPaths(fit = ", fit_name, ",
                            node_positions = node_list,
                            path_positions = path_list,
                            panel_titles = title_list,
                            panel_notes = note_list)
my_path_diagram
#### END diyPaths CODE :) ####")

  which_template <- if (diyPaths_arguments == "max") diyPaths_arguments_all_args else diyPaths_arguments_minimal_args

  template_text <- paste(node_string, path_string, title_string, note_string,  which_template, sep = "\n\n")
  cat(template_text, "\n")
  invisible(template_text)
}


#' Create a node position list
#'
#' @description
#' Helper function that creates a list of arguments which specify a given node's position and its name in the diagram,
#' designed for use within the `node_positions` argument of [diyPaths()].
#'
#' @param name The name of the variable within the `lavaan` model.
#' @param x Numeric value for the x-coordinate of the node's centre. Default is `0`.
#' @param y Numeric value for the y-coordinate of the node's centre. Default is `0`.
#' @param label Custom label to display instead of `lavaan` variable name. Defaults to the `lavaan` variable name.
#'
#'#' @seealso [path()] to specify the paths between nodes;
#'   [diyPaths()] to render the diagram; [template()] to generate a starting `node_positions` list.
#'
#' @return A list containing arguments that specify a node's position and label, for use within the `node_positions` argument of [diyPaths()].
#' @export
#'
#' @examples
#'
#' #a list that specifies a node positioned on x = 1, y = 2,
#' #and relabelled from its `lavaan` model name.
#'
#' node(name = "bpm", x = 1, y = 2,  label = "Beats per Minute")
#'

node <- function(name, x = 0, y = 0, label = NULL) {
  list(
    name = as.character(name),
    x = as.numeric(x),
    y = as.numeric(y),
    label = if (is.null(label)) as.character(name) else as.character(label)
  )
}

#' Create a path position list
#'
#' @description
#' Helper function that creates a list of arguments which specify a given path's position and fine-tuning adjustments,
#' designed for use within the `path_positions` argument of [diyPaths()].
#'
#' @param from The source variable name within the `lavaan` model.
#' @param to The target variable name within the `lavaan` model.
#' @param side_from Side of source node where path starts ("top", "bottom", "left", "right"). Default is "right".
#' @param side_to Side of target node where path ends ("top", "bottom", "left", "right"). Default is "left".
#' @param cov_curve Numeric value for curvature of covariance/correlation paths. Default is NULL.
#' @param nudge_text_x Numeric fine tuning adjustment for path estimate text along the x-axis. Default is `0`.
#' @param nudge_text_y Numeric fine tuning adjustment for path estimate text along the y-axis. Default is `0`.
#' @param variance_position Placement of variance/residual paths ("top", "bottom", "left", or "right"). Default is "top".
#'
#' @details
#'
#' `from`/`to` in `path()` must exactly match the variable name used in the `lavaan` model syntax.
#'  Furthermore, the order must also be correct for regression or loading paths. A misspelled or mismatched
#'  `path()` entry will not raise an error, instead, it will use default attachment points and values, not applying
#'   specific customisations. This can be completely avoided using [template()].
#'
#' `cov_curve`'s value can be used to adjust direction of curve on covariance/correlation path.
#'  For a mostly vertical path (i.e. node1: x = 0, y = 1 -> node2: x = 0, y = 2), positive curvature bends it left and negative curvature
#'  bends it right. For a mostly horizontal path (i.e. node1: x = 1, y = 0 -> node2: x = 2, y = 0), positive curvature bends
#'  it down and negative curvature bends it up. Best results typically range from -1 to 1.
#'
#' @seealso [node()] to specify node positions;
#'   [diyPaths()] to render the diagram; [template()] to generate a starting `path_positions` list.
#'
#' @examples
#'
#' #a list that specifies a regression path of node "anxiety" predicting node "depression".
#'
#' path(from = "anx", to = "dep", side_from = "right", side_to = "left")
#'
#' #a list that specifies a covariance/correlation path of node "anxiety" and node "depression".
#' #Order of "from" and "to" does not matter for covariance/correlation.
#'
#'
#' path(from = "dep", to = "anx", side_from = "left", side_to = "left", cov_curve = -0.6)
#'
#' @return A list containing arguments that specify a path's position and fine-tuning adjustments, for use within the `path_positions` argument of [diyPaths()].
#' @export

path <- function(from, to, side_from = "right", side_to = "left", cov_curve = NULL,
                 nudge_text_x = 0, nudge_text_y = 0, variance_position = "top") {
  list(
    from = as.character(from),
    to = as.character(to),
    side_from = side_from,
    side_to = side_to,
    nudge_text_x = nudge_text_x,
    nudge_text_y = nudge_text_y,
    cov_curve = cov_curve,
    variance_position = variance_position
  )
}

#' Create a note for a diyPaths panel
#'
#' @description
#' Helper function that creates a list of arguments which specify a custom note and its target panel,
#' designed for use within the `panel_notes` argument of [diyPaths()]. Use the
#' `show_group_labels` argument in [diyPaths()] to view each panel's number and
#' which group it refers to.
#'
#' @param panel_num Integer value for the panel number this note applies to.
#' @param note_text Text to display in the note. If `NULL`, no note is drawn.
#' @param x Numeric value for the x-coordinate of the note. Default is `0`.
#' @param y Numeric value for the y-coordinate of the note. Default is `0`.
#'
#' @seealso [diyPaths()], where the list of notes is passed to `panel_notes`;
#' [panel_title()] for adding titles to panels; [template()], whose output includes a starting note list.
#'
#' @return A list containing arguments that specify a panel's note text and position, for use within the `panel_notes` argument of [diyPaths()].
#' @export
#'
#'
#' @examples
#' # a note on the first panel, placed at x = 2.5, y = 0
#' panel_note(panel_num = 1, note_text = "n = 100", x = 2.5, y = 0)
#'
#' # a note on the second panel of a multi-group model
#' panel_note(panel_num = 2, note_text = "Source: my data", x = 2.5, y = 0)
#'

panel_note <- function(panel_num, note_text = NULL, x = 0, y = 0) {
  list(panel_num = as.integer(panel_num),
       note_text = if (is.null(note_text)) NA_character_ else as.character(note_text),
       x = as.numeric(x),
       y = as.numeric(y))
}

#' Create a title for a diyPaths panel
#'
#' @description
#' Helper function that creates a list of arguments which specify custom titles and its target panel,
#' designed for use within the `panel_titles` argument of [diyPaths()]. Use the
#' `show_group_labels` argument in [diyPaths()] to view each panel's number and
#' which group it refers to.
#'
#' @param panel_num Integer value for the panel number this title applies to. Default is `1`.
#' @param title The title text to display.
#'
#' @seealso [diyPaths()], where the list of titles is passed to `panel_titles`;
#'   [panel_note()] for adding notes to panels; [template()], whose output includes a starting title list.
#'
#' @return A list containing arguments that specify a panel's number and title, for use within the `panel_titles` argument of [diyPaths()].
#' @export
#'
#' @examples
#' # titling a single, non-grouped model
#' panel_title(title = "My SEM Model")
#'
#' # titling the second panel of a multi-group model
#' panel_title(panel_num = 2, title = "Female Participants")



panel_title <- function(panel_num = 1, title = NULL) {
  list(panel_num = as.integer(panel_num),
       title = if (is.null(title)) NA_character_ else as.character(title))
}

#' Manually plot path diagrams for structural equation models
#'
#' @description
#'
#' Plot fully customisable path diagrams for structural equation models fitted with \pkg{lavaan}, rendered using \pkg{ggplot2}.
#'
#' To render the `ggplot` object of the diagram, the function only requires users to supply a fitted `lavaan` model, specify node positions using x-y coordinates, and detail where on
#' the perimeter of each node (top, bottom, left, or right) each path should begin and end. The render automatically inserts estimates centered on the midpoint and adjusts each node's
#' shape to match its variable type. A range of optional fine-tuning arguments are included, facilitating the creation of a path diagram exactly as you envision it, entirely within R.
#'
#'
#' @param fit A fitted model object of class `lavaan`.
#' @param node_positions Specify a list of node position objects. Use the [node()] helper function to assist with this.
#' @param path_positions Specify a list of path configuration objects. Use the [path()] helper function to assist with this.
#' @param standardised Logical. If `TRUE`, uses standardised parameter estimates (`est.std`). Default is `FALSE`.
#' @param digits Number of digits to display for estimates. Default is `3`.
#' @param est_stars Logical. Whether to display significance stars on path estimates. Default is `FALSE`.
#' @param est_p Logical. Whether to display p-values on path estimates. Default is `FALSE`.
#' @param est_ci Logical. Whether to display confidence intervals on path estimates. Default is `FALSE`.
#' @param show_variances Logical. Whether to display variance and residual paths. Default is `FALSE`.
#' @param variance_stars Logical. Whether to display significance stars on variance paths. Default is `FALSE`.
#' @param variance_p Logical. Whether to display p-values on variance paths. Default is `FALSE`.
#' @param variance_ci Logical. Whether to display confidence intervals on variance paths. Default is `FALSE`.
#' @param sig_linetype Logical. If `TRUE`, renders non-significant paths with dashed lines. Default is `FALSE`.
#' @param p_threshold Statistical significance threshold used to determine non-significant paths when `sig_linetype = TRUE`. Default is `0.05`.
#' @param node_width Base width for nodes. Default is `1.5`.
#' @param node_height Base height for node shapes. Default is `1`.
#' @param latent_node_size_adjust Numeric multiplier scaling latent node ellipses. Default is `1`.
#' @param observed_node_size_adjust Numeric multiplier scaling observed node rectangles. Default is `1`.
#' @param latent_node_text_size Text font size for latent node labels. Default is `4`.
#' @param observed_node_text_size Text font size for observed node labels. Default is `4`.
#' @param path_transparent_text Logical. If `TRUE`, path estimate labels have a transparent background. If `FALSE`, they receive a white background mask. Default is `FALSE`.
#' @param path_text_size Text font size for path estimate labels. Default is `3.5`.
#' @param line_thickness Sets thickness of paths. Default is `0.6`.
#' @param arrow_size Sets the size of arrow heads. Default is `0.2`.
#' @param panel_cols Integer for number of columns to use when arranging multi-group panels. Default is `NULL`.
#' @param show_group_labels Logical. For multi-group models, whether to annotate each panel
#'   with its panel number and the raw group value it represents (e.g. "Panel 1: Group = male").
#'   Needed to identify which diagram represents each group and its internal panel number when creating panel titles and notes.
#'   Default is `FALSE`.
#' @param title_text_size Font size for panel titles. Default is `25`.
#' @param panel_titles Specify a list of titles for panels. Use the [panel_title()] helper function to assist with this. Any panel not referenced is
#'   left untitled. Default is `NULL`.
#' @param panel_notes Specify a list of notes and their positions for panels. Use the [panel_note()] helper function to assist with this. Any panel not referenced is
#'   left without a note. Notes placed outside the plot limits are clipped, so increase `margin_x`/`margin_y` if needed. Default is `NULL`.
#' @param note_text_size Font size for panel notes. Default is `4`.
#' @param note_transparent_text Logical. If `TRUE`, panel notes have a transparent background. If `FALSE`, they receive a white background mask. Default is `FALSE`.
#' @param note_outline Logical. If `TRUE`, panel notes receive a black outline. Default is `TRUE`.
#' @param show_grid Logical. Whether to overlay a coordinate grid. Default is `FALSE`.
#' @param grid_axis_scale Sets the spacing of grid-lines when `show_grid = TRUE`. Default is `1`.
#' @param margin_x Padding for plot limits along the x-axis. Default is `0.5`.
#' @param margin_y Padding for plot limits along the y-axis. Default is `0.5`.
#' @param look_up_table Logical. If `TRUE`, also returns a look-up table detailing the width (x scale) and height (y scale) of latent and observed nodes,
#'   along with the text sizes used for latent, observed, and path labels. Default is `FALSE`.
#'
#'@details
#'
#' Using the function requires 4 steps and is illustrated by the example below. To save time and avoid syntax/logical errors, [template()] prints starting code
#' for `node_positions`, `path_positions`, `panel_titles` and the `diyPaths()` call based on your fitted model.
#'
#' See `vignette("diy.sem.plot")` for in-depth examples and guidance on using the function's arguments.
#'
#' 1. Specify and fit the SEM using \pkg{lavaan}.
#'
#' 2. Specify `node_positions` as a list (`node_positions = list(...)`) and,
#'    within it, define each node's position and label using the [node()]
#'    helper function.
#'
#' 3. Specify `path_positions` as a list (`path_positions = list(...)`) and,
#'    within it, define each path's connection points, curvature if covariance/correlations, and fine-tune estimate positions
#'    adjustments using the [path()] helper function.
#'
#' 4. Call `diyPaths()`, passing in the fitted model, `node_positions`,
#'    and `path_positions`, along with any additional display augmentations (i.e. show significance stars on estimates).
#'
#' Creating a diagram for a specific model is inherently an iterative process. As such, steps 2-4 will likely need to be repeated
#' with additional fine-tuning adjustments to achieve the desired result.
#'
#' @return A `ggplot` (or `patchwork`, for multi-group models) object representing the SEM
#'   path diagram(s). If `look_up_table = TRUE`, a list containing the diagram(s) (`$plot`) and a look-up
#'   `data.frame` of node width/height and text sizes for latent, observed, and path labels
#'   (`$look_up_table`).
#'
#' @seealso [template()] to generate starting code for `node_positions`, `path_positions`, `panel_titles` and `panel_notes`;
#'   [node()], [path()], [panel_title()], and [panel_note()] for the helper functions used within them.
#'
#' @export
#'
#' @examples
#'
#' #full SEM example, this model may not make theoretical sense.
#'
#' library(lavaan)
#' library(ggplot2)
#'
#' data(HolzingerSwineford1939, package = "lavaan")
#'
#' #specify the model
#'
#' sem_model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#'
#'   speed ~ visual + textual
#'   visual ~~ textual
#' '
#'
#' #fit the model
#'
#' fit <- sem(sem_model, data = HolzingerSwineford1939)
#'
#' #generate a starting template
#'
#' temp <- template(fit = fit, diyPaths_arguments = "max")
#'
#' #### edited template ####
#'
#' node_list <- list(
#'   node(name = "visual", x = 1, y = 1, label = "Visual"),
#'   node(name = "textual", x = 1, y = 2, label = "Textual"),
#'   node(name = "speed", x = 4, y = 1.5, label = "Speed"),
#'   node(name = "x1", x = -0.06, y = -0.5, label = "Visual\nPerception"),
#'   node(name = "x2", x = 1, y = -0.5, label = "Cubes"),
#'   node(name = "x3", x = 2.06, y = -0.5, label = "Lozenges"),
#'   node(name = "x4", x = -0.06, y = 3.5, label = "Paragraph\nComprehension"),
#'   node(name = "x5", x = 1, y = 3.5, label = "Sentence\nCompletion"),
#'   node(name = "x6", x = 2.06, y = 3.5, label = "Word\nMeaning"),
#'   node(name = "x7", x = 6, y = 0.5, label = "Speeded\nAddition"),
#'   node(name = "x8", x = 6, y = 1.5, label = "Speeded\nCounting"),
#'   node(name = "x9", x = 6, y = 2.5, label = "Speeded\nDiscrimination")
#' )
#'
#' path_list <- list(
#'   # regression paths
#'   path(from = "visual", to = "speed", side_from = "right", side_to = "left"),
#'   path(from = "textual", to = "speed", side_from = "right", side_to = "left"),
#'
#'   # loading paths
#'   path(from = "visual", to = "x1", side_from = "bottom", side_to = "top", nudge_text_x = -0.1),
#'   path(from = "visual", to = "x2", side_from = "bottom", side_to = "top"),
#'   path(from = "visual", to = "x3", side_from = "bottom", side_to = "top", nudge_text_x = 0.1),
#'   path(from = "textual", to = "x4", side_from = "top", side_to = "bottom", nudge_text_x = -0.1),
#'   path(from = "textual", to = "x5", side_from = "top", side_to = "bottom"),
#'   path(from = "textual", to = "x6", side_from = "top", side_to = "bottom", nudge_text_x = 0.1),
#'   path(from = "speed", to = "x7", side_from = "right", side_to = "left"),
#'   path(from = "speed", to = "x8", side_from = "right", side_to = "left"),
#'   path(from = "speed", to = "x9", side_from = "right", side_to = "left"),
#'
#'   # covariance/correlation paths
#'   path(from = "visual", to = "textual", side_from = "left", side_to = "left", cov_curve = -0.6)
#' )
#'
#' title_list <- list(
#'   panel_title(panel_num = 1, title = "My SEM Plot")
#' )
#'
#' p <- diyPaths(fit = fit, node_positions = node_list, path_positions = path_list,
#'               standardised = TRUE,
#'               digits = 3,
#'               est_stars = TRUE,
#'               est_p = FALSE,
#'               est_ci = FALSE,
#'               show_variances = FALSE,
#'               variance_stars = FALSE,
#'               variance_p = FALSE,
#'               variance_ci = FALSE,
#'               sig_linetype = FALSE,
#'               p_threshold = 0.05,
#'               panel_titles = title_list,
#'               show_grid = TRUE,
#'               grid_axis_scale = 0.5,
#'               path_transparent_text = FALSE,
#'               latent_node_text_size = 4,
#'               observed_node_text_size = 3,
#'               path_text_size = 3.5,
#'               line_thickness = 0.6,
#'               arrow_size = 0.2,
#'               node_width = 1.5, node_height = 1,
#'               latent_node_size_adjust = 0.8,
#'               observed_node_size_adjust = 0.55,
#'               show_group_labels = FALSE,
#'               panel_cols = NULL,
#'               margin_x = 0.5,
#'               margin_y = 0.5,
#'               look_up_table = TRUE)
#'
#' print(p)

diyPaths <- function(fit, node_positions, path_positions, standardised = FALSE, digits = 3,
                     est_stars = FALSE, est_p = FALSE, est_ci = FALSE,
                     show_variances = FALSE, variance_stars = FALSE, variance_p = FALSE, variance_ci = FALSE,
                     sig_linetype = FALSE, p_threshold = 0.05,
                     node_width = 1.5, node_height = 1,
                     latent_node_size_adjust = 1,observed_node_size_adjust = 1,
                     latent_node_text_size = 4, observed_node_text_size = 4,
                     path_transparent_text = FALSE, path_text_size = 3.5,
                     line_thickness = 0.6, arrow_size = 0.2,
                     panel_cols = NULL, show_group_labels = FALSE,
                     title_text_size = 25, panel_titles = NULL,
                     panel_notes = NULL, note_text_size = 4, note_transparent_text = FALSE, note_outline = TRUE,
                     show_grid = FALSE, grid_axis_scale = 1,
                     margin_x = 0.5, margin_y = 0.5,
                     look_up_table = FALSE){

  pos_df <- do.call(rbind, lapply(node_positions, function(v) {
    data.frame(
      name = as.character(v$name),
      x = as.numeric(v$x),
      y = as.numeric(v$y),
      label = if (!is.null(v$label)) as.character(v$label) else as.character(v$name),
      stringsAsFactors = FALSE
    )
  }))

  notes_df <- do.call(rbind, lapply(panel_notes, as.data.frame))

  titles_df <- do.call(rbind, lapply(panel_titles, as.data.frame))

  pe_full <- if (standardised){
    stan <- lavaan::standardizedsolution(fit)
    names(stan)[names(stan) == "est.std"] <- "est"
    stan
  } else {
    lavaan::parameterestimates(fit)
  }

  size_tbl <- data.frame(
    type      = c("latent", "observed", "path"),
    width     = c(node_width * latent_node_size_adjust,
                  node_width * observed_node_size_adjust,
                  NA),
    height    = c(node_height * latent_node_size_adjust,
                  node_height * observed_node_size_adjust,
                  NA),
    text_size = c(latent_node_text_size,
                  observed_node_text_size,
                  path_text_size)
  )

  latent_vars_all <- unique(pe_full$lhs[pe_full$op == "=~"])
  pos_df$is_latent <- pos_df$name %in% latent_vars_all

  pos_df$w <- ifelse(pos_df$is_latent, node_width * latent_node_size_adjust, node_width * observed_node_size_adjust)
  pos_df$h <- ifelse(pos_df$is_latent, node_height * latent_node_size_adjust, node_height * observed_node_size_adjust)
  pos_df$a <- pos_df$w / 2
  pos_df$b <- pos_df$h / 2

  pos_df$xmin <- pos_df$x - pos_df$w / 2
  pos_df$xmax <- pos_df$x + pos_df$w / 2
  pos_df$ymin <- pos_df$y - pos_df$h / 2
  pos_df$ymax <- pos_df$y + pos_df$h / 2

  sub_text_size <- path_text_size * 2

  group_ids <- if ("group" %in% names(pe_full)) sort(unique(pe_full$group)) else 1
  raw_group_values <- if (length(group_ids) > 1) lavaan::lavInspect(fit, "group.label") else NULL

  plots <- vector("list", length(group_ids))

  for (g in seq_along(group_ids)) {

    pe <- if (length(group_ids) > 1) pe_full[pe_full$group == group_ids[g], ] else pe_full

    allowed_ops <- c("=~", "~~", "~")

    paths <- pe[pe$op %in% allowed_ops, c("lhs", "op", "rhs", "est", "ci.lower", "ci.upper", "pvalue")]
    colnames(paths) <- c("lhs", "type", "rhs", "est", "ci.lower", "ci.upper", "pvalue")

    is_loading <- paths$type == "=~"
    is_reg <- paths$type == "~"
    is_cov <- paths$type == "~~" & paths$lhs != paths$rhs
    is_var <- paths$type == "~~" & paths$lhs == paths$rhs

    paths$from <- NA
    paths$to <- NA

    paths$from[is_loading] <- paths$lhs[is_loading]
    paths$to[is_loading] <- paths$rhs[is_loading]

    paths$from[is_reg] <- paths$rhs[is_reg]
    paths$to[is_reg] <- paths$lhs[is_reg]

    paths$from[is_cov | is_var] <- paths$lhs[is_cov | is_var]
    paths$to[is_cov | is_var] <- paths$rhs[is_cov | is_var]

    if (!show_variances) paths <- paths[!(paths$type == "~~" & paths$from == paths$to), ]

    digs <- paste0("%.", digits, "f")

    paths$value <- sprintf(digs, paths$est)
    paths$ci.lower <- sprintf(digs, paths$ci.lower)
    paths$ci.upper <- sprintf(digs, paths$ci.upper)

    stars  <- character(nrow(paths))
    stars[!is.na(paths$pvalue) & paths$pvalue <= 0.001] <- "***"
    stars[!is.na(paths$pvalue) & paths$pvalue > 0.001 & paths$pvalue <= 0.01] <- "**"
    stars[!is.na(paths$pvalue) & paths$pvalue > 0.01 & paths$pvalue <= 0.05] <- "*"

    use_stars <- ifelse(paths$type == "~~" & paths$from == paths$to, variance_stars, est_stars)
    use_p <- ifelse(paths$type == "~~" & paths$from == paths$to, variance_p, est_p)
    use_ci <- ifelse(paths$type == "~~" & paths$from == paths$to, variance_ci, est_ci)

    paths$label_text <- as.character(paths$value)
    paths$label_text[use_stars] <- paste0(paths$label_text[use_stars], stars[use_stars])

    sub_text <- character(nrow(paths))
    ci_valid <- use_ci & !is.na(paths$ci.lower) & !is.na(paths$ci.upper)
    sub_text[ci_valid] <- paste0("[", paths$ci.lower[ci_valid], ", ", paths$ci.upper[ci_valid], "]")

    p_valid <- use_p & !is.na(paths$pvalue)
    ps <- character(nrow(paths))
    ps[p_valid] <- ifelse(paths$pvalue[p_valid] < 0.001, "p < .001", paste0("p = ", sprintf(digs, paths$pvalue[p_valid])))

    both_sub <- nchar(sub_text) > 0 & nchar(ps) > 0
    sub_text[both_sub] <- paste0(sub_text[both_sub], ", ", ps[both_sub])
    sub_text[!both_sub & nchar(ps) > 0] <- ps[!both_sub & nchar(ps) > 0]

    has_sub <- nchar(sub_text) > 0
    if (any(has_sub)) {
      paths$label_text[has_sub] <- paste0(paths$label_text[has_sub],
                                          "<br><span style='font-size:", sub_text_size, "pt;'>",
                                          sub_text[has_sub], "</span>")
    }

    paths$linetype <- if (sig_linetype) ifelse(!is.na(paths$pvalue) & paths$pvalue > p_threshold, "dashed", "solid") else "solid"

    paths$side_from <- "right"
    paths$side_to <- "left"
    paths$nudge_x <- 0
    paths$nudge_y <- 0
    paths$variance_position <- "top"
    paths$curvature <- ifelse(
      paths$type == "~~" & paths$from != paths$to, 0.4,
      ifelse(paths$type == "~~" & paths$from == paths$to,
             ifelse(paths$variance_position == "bottom", 1.5, -1.5),
             0)
    )

    for (pp in path_positions) {
      match <- ifelse(
        paths$type == "~~",
        (paths$from == pp$from & paths$to == pp$to) | (paths$from == pp$to & paths$to == pp$from),
        paths$from == pp$from & paths$to == pp$to
      )
      if (any(match)) {
        paths$side_from[match] <- pp$side_from
        paths$side_to[match] <- pp$side_to
        if (!is.null(pp$cov_curve)) paths$curvature[match] <- pp$cov_curve
        if (!is.null(pp$nudge_text_x)) paths$nudge_x[match] <- pp$nudge_text_x
        if (!is.null(pp$nudge_text_y)) paths$nudge_y[match] <- pp$nudge_text_y
        if (!is.null(pp$variance_position)) paths$variance_position[match] <- pp$variance_position
      }
    }

    is_var_type <- paths$type == "~~" & paths$from == paths$to
    paths$curvature[is_var_type] <- ifelse(
      paths$variance_position[is_var_type] %in% c("bottom", "right"), 1.5, -1.5
    )
    pos_map <- split(pos_df, pos_df$name)
    get_pt <- function(node, side) {
      switch(side,
             "right" = c(node$xmax, node$y),
             "left" = c(node$xmin, node$y),
             "top" = c(node$x, node$ymax),
             "bottom" = c(node$x, node$ymin),
             c(node$x, node$y))
    }

    n <- nrow(paths)
    start_x <- numeric(n); start_y <- numeric(n)
    end_x <- numeric(n); end_y   <- numeric(n)

    for (i in 1:n) {
      start_pt <- get_pt(pos_map[[paths$from[i]]], paths$side_from[i])
      end_pt <- get_pt(pos_map[[paths$to[i]]], paths$side_to[i])

      start_x[i] <- start_pt[1]
      start_y[i] <- start_pt[2]
      end_x[i] <- end_pt[1]
      end_y[i] <- end_pt[2]
    }

    paths$x <- start_x; paths$y <- start_y
    paths$xend <- end_x; paths$yend <- end_y

    paths$mid_x <- (paths$x + paths$xend) / 2 + paths$nudge_x
    paths$mid_y <- (paths$y + paths$yend) / 2 + paths$nudge_y

    is_cov_path <- paths$type == "~~" & paths$from != paths$to
    if (any(is_cov_path)) {
      for (i in which(is_cov_path)) {
        x1 <- paths$x[i]; y1 <- paths$y[i]
        x2 <- paths$xend[i]; y2 <- paths$yend[i]
        curv <- paths$curvature[i]

        dx <- x2 - x1
        dy <- y2 - y1
        d  <- sqrt(dx^2 + dy^2)

        perp_x <- dy / d
        perp_y <- -dx / d
        bump   <- curv * d / 2

        m_x <- (x1 + x2) / 2
        m_y <- (y1 + y2) / 2

        paths$mid_x[i] <- m_x + perp_x * bump + paths$nudge_x[i]
        paths$mid_y[i] <- m_y + perp_y * bump + paths$nudge_y[i]
      }
    }

    is_var_path <- paths$type == "~~" & paths$from == paths$to
    if (any(is_var_path)) {
      for (i in which(is_var_path)) {
        variance_map <- pos_map[[paths$from[i]]]
        variance_position <- paths$variance_position[i]
        if (variance_position == "bottom") {
          paths$x[i] <- variance_map$x - 0.2 + paths$nudge_x[i]
          paths$y[i] <- variance_map$ymin
          paths$xend[i] <- variance_map$x + 0.2 + paths$nudge_x[i]
          paths$yend[i] <- variance_map$ymin
          paths$mid_x[i] <- variance_map$x + paths$nudge_x[i]
          paths$mid_y[i] <- variance_map$ymin - 0.4 + paths$nudge_y[i]
        } else if (variance_position == "top") {
          paths$x[i] <- variance_map$x - 0.2 + paths$nudge_x[i]
          paths$y[i] <- variance_map$ymax
          paths$xend[i] <- variance_map$x + 0.2 + paths$nudge_x[i]
          paths$yend[i] <- variance_map$ymax
          paths$mid_x[i] <- variance_map$x + paths$nudge_x[i]
          paths$mid_y[i] <- variance_map$ymax + 0.4 + paths$nudge_y[i]
        } else if (variance_position == "left") {
          paths$x[i] <- variance_map$xmin
          paths$y[i] <- variance_map$y - 0.2 + paths$nudge_y[i]
          paths$xend[i] <- variance_map$xmin
          paths$yend[i] <- variance_map$y + 0.2 + paths$nudge_y[i]
          paths$mid_x[i] <- variance_map$xmin - 0.4 + paths$nudge_x[i]
          paths$mid_y[i] <- variance_map$y + paths$nudge_y[i]
        } else if (variance_position == "right") {
          paths$x[i] <- variance_map$xmax
          paths$y[i] <- variance_map$y - 0.2 + paths$nudge_y[i]
          paths$xend[i] <- variance_map$xmax
          paths$yend[i] <- variance_map$y + 0.2 + paths$nudge_y[i]
          paths$mid_x[i] <- variance_map$xmax + 0.4 + paths$nudge_x[i]
          paths$mid_y[i] <- variance_map$y + paths$nudge_y[i]
        }
      }
    }

    measured_df <- pos_df[!pos_df$is_latent, ]
    latent_df <- pos_df[pos_df$is_latent, ]

    directional_paths <- paths[paths$type %in% c("=~", "~"), ]
    cov_paths <- paths[paths$type == "~~" & paths$from != paths$to, ]
    variance_paths <- paths[paths$type == "~~" & paths$from == paths$to, ]

    p <- ggplot2::ggplot()

    if (nrow(measured_df) > 0) {
      p <- p + ggplot2::geom_rect(data = measured_df,
                                  ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                  fill = "white", color = "black") +
        ggplot2::geom_text(data = measured_df,
                           ggplot2::aes(x = x, y = y, label = label),
                           size = observed_node_text_size)
    }

    if (nrow(latent_df) > 0) {
      p <- p + ggforce::geom_ellipse(data = latent_df,
                                     ggplot2::aes(x0 = x, y0 = y, a = a, b = b, angle = 0),
                                     fill = "white", color = "black", linewidth = 0.5) +
        ggplot2::geom_text(data = latent_df,
                           ggplot2::aes(x = x, y = y, label = label),
                           size = latent_node_text_size)
    }

    if (nrow(directional_paths) > 0) {
      p <- p +
        ggplot2::geom_segment(data = directional_paths,
                              ggplot2::aes(x = x, y = y, xend = xend, yend = yend, linetype = linetype),
                              linewidth = line_thickness,
                              arrow = ggplot2::arrow(length = grid::unit(arrow_size, "cm"), type = "closed")) +
        ggtext::geom_richtext(data = directional_paths, ggplot2::aes(x = mid_x, y = mid_y, label = label_text),
                              fill = if (path_transparent_text) NA else "white",
                              label.padding = grid::unit(rep(2, 4), "pt"), label.color = NA, size = path_text_size)
    }

    if (nrow(cov_paths) > 0) {
      for (i in seq_len(nrow(cov_paths))) {
        p <- p + ggplot2::geom_curve(data = cov_paths[i, ],
                                     ggplot2::aes(x = x, y = y, xend = xend, yend = yend, linetype = linetype),
                                     linewidth = line_thickness,
                                     curvature = cov_paths$curvature[i],
                                     arrow = ggplot2::arrow(length = grid::unit(arrow_size, "cm"), ends = "both"))
      }
      p <- p + ggtext::geom_richtext(data = cov_paths, ggplot2::aes(x = mid_x, y = mid_y, label = label_text),
                                     fill = if (path_transparent_text) NA else "white",
                                     label.padding = grid::unit(rep(2, 4), "pt"), label.color = NA, size = path_text_size)
    }

    if (nrow(variance_paths) > 0) {
      for (i in seq_len(nrow(variance_paths))) {
        p <- p + ggplot2::geom_curve(data = variance_paths[i, ],
                                     ggplot2::aes(x = x, y = y, xend = xend, yend = yend, linetype = linetype),
                                     linewidth = line_thickness,
                                     curvature = variance_paths$curvature[i],
                                     arrow = ggplot2::arrow(length = grid::unit(arrow_size, "cm"), type = "closed"))
      }
      p <- p + ggtext::geom_richtext(data = variance_paths, ggplot2::aes(x = mid_x, y = mid_y, label = label_text),
                                     fill = if (path_transparent_text) NA else "white",
                                     label.padding = grid::unit(rep(2, 4), "pt"), label.color = NA, size = path_text_size)
    }


    x_limits <- c(min(pos_df$xmin) - margin_x, max(pos_df$xmax) + margin_x)
    y_limits <- c(min(pos_df$ymin) - margin_y, max(pos_df$ymax) + margin_y)

    p <- p + ggplot2::scale_linetype_identity() +
      ggplot2::scale_x_continuous(breaks = seq(floor(x_limits[1]), ceiling(x_limits[2]), by = grid_axis_scale)) +
      ggplot2::scale_y_continuous(breaks = seq(floor(y_limits[1]), ceiling(y_limits[2]), by = grid_axis_scale)) +
      ggplot2::coord_fixed(xlim = x_limits, ylim = y_limits)

    if (!is.null(titles_df)) {
      match_id_title <- titles_df[titles_df$panel_num == g & !is.na(titles_df$title), ]
      if (nrow(match_id_title) > 0)
        p <- p + ggplot2::labs(title = match_id_title$title[1])
    }


    if (!is.null(notes_df)) {
      match_id_note <- notes_df[notes_df$panel_num == g & !is.na(notes_df$note_text), ]
      if (nrow(match_id_note) > 0)
        p <- p + ggtext::geom_richtext(
          data = match_id_note,
          ggplot2::aes(x = x, y = y, label = note_text),
          size = note_text_size,
          fill = if (note_transparent_text) NA else "white",
          label.color = if (note_outline) "black" else NA
        )
    }


    if (show_group_labels && length(group_ids) > 1) {
      p <- p + ggplot2::annotate(
        "text", x = Inf, y = -Inf,
        label = paste0("Panel ", g, ": Group = ", raw_group_values[g]),
        hjust = 1.05, vjust = -0.5, size = 3, colour = "grey40"
      )
    }

    grid_or_no <- if (show_grid) {
      ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(color = "grey", linetype = "dashed"),
          panel.grid.minor = ggplot2::element_line(color = "grey", linetype = "dotted"),
          axis.text = ggplot2::element_text(size = 9, color = "black"),
          axis.title = ggplot2::element_text(size = 10),
          axis.ticks = ggplot2::element_line(color = "black"),
          panel.background = ggplot2::element_rect(fill = "white"),
          plot.title = ggplot2::element_text(size = title_text_size)
        )
    } else {
      ggplot2::theme_void()+
        ggplot2::theme(plot.title = ggplot2::element_text(size = title_text_size))
    }

    p <- p + grid_or_no

    plots[[g]] <- p
  }

  final <- if (length(plots) == 1) {
    plots[[1]]
  } else {
    cols <- if (!is.null(panel_cols)) panel_cols else min(length(plots), 2)
    patchwork::wrap_plots(plots, ncol = cols)
  }

  if (look_up_table) {
    return(list(plot = final, look_up_table = size_tbl))
  }

  final
}

