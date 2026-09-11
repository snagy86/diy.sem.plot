utils::globalVariables(c(
  "xmin", "xmax", "ymin", "ymax", "x", "y", "label",
  "a", "b", "xend", "yend", "linetype", "mid_x", "mid_y", "label_text", ".data"
))

#' Create a node position list
#'
#' @description
#' Helper function that creates a list of arguments which specify a given node's position and its name in the diagram,
#' designed for use within the `node_positions` argument of [diyPaths()].
#'
#' @param name The name of the variable within the lavaan model.
#' @param x Numeric value for a nodes x-coordinate relative to center of the node. Default is `0`.
#' @param y Numeric value for a nodes y-coordinate relative to center of the node. Default is `0`.
#' @param label Custom label to display instead of lavaan variable name. Defaults to the lavaan variable name.
#'
#' @examples
#'
#' #a list that specifies a node positioned on x = 1,  y = 2,
#' #and relabelled from  its lavaan model name
#'
#' node(name = "bpm", x = 1, y = 2,  label = "Beats per Minute")
#'
#' @return A list containing arguments for an individual nodes position and label within a diyPaths plot.
#' @export

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
#' designed for use within the `paths_positions` argument of [diyPaths()].
#'
#' @param from The source variable name with in lavaan model.
#' @param to The target variable name within lavaan model.
#' @param side_from Side of source node where path originates ("top", "bottom", "left", "right"). Default is "right".
#' @param side_to Side of target node where path terminates ("top", "bottom", "left", "right"). Default is "left".
#' @param cov_curve Numeric value for curvature of covariance/correlation paths. Default is NULL.
#' @param nudge_text_x Numeric fine tuning adjustment for path estimate text along the x-axis. Default is `0`.
#' @param nudge_text_y Numeric fine tuning adjustment for path estimate text along the y-axis. Default is `0`.
#' @param variance_position Placement of variance/residual paths ("top" or "bottom"). Default is "top".
#'
#' @details
#' `from`/`to` in [path()] must exactly match the variable name used in the
#'  \pkg{lavaan} model syntax. Furthermore, the order must also be correct for regression or loading paths. Misspelled or mismatched paths will be
#'  excluded  from the diagram without raising an error.
#'
#' `cov_curve`'s value can be used to adjust direction of curve on covariance/correlation path.
#'  For a mostly vertical path (i.e. node1: x = 0, y = 1 -> node2: x = 0, y = 1), positive curvature bends it left and negative curvature
#'  bends it right. For a mostly horizontal path (i.e. node1: x = 1, y = 0 -> node2: x = 2, y = 0), positive curvature bends
#'  it down and negative curvature bends it up. Best results typically range from -1 to 1.
#'
#'
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
#' @return A list containing arguments that specify position for a given path within a diyPaths plot.
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
#' Create a title for a diyPaths panel
#'
#' @description
#' Helper function to assign a custom title to a specific panel produced by
#' [diyPaths()]. .
#' Use the `show_group_labels` argument in [diyPaths()] to view each panel's plot
#' number and which group it refers to.
#'
#' @param panel Integer value for the a panel's panel number  this title applies to. Default is `1`.
#' @param title The title text to display.
#'
#' @examples
#' # titling a single, non-grouped model
#' panel_title(title = "My SEM Model")
#'
#' # titling the second panel of a multi-group model
#' panel_title(panel = 2, title = "Female Participants")
#'
#' @return A list containing the panel index and its title.
#' @export
panel_title <- function(panel = 1, title = NULL) {
  list(panel = as.integer(panel), title = as.character(title))
}

#' Manually plot path diagrams for structural equation models
#'
#' @description
#'
#' Plot fully customisable path diagrams for structural equation models fitted with \pkg{lavaan}, rendered using \pkg{ggplot2}.
#'
#' Diagrams are built by specifying each node's coordinates and the attachment points (sides of relevant node) where each
#' paths begins and ends. A range of further fine-tuning options are included, facilitating creation of a path diagram
#' exactly as you envision it, entirely within R.
#'
#' @param fit A fitted model object of class `lavaan`.
#' @param node_positions Specify a list of node position objects, I highly suggest using the [node()] helper function.
#' @param path_positions Specify a list of path configuration objects, I highly suggest using the [path()] helper function.
#' @param standardised Logical. If `TRUE`, uses standardised parameter estimates (`est.std`). Default is `FALSE`.
#' @param digits Number of digits to display for estimates. Default is `3`.
#' @param est_stars Logical. Whether to display significance stars on path estimates. Default is `FALSE`.
#' @param est_p Logical. Whether to display p-values on path estimates. Default is `FALSE`.
#' @param est_ci Logical. Whether to display confidence intervals on path estimates. Default is `FALSE`.
#' @param variance_stars Logical. Whether to display significance stars on variance paths. Default is `FALSE`.
#' @param variance_p Logical. Whether to display p-values on variance paths. Default is `FALSE`.
#' @param variance_ci Logical. Whether to display confidence intervals on variance paths. Default is `FALSE`.
#' @param sig_linetype Logical. If `TRUE`, renders non-significant paths with dashed lines. Default is `FALSE`.
#' @param p_threshold Statistical significance threshold used to determine non-significant paths when `sig_linetype = TRUE`. Default is `0.05`.
#' @param show_variances Logical. Whether to display variance and residual paths. Default is `FALSE`.
#' @param latent_node_text_size Text font size for latent variable labels. Default is `4`.
#' @param observed_node_text_size Text font size for observed variable labels. Default is `4`.
#' @param path_text_size Text font size for path estimate labels. Default is `3.5`.
#' @param line_thickness Sets thickness of paths. Default is `0.6`.
#' @param arrow_size Sets the size of arrow heads. Default is `0.2`.
#' @param node_width Base width for nodes. Default is `1.5`.
#' @param node_height Base height for node shapes. Default is `0.8`.
#' @param latent_variable_size_adjust Numeric multiplier scaling latent variable ellipses. Default is `1`.
#' @param observed_variable_size_adjust Numeric multiplier scaling observed variable rectangles. Default is `1`.
#' @param show_group_labels Logical. For multi-group models, whether to annotate each panel
#'   with its panel number and the raw group value it represents (e.g. "Panel 1: Group = male").
#'   Needed to identify which diagram represents each group and its internal panel number when creating panel titles.
#'   Default is `FALSE`.
#' @param panel_titles Specify a list of titles for panels, I highly suggest [panel_title()] helper function. Any panel not referenced is
#'   left untitled. Default is `NULL`.
#' @param panel_cols Integer for number columns to use when arranging multi-group panels. Default is `NULL`.
#' @param non_transparent_text Logical. If `TRUE`, path estimate labels receive a white background mask. Default is `TRUE`.
#' @param show_grid Logical. Whether to overlay a coordinate grid. Default is `FALSE`.
#' @param grid_axis_scale Sets the spacing of gridlines when `show_grid = TRUE`. Default is `1`.
#' @param margin_x Padding for plot limits along the x-axis. Default is `0`.
#' @param margin_y_bottom Padding for plot limits at the bottom. Default is `0`.
#' @param margin_y_top Padding for plot limits at the top. Default is `0`.
#'
#'@details
#'
#' Using the function requires 4 steps and is best illustrated by the example below.
#' Further examples can be found the vignette, along side how to set up common path diagram
#' structures using the function.
#'
#' 1. Specify and fit the SEM using \pkg{lavaan}.
#'
#' 2. Specify `node_positions` as a list (`node_positions <- list(...)`) and,
#'    within it, define each node's position and label using the [node()]
#'    helper function.
#'
#' 3. Specify `path_positions` as a list (`path_positions <- list(...)`) and,
#'    within it, define each path's connection points, curvature, and label
#'    adjustments using the [path()] helper function.
#'
#' 4. Call `diyPaths()`, passing in the fitted model, `node_positions`,
#'    and `path_positions`, along with any additional display augmentations (i.e. show significance stars on estimates).
#'
#' Creating a diagram for your specific model is inherently an iterative process. As such, steps 2-4 will likely need to be repeated
#' with additional fine-tuning adjustments to achieve desired result.
#'
#' @return A `ggplot` object representing the SEM diagram.
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
#' #specify the node position, this was done iteratively with show_grid to help with layout.
#'
#' node_positions <- list(
#'   #main latent variable structure
#'   node("visual", x = 1, y = 1, label = "Visual"),
#'   node("textual", x = 1, y = 2, label = "Textual"),
#'   node("speed", x = 4, y = 1.5, label = "Speed"),
#'
#'   # observed variables that visual perception ability loads onto
#'   node("x1", x = -0.06, y = -0.5, label = "Visual\nPerception"), #\n creates a line break
#'   node("x2", x = 1, y = -0.5, label = "Cubes"),
#'   node("x3", x = 2.06, y = -0.5, label = "Lozenges"),
#'
#'   #observed variables that textual ability loads onto
#'   node("x4", x = -0.06, y = 3.5, label = "Paragraph\nComprehension"),
#'   node("x5", x = 1, y = 3.5, label = "Sentence\nCompletion"),
#'   node("x6", x = 2.06, y = 3.5, label = "Word\nMeaning"),
#'
#'   #observed variables that speeded cognitive processing loads onto
#'   node("x7", x = 6, y = 0.5, label = "Speeded\nAddition"),
#'   node("x8", x = 6, y = 1.5, label = "Speeded\nCounting"),
#'   node("x9", x = 6, y = 2.5, label = "Speeded\nDiscrimination")
#' )
#'
#' #Specify the paths
#'
#' path_positions <- list(
#'   path(from = "visual", to = "x1", side_from = "bottom", side_to = "top", nudge_text_x = -0.1),
#'   path(from = "visual", to = "x2", side_from = "bottom", side_to = "top"),
#'   path(from = "visual", to = "x3", side_from = "bottom", side_to = "top", nudge_text_x = 0.1),
#'
#'   path(from = "textual", to = "x4", side_from = "top", side_to = "bottom", nudge_text_x = -0.1),
#'   path(from = "textual", to = "x5", side_from = "top", side_to = "bottom"),
#'   path(from = "textual", to = "x6", side_from = "top", side_to = "bottom", nudge_text_x = 0.1),
#'
#'   path(from = "speed",   to = "x7", side_from = "right", side_to = "left"),
#'   path(from = "speed",   to = "x8", side_from = "right", side_to = "left"),
#'   path(from = "speed",   to = "x9", side_from = "right", side_to = "left"),
#'
#'   path(from = "visual",  to = "speed", side_from = "right", side_to = "left"),
#'   path(from = "textual", to = "speed", side_from = "right", side_to = "left"),
#'
#'   path(from = "visual",  to = "textual", side_from = "left", side_to = "left", cov_curve = -0.6)
#' )
#'
#' #creating the diagram
#'
#' p <- diyPaths(
#'   fit = fit,
#'   node_positions = node_positions,
#'   path_positions = path_positions,
#'   standardised = TRUE,
#'   est_stars = TRUE,
#'   observed_variable_size_adjust = 0.55, #making observed variables smaller than latent
#'   observed_node_text_size = 3,
#'   show_grid = TRUE,
#'   grid_axis_scale = 0.4
#' )
#'
#' print(p)

diyPaths <- function(fit, node_positions, path_positions, standardised = FALSE, digits = 3,
                     est_stars = FALSE, est_p = FALSE, est_ci = FALSE,
                     variance_stars = FALSE, variance_p = FALSE, variance_ci = FALSE,
                     sig_linetype = FALSE, p_threshold = 0.05,
                     show_variances = FALSE,
                     show_grid = FALSE,
                     grid_axis_scale = 1,
                     non_transparent_text = TRUE,
                     latent_node_text_size = 4,
                     observed_node_text_size = 4,
                     path_text_size = 3.5,
                     line_thickness = 0.6,
                     arrow_size = 0.2,
                     node_width = 1.5, node_height = 0.8,
                     latent_variable_size_adjust = 1,
                     observed_variable_size_adjust = 1,
                     show_group_labels = FALSE,
                     panel_titles = NULL,
                     panel_cols = NULL,
                     margin_x = 0,
                     margin_y_bottom = 0,
                     margin_y_top = 0){

  pos_df <- do.call(rbind, lapply(node_positions, function(v) {
    data.frame(
      name = as.character(v$name),
      x = as.numeric(v$x),
      y = as.numeric(v$y),
      label = if (!is.null(v$label)) as.character(v$label) else as.character(v$name),
      stringsAsFactors = FALSE
    )
  }))

  pe_full <- if (standardised){
    stan <- lavaan::standardizedsolution(fit)
    names(stan)[names(stan) == "est.std"] <- "est"
    stan
  } else {
    lavaan::parameterestimates(fit)
  }

  group_ids <- if ("group" %in% names(pe_full)) sort(unique(pe_full$group)) else 1
  raw_group_values <- if (length(group_ids) > 1) lavaan::lavInspect(fit, "group.label") else NULL

  plots <- vector("list", length(group_ids))

  for (g in seq_along(group_ids)) {

    pe <- if (length(group_ids) > 1) pe_full[pe_full$group == group_ids[g], ] else pe_full

    latent_vars <- unique(pe$lhs[pe$op == "=~"])
    pos_df$is_latent <- pos_df$name %in% latent_vars

    pos_df$w <- ifelse(pos_df$is_latent, node_width * latent_variable_size_adjust, node_width * observed_variable_size_adjust)
    pos_df$h <- ifelse(pos_df$is_latent, node_height * latent_variable_size_adjust, node_height * observed_variable_size_adjust)
    pos_df$a <- pos_df$w / 2
    pos_df$b <- pos_df$h / 2

    pos_df$xmin <- pos_df$x - pos_df$w / 2
    pos_df$xmax <- pos_df$x + pos_df$w / 2
    pos_df$ymin <- pos_df$y - pos_df$h / 2
    pos_df$ymax <- pos_df$y + pos_df$h / 2

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
    sub_text[both_sub] <- paste0(sub_text[both_sub], " ", ps[both_sub])
    sub_text[!both_sub & nchar(ps) > 0] <- ps[!both_sub & nchar(ps) > 0]

    has_sub <- nchar(sub_text) > 0
    if (any(has_sub)) {
      paths$label_text[has_sub] <- paste0(paths$label_text[has_sub],
                                          "<br><span style='font-size:7pt;'>",
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

    for (p in path_positions) {
      match <- ifelse(
        paths$type == "~~",
        (paths$from == p$from & paths$to == p$to) | (paths$from == p$to & paths$to == p$from),
        paths$from == p$from & paths$to == p$to
      )
      if (any(match)) {
        paths$side_from[match] <- p$side_from
        paths$side_to[match] <- p$side_to
        if (!is.null(p$cov_curve)) paths$curvature[match] <- p$cov_curve
        if (!is.null(p$nudge_text_x)) paths$nudge_x[match] <- p$nudge_text_x
        if (!is.null(p$nudge_text_y)) paths$nudge_y[match] <- p$nudge_text_y
        if (!is.null(p$variance_position)) paths$variance_position[match] <- p$variance_position
      }
    }

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
        } else {
          paths$x[i] <- variance_map$x - 0.2 + paths$nudge_x[i]
          paths$y[i] <- variance_map$ymax
          paths$xend[i] <- variance_map$x + 0.2 + paths$nudge_x[i]
          paths$yend[i] <- variance_map$ymax
          paths$mid_x[i] <- variance_map$x + paths$nudge_x[i]
          paths$mid_y[i] <- variance_map$ymax + 0.4 + paths$nudge_y[i]
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
                              fill = if (non_transparent_text) "white" else NA, label.color = NA,
                              label.padding = grid::unit(rep(2, 4), "pt"), size = path_text_size)
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
                                     fill = if (non_transparent_text) "white" else NA, label.color = NA,
                                     label.padding = grid::unit(rep(2, 4), "pt"), size = path_text_size)
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
                                     fill = if (non_transparent_text) "white" else NA, label.color = NA,
                                     label.padding = grid::unit(rep(2, 4), "pt"), size = path_text_size)
    }

    grid_or_no <- if (show_grid) {
      ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(color = "grey", linetype = "dashed"),
          panel.grid.minor = ggplot2::element_line(color = "grey", linetype = "dotted"),
          axis.text = ggplot2::element_text(size = 9, color = "black"),
          axis.title = ggplot2::element_text(size = 10),
          axis.ticks = ggplot2::element_line(color = "black"),
          panel.background = ggplot2::element_rect(fill = "white")
        )
    } else {
      ggplot2::theme_void()
    }

    x_limits <- c(min(pos_df$xmin) - margin_x, max(pos_df$xmax) + margin_x)
    y_limits <- c(min(pos_df$ymin) - margin_y_bottom, max(pos_df$ymax) + margin_y_top)

    p <- p + ggplot2::scale_linetype_identity() +
      ggplot2::scale_x_continuous(breaks = seq(floor(x_limits[1]), ceiling(x_limits[2]), by = grid_axis_scale)) +
      ggplot2::scale_y_continuous(breaks = seq(floor(y_limits[1]), ceiling(y_limits[2]), by = grid_axis_scale)) +
      ggplot2::coord_fixed(xlim = x_limits, ylim = y_limits) +
      grid_or_no

    if (!is.null(panel_titles)) {
      match_idx <- which(vapply(panel_titles, function(pt) pt$panel == g, logical(1)))
      if (length(match_idx) > 0) p <- p + ggplot2::labs(title = panel_titles[[match_idx[1]]]$title)
    }

    if (show_group_labels && length(group_ids) > 1) {
      p <- p + ggplot2::annotate(
        "text", x = Inf, y = -Inf,
        label = paste0("Panel ", g, ": Group = ", raw_group_values[g]),
        hjust = 1.05, vjust = -0.5, size = 3, colour = "grey40"
      )
    }

    plots[[g]] <- p
  }

  if (length(plots) == 1) return(plots[[1]])

  cols <- if (!is.null(panel_cols)) {
    panel_cols
  } else {
     min(length(plots), 2)
  }

  patchwork::wrap_plots(plots, ncol = cols)
}
