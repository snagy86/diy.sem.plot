

test_that("node() creates correct position and label defaults", {
  n <- node("x", x = 2, label = "Visual")
  expect_equal(n$name, "x")
  expect_equal(n$x, 2)
  expect_equal(n$y, 0)
  expect_equal(n$label, "Visual")
})

test_that("path() creates correct connection and defaults", {
  p <- path(from = "x", to = "x1", side_from = "bottom")
  expect_equal(p$from, "x")
  expect_equal(p$to, "x1")
  expect_equal(p$side_from, "bottom")
  expect_equal(p$side_to, "left")
  expect_null(p$cov_curve)
})

test_that("panel_title() creates correct panel and defaults", {
  pt <- panel_title()
  expect_equal(pt$panel_num, 1)
  expect_true(is.na(pt$title))

  pt_custom <- panel_title(panel_num = 2, title = "Female Participants")
  expect_equal(pt_custom$panel_num, 2)
  expect_equal(pt_custom$title, "Female Participants")
})

test_that("diyPaths generates a ggplot object from a simple fit", {

  data(HolzingerSwineford1939, package = "lavaan")
  fit <- lavaan::sem('visual =~ x1 + x2 + x3', data = HolzingerSwineford1939)

  node_pos <- list(
    node("visual", x = 1, y = 1),
    node("x1", x = 0, y = 0),
    node("x2", x = 1, y = 0),
    node("x3", x = 2, y = 0)
  )

  path_pos <- list(
    path("visual", "x1"),
    path("visual", "x2"),
    path("visual", "x3")
  )

  p <- diyPaths(fit = fit, node_positions = node_pos, path_positions = path_pos)
  expect_s3_class(p, "ggplot")
})

test_that("diyPaths defaults, panel numbers, and group labels work correctly", {
  data(HolzingerSwineford1939, package = "lavaan")
  sem_model <- '
    visual =~ x1 + x2 + x3
  '
  fit_group <- lavaan::sem(sem_model, data = HolzingerSwineford1939, group = "school")

  node_positions <- list(
    node("visual", x = 1, y = 1),
    node("x1", x = 0, y = 0),
    node("x2", x = 1, y = 0),
    node("x3", x = 2, y = 0)
  )

  path_positions <- list(
    path(from = "visual", to = "x1", side_from = "bottom", side_to = "top"),
    path(from = "visual", to = "x2", side_from = "bottom", side_to = "top"),
    path(from = "visual", to = "x3", side_from = "bottom", side_to = "top")
  )


  fit_single <- lavaan::sem(sem_model, data = HolzingerSwineford1939)
  p_single <- diyPaths(
    fit = fit_single,
    node_positions = node_positions,
    path_positions = path_positions
  )
  expect_s3_class(p_single, "ggplot")

  p_multi_default <- diyPaths(
    fit = fit_group,
    node_positions = node_positions,
    path_positions = path_positions,
    show_group_labels = TRUE
  )
  expect_s3_class(p_multi_default, "patchwork")

 partial_titles <- list(
    panel_title(panel_num = 1, title = "Custom Group 1")
  )
  p_multi_partial <- diyPaths(
    fit = fit_group,
    node_positions = node_positions,
    path_positions = path_positions,
    panel_titles = partial_titles
  )
  expect_s3_class(p_multi_partial, "patchwork")
  expect_equal(p_multi_partial[[1]]$labels$title, "Custom Group 1")
})

test_that("template works", {
  n <- node("x", x = 2, label = "Visual")
  expect_equal(n$name, "x")
  expect_equal(n$x, 2)
  expect_equal(n$y, 0)
  expect_equal(n$label, "Visual")
})

test_that("template() prints with variance paths on", {
  data(HolzingerSwineford1939, package = "lavaan")
  fit <- lavaan::sem('visual =~ x1 + x2 + x3', data = HolzingerSwineford1939)
  expect_output(template(fit, include_variance_paths = TRUE, diyPaths_arguments = "min"))
})

test_that("template() prints with variance paths off", {
  data(HolzingerSwineford1939, package = "lavaan")
  fit <- lavaan::sem('visual =~ x1 + x2 + x3', data = HolzingerSwineford1939)
  expect_output(template(fit, include_variance_paths = FALSE, diyPaths_arguments = "min"))
})

test_that("template() prints with max arguments", {
  data(HolzingerSwineford1939, package = "lavaan")
  fit <- lavaan::sem('visual =~ x1 + x2 + x3', data = HolzingerSwineford1939)
  expect_output(template(fit, diyPaths_arguments = "max"))
})

test_that("template() prints with min arguments", {
  data(HolzingerSwineford1939, package = "lavaan")
  fit <- lavaan::sem('visual =~ x1 + x2 + x3', data = HolzingerSwineford1939)
  expect_output(template(fit, diyPaths_arguments = "min"))
})
