test_that("node() creates correct position and label defaults", {
  n <- node("x", x = 2, label = "Visual")
  expect_equal(n$name, "x")
  expect_equal(n$x, 2)
  expect_equal(n$y, 0)
  expect_equal(n$label, "Visual")
})

test_that("path() creates correct connection and defaults", {
  path <- path(from = "x", to = "x1", side_from = "bottom")
  expect_equal(path$from, "x")
  expect_equal(path$to, "x1")
  expect_equal(path$side_from, "bottom")
  expect_equal(path$side_to, "left")
  expect_null(path$cov_curve)
})

test_that("panel_title() creates correct panel and defaults", {
  pt <- panel_title()
  expect_equal(pt$panel, 1)
  expect_equal(pt$title, character(0))

  pt_custom <- panel_title(panel = 2, title = "Female Participants")
  expect_equal(pt_custom$panel, 2)
  expect_equal(pt_custom$title, "Female Participants")
})

test_that("diyPaths generates a ggplot object from a simple fit", {
  data(HolzingerSwineford1939, package = "lavaan")
  fit_simple <- lavaan::sem('visual =~ x1 + x2 + x3', data = HolzingerSwineford1939)

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

  p <- diyPaths(fit = fit_simple, node_positions = node_pos, path_positions = path_pos)
  expect_s3_class(p, "ggplot")
})

test_that("diyPaths defaults, panel numbers, and group labels work correctly", {
  data(HolzingerSwineford1939, package = "lavaan")
  sem_model <- '
    visual =~ x1 + x2 + x3
  '
  fit <- sem(sem_model, data = HolzingerSwineford1939, group = "school")

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

  fit_single <- sem(sem_model, data = HolzingerSwineford1939)
  p_single <- diyPaths(
    fit = fit_single,
    node_positions = node_positions,
    path_positions = path_positions
  )
  expect_s3_class(p_single, "ggplot")

   p_multi_default <- diyPaths(
    fit = fit,
    node_positions = node_positions,
    path_positions = path_positions,
    show_group_labels = TRUE
  )
  expect_s3_class(p_multi_default, "patchwork")

  partial_titles <- list(
    panel_title(panel = 1, title = "Custom Group 1")
  )
  p_multi_partial <- diyPaths(
    fit = fit,
    node_positions = node_positions,
    path_positions = path_positions,
    panel_titles = partial_titles
  )
  expect_s3_class(p_multi_partial, "patchwork")
})
