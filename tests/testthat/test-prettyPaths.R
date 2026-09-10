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
