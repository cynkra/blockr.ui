test_that("dash_board_plugins works", {
  plugs <- dash_board_plugins("manage_blocks")
  expect_s3_class(plugs, "plugins")
  expect_length(plugs, 1)
  expect_s3_class(plugs[[1]], c("manage_blocks", "plugin"))
})
