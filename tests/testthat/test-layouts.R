context("layouts")

test_that("Basic layouts work correctly", {
  layout <- simple_log_layout()
  expect_match(layout("INFO", "Message"), "Message")

  layout <- default_log_layout()
  expect_match(layout("INFO", "Message"), "Message")

  layout <- bare_log_layout()
  expect_match(layout("INFO", "Message"), "Message")

  layout <- csv_log_layout()
  expect_match(layout("INFO", "Message"), ",Message\\n", )
  expect_match(layout("INFO", "Message", "Second"), ",Message,Second\\n")
  layout <- csv_log_layout(delimiter = "\t")
  expect_match(layout("INFO", "Message"), "\\tMessage\\n")
})

test_that("JSON layouts work correctly", {
  skip_if_not_installed("jsonlite")

  layout <- json_log_layout()
  expect_match(layout("INFO", "Message"), "\"message\":\"Message\"")
  expect_match(layout("INFO", field = "value"), "\"field\":\"value\"")
})

test_that("Wonky times formats are caught early", {
  expect_error(default_log_layout(strrep("%Y", 30)), regex = "Invalid")
})
