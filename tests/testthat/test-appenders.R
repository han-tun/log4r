context("appenders")

test_that("The console appender works correctly", {
  appender <- console_appender(simple_log_layout())
  expect_output(appender("Message"), "Message")
})

test_that("The file appender works correctly", {
  outfile <- tempfile("log")
  appender <- file_appender(outfile, layout = simple_log_layout())

  expect_silent(appender("Message"))
  expect_file_contains(outfile, regex = "Message")
})

test_that("The HTTP appender works correctly", {
  skip_if_not_installed("httr")

  appender <- http_appender("http://example.com", layout = simple_log_layout())
  expect_silent(appender("INFO", "Message"))

  appender <- expect_silent(http_appender(
    "http://example.com", "POST", layout = json_log_layout(),
    httr::content_type_json()
  ))

  # Don't send actual HTTP requests on CRAN.
  skip_on_cran()
  expect_silent(appender("INFO", "Message"))
})

test_that("The HTTP appender accepts only valid verbs", {
  skip_if_not_installed("httr")

  expect_error(
    http_appender("http://example.com", method = "INVALID"),
    regex = "not a supported HTTP method"
  )
})

test_that("Layout arguments are checked", {
  expect_error(console_appender("notalayout"))
  expect_error(file_appender(tempfile("log"), layout = "notalayout"))
  expect_error(tcp_appender(layout = "notalayout"))

  skip_if_not_installed("httr")
  expect_error(http_appender(layout = "notalayout"))
})
