testthat::test_that("get_ona_page Successfully API call processes correctly", {
  testthat::skip_on_cran()

  result <- tryCatch({
    suppressMessages(
      get_ona_page(
        api_url = "https://fakerapi.it/api/v1/addresses?_quantity=10",
        api_token = NULL
      )
    )
  }, error = function(e) {
    testthat::skip(paste("External API unavailable:", e$message))
  })

  testthat::expect_type(result$data, "list")
  testthat::expect_equal(result$status, "OK")
  testthat::expect_equal(result$code, 200)
  testthat::expect_equal(result$total, 10)
})

testthat::test_that("check_status_api when its success", {
  testthat::skip_on_cran()

  # get response
  response <- tryCatch({
    httr::HEAD("https://fakerapi.it/api/v1/addresses?_quantity=10")
  }, error = function(e) {
    testthat::skip(paste("External API unavailable:", e$message))
  })

  # Skip if API returned an error status

  if (response$status_code >= 500) {
    testthat::skip(paste("External API returned error:", response$status_code))
  }

  # test response
  testthat::expect_invisible(check_status_api(response$status_code))
})

testthat::test_that("check_status_api when it fails", {
  testthat::skip_on_cran()

  # get response
  response <- tryCatch({
    httr::HEAD("https://fakerapi.it/api/add")
  }, error = function(e) {
    testthat::skip(paste("External API unavailable:", e$message))
  })

  # test response
  testthat::expect_error(check_status_api(response))
})

# Regression tests for redundant `available_ona_forms()` calls ----------------
# Background: `get_ona_data()` validates form IDs by calling
# `available_ona_forms()` once. Prior to this fix, `get_ona_form()` also
# called `available_ona_forms()` on every invocation, so a single-form
# `get_ona_data()` call produced two `available_ona_forms()` lookups
# (each itself a HEAD + GET pair). With `validate = FALSE`, the inner
# check is skipped when the caller has already done it.

testthat::test_that("get_ona_form respects validate = FALSE", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mockery")

  call_count <- 0L
  fake_available <- function(base_url, api_token) {
    call_count <<- call_count + 1L
    data.frame(id = 269L)
  }
  fake_paginated <- function(api_url, api_token) {
    data.frame(`_id` = 1:2, val = c("a", "b"), check.names = FALSE)
  }

  mockery::stub(get_ona_form, "available_ona_forms", fake_available)
  mockery::stub(get_ona_form, "get_paginated_data", fake_paginated)

  # validate = FALSE should NOT call available_ona_forms
  suppressMessages(
    get_ona_form(form_id = 269, api_token = "x", validate = FALSE)
  )
  testthat::expect_equal(call_count, 0L)

  # validate = TRUE (the default) should still call it
  suppressMessages(
    get_ona_form(form_id = 269, api_token = "x", validate = TRUE)
  )
  testthat::expect_equal(call_count, 1L)
})

testthat::test_that("get_ona_data only validates form IDs once", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mockery")

  call_count <- 0L
  fake_available <- function(base_url, api_token) {
    call_count <<- call_count + 1L
    data.frame(id = c(269L, 270L))
  }
  # Skip the actual download — return a minimal frame
  fake_form <- function(base_url, form_id, api_token,
                       selected_columns, logical_filters,
                       comparison_filters, validate) {
    # Confirm get_ona_data passes validate = FALSE
    testthat::expect_false(validate)
    data.frame(`_id` = form_id, check.names = FALSE)
  }

  mockery::stub(get_ona_data, "available_ona_forms", fake_available)
  mockery::stub(get_ona_data, "get_ona_form", fake_form)
  # Stub the internal GPS helper since our fake data has no GPS columns
  mockery::stub(get_ona_data, ".parse_gps_columns", function(x) x)

  suppressMessages(
    get_ona_data(form_ids = c(269L, 270L), api_token = "x")
  )

  # Should be exactly one availability check, regardless of form count
  testthat::expect_equal(call_count, 1L)
})

testthat::test_that("get_ona_data forwards base_url to get_ona_form", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mockery")

  captured_base_url <- NULL
  fake_available <- function(base_url, api_token) {
    data.frame(id = 269L)
  }
  fake_form <- function(base_url, form_id, api_token,
                       selected_columns, logical_filters,
                       comparison_filters, validate) {
    captured_base_url <<- base_url
    data.frame(`_id` = form_id, check.names = FALSE)
  }

  mockery::stub(get_ona_data, "available_ona_forms", fake_available)
  mockery::stub(get_ona_data, "get_ona_form", fake_form)
  mockery::stub(get_ona_data, ".parse_gps_columns", function(x) x)

  suppressMessages(
    get_ona_data(
      base_url = "https://staging.example.org",
      form_ids = 269L,
      api_token = "x"
    )
  )

  testthat::expect_equal(captured_base_url, "https://staging.example.org")
})
