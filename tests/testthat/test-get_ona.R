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

# Regression tests for row-duplication via multi-element atomic vectors -------
# Background: ONA returns submission geolocation as a JSON array of scalars,
# e.g. "_geolocation": [12.21, 4.38]. jsonlite::fromJSON parses this as an
# atomic numeric vector of length 2. Prior to this fix, flatten_ona_record
# stored the entire vector under a single key, and the subsequent
# as.data.frame() call recycled the length-1 fields to match, producing
# two rows per submission and silently doubling row counts wherever a
# geolocation (or any other JSON array of scalars) was present.

testthat::test_that(
  "flatten_ona_record expands multi-element atomic vectors into indexed keys",
  {
    record <- list(
      `_id`          = 12345L,
      `_geolocation` = c(12.212685, 4.381303),
      Name           = "Test"
    )

    flat <- flatten_ona_record(record)

    testthat::expect_true("_geolocation[1]" %in% names(flat))
    testthat::expect_true("_geolocation[2]" %in% names(flat))
    testthat::expect_false("_geolocation"   %in% names(flat))

    testthat::expect_equal(flat[["_geolocation[1]"]], "12.212685")
    testthat::expect_equal(flat[["_geolocation[2]"]], "4.381303")
    testthat::expect_equal(flat[["_id"]],  "12345")
    testthat::expect_equal(flat[["Name"]], "Test")
  }
)

testthat::test_that(
  "flattened record converts to a single-row data frame (no recycling)",
  {
    record <- list(
      `_id`          = 12345L,
      `_geolocation` = c(12.212685, 4.381303),
      Name           = "Test"
    )

    df <- as.data.frame(
      flatten_ona_record(record),
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )

    # The whole point: ONE row per submission, regardless of geolocation.
    testthat::expect_equal(nrow(df), 1L)
    testthat::expect_equal(df[["_id"]],  "12345")
    testthat::expect_equal(df[["Name"]], "Test")
  }
)

testthat::test_that(
  "flatten_ona_record preserves length-1 atomic behaviour", {
    record <- list(`_id` = 1L, Name = "Solo", States = c("BORNO"))

    flat <- flatten_ona_record(record)

    # Length-1 vectors should still use the plain key, not indexed form
    testthat::expect_true("States" %in% names(flat))
    testthat::expect_false("States[1]" %in% names(flat))
    testthat::expect_equal(flat[["States"]], "BORNO")
  }
)

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
  # no server check: this test is about validate only
  mockery::stub(get_ona_form, ".resolve_ona_base_url", function(base_url, api_token) base_url)

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
  mockery::stub(get_ona_data, ".resolve_ona_base_url", function(base_url, api_token) base_url)
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

# ONA server move: api.whonghub.org -> api.padacor.org -------------------------

fake_ona_response <- function(url, status = 200L, type = "application/json") {
  structure(
    list(url = url, status_code = status, headers = list(`content-type` = type)),
    class = "response"
  )
}

testthat::test_that("the default ONA address is api.padacor.org", {
  testthat::expect_equal(formals(get_ona_data)$base_url, "https://api.padacor.org")
  testthat::expect_equal(formals(get_ona_form)$base_url, "https://api.padacor.org")
  testthat::expect_equal(formals(available_ona_forms)$base_url, "https://api.padacor.org")
  testthat::expect_equal(formals(get_updated_ona_data)$base_url, "https://api.padacor.org")
})

testthat::test_that("api.padacor.org is used once it answers as an ONA server", {
  testthat::skip_if_not_installed("mockery")
  .ona_url_state$resolved <- NULL
  calls <- 0L
  mockery::stub(.resolve_ona_base_url, "httr::GET", function(...) {
    calls <<- calls + 1L
    fake_ona_response("https://api.padacor.org/api/v1/user", 401L)
  })
  testthat::expect_equal(
    .resolve_ona_base_url("https://api.padacor.org", "x"), "https://api.padacor.org"
  )
  # checked once per session
  .resolve_ona_base_url("https://api.padacor.org/", "x")
  testthat::expect_equal(calls, 1L)
  .ona_url_state$resolved <- NULL
})

testthat::test_that("it falls back to api.whonghub.org while api.padacor.org isn't live", {
  testthat::skip_if_not_installed("mockery")
  .ona_url_state$resolved <- NULL
  # the "Page has been disabled" page: HTML, redirected to whonghub.org
  mockery::stub(.resolve_ona_base_url, "httr::GET", function(...) {
    fake_ona_response("https://whonghub.org/", 200L, "text/html; charset=utf-8")
  })
  msgs <- testthat::capture_messages(
    url <- .resolve_ona_base_url("https://api.padacor.org", "x")
  )
  testthat::expect_equal(url, "https://api.whonghub.org")
  testthat::expect_true(any(grepl("isn't answering", msgs)))
  .ona_url_state$resolved <- NULL

  # no connection at all
  mockery::stub(.resolve_ona_base_url, "httr::GET", function(...) stop("Could not resolve host"))
  testthat::expect_equal(
    suppressMessages(.resolve_ona_base_url("https://api.padacor.org", "x")),
    "https://api.whonghub.org"
  )
  .ona_url_state$resolved <- NULL
})

testthat::test_that("other ONA addresses are never checked or changed", {
  testthat::skip_if_not_installed("mockery")
  .ona_url_state$resolved <- NULL
  mockery::stub(.resolve_ona_base_url, "httr::GET", function(...) stop("should not be called"))
  testthat::expect_equal(
    .resolve_ona_base_url("https://api.whonghub.org/api/v1", "x"), "https://api.whonghub.org"
  )
  testthat::expect_equal(
    .resolve_ona_base_url("https://staging.example.org", "x"), "https://staging.example.org"
  )
  testthat::expect_error(.resolve_ona_base_url("not a url", "x"), "not a valid base URL")
})
