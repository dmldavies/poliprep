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
