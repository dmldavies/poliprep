#' Universal Console Clearing
#'
#' Attempts multiple methods to clear the console across different R environments
#' including RStudio, VSCode, Positron, and various terminal applications.
#'
#' @return Invisible NULL
#' @keywords internal
#' @noRd
.clear_console <- function() {
  if (!interactive()) return(invisible(NULL))

  # Method 1: Form feed (works in RStudio)
  cat("\014")

  # Method 2: ANSI escape codes (works in most terminals including VSCode)
  cat("\033[2J\033[H")

  # Method 3: System call for Unix/Mac terminals
  if (.Platform$OS.type == "unix") {
    system("clear", ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  # Method 4: System call for Windows
  if (.Platform$OS.type == "windows") {
    system("cls", ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  invisible(NULL)
}

#' Save Dataframe Using sntutils Write Function
#'
#' Prompts the user for confirmation before saving a dataframe to a file.
#' Supports all file formats supported by the sntutils write() function
#' including RDS, CSV, Excel, DTA, TSV, and more. If the specified file path
#' does not exist or is NULL, the user is prompted to provide a new path.
#' A default file name is used if no valid path is provided. If a cache file
#' already exists, the function merges new data with existing data, ensuring
#' that the most recent entries (based on `created_time`) are retained.
#'
#' @param data_to_save DataFrame to be saved.
#' @param default_save_path Default path for saving the dataframe. If not
#'                     provided or invalid, the user is prompted for a new path.
#'                     File extension determines format (e.g., .rds, .csv, .xlsx,
#'                     .dta, .tsv). Defaults to \code{NULL}.
#'
#' @return Invisible \code{NULL}. The function's primary purpose is to saving a
#'        file, not to return a value.
#'
#' @examples
#' # handle_file_save(data_to_save, "path/to/default/location.rds")
#' # handle_file_save(data_to_save, "path/to/default/location.csv")
#' # handle_file_save(data_to_save, "path/to/default/location.xlsx")
#' # handle_file_save(data_to_save, "path/to/default/location.dta")
#' # handle_file_save(data_to_save, "path/to/default/location.tsv")
#'
#' @keywords internal
#' @noRd
handle_file_save <- function(data_to_save, default_save_path = NULL) {
  cache_path <- default_save_path

  while (TRUE) {
    # Prompt user for confirmation
    confirm_save <- tolower(
      readline("Do you want to save the cleaned cache file? [y/n]: ")
    )

    if (confirm_save == "y") {
      # Validate or set a default cache path
      if (is.null(cache_path) || dir.exists(cache_path)) {
        cli::cli_alert_warning(
          "The specified path is null or is a directory."
        )

        # Prompt for a valid file path
        cache_path <- readline(
          prompt = "Enter the new file path (including filename) for saving: "
        )

        # Ensure the file path includes a filename
        if (cache_path == "" || dir.exists(cache_path)) {
          cache_path <- file.path(getwd(), "prepped_geoname_cache.rds")
          cli::cli_alert_info(
            "No valid path provided. Using default: {cache_path}"
          )
        }
      }

      # Ensure parent directory exists
      cache_dir <- dirname(cache_path)
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
      }

      # Ensure we are working with a file, not a directory
      if (dir.exists(cache_path)) {
         cli::cli_abort("`cache_path` should be a file path, not a directory.")
      }

      # Merge with any cache already on disk (latest decision per name and
      # location wins) and save
      .save_geoname_cache(data_to_save, cache_path)
      cli::cli_alert_success("File saved successfully to {cache_path}.")
      break
    } else if (confirm_save == "n") {
      cli::cli_alert_info("File not saved. Proceeding without saving...")
      break
    } else {
      cli::cli_alert_warning("Invalid input. Please respond with 'y' or 'n'.")
    }
  }
}

# Geoname correction cache ------------------------------------------------------
#
# Every correction made in the interactive menu is stored as one row:
#
#   level           "level0".."level4": position of the column in the call
#   name_to_match   the name as it appeared in the data (e.g. MUNICIPAL AREA
#                   COUNCIL)
#   replacement     the name it was corrected to (e.g. AMAC)
#   level0_prepped.. the corrected names of the parent levels, i.e. where the
#   level4_prepped  name was found (e.g. NIGERIA, FCT), plus the replacement at
#                   its own level
#   created_time, name_of_creator
#
# A correction is identified by its level, its parents and the original name,
# so the same ward name can be corrected differently in two LGAs. When the
# same correction is made twice, the most recent one wins.

#' Columns of a geoname cache, in order
#' @keywords internal
#' @noRd
.geoname_cache_cols <- c(
  "level", "name_to_match", "replacement",
  paste0("level", 0:4, "_prepped"),
  "created_time", "name_of_creator",
  # "yes" once someone has confirmed that a correction merging two names that
  # look different is intended (see .find_geoname_merges())
  "reviewed"
)

#' Normalise admin names so that spelling, not spacing or case, is compared
#'
#' Upper-cases, turns non-breaking spaces, tabs and line breaks into spaces,
#' trims and collapses repeated spaces.
#' @keywords internal
#' @noRd
.normalise_geoname <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x)) return(x)
  # str_squish() treats tabs, line breaks and non-breaking spaces as spaces
  toupper(stringr::str_squish(x))
}

#' Default location of the geoname cache
#'
#' Used when `prep_geonames()` is called without `cache_path`. Set
#' `options(poliprep.geoname_cache = "path/to/cache.rds")` to use another file,
#' for example one shared by a team.
#' @keywords internal
#' @noRd
.default_geoname_cache_path <- function() {
  path <- getOption("poliprep.geoname_cache")
  if (!is.null(path)) return(path)

  user_dir <- get0("R_user_dir", envir = asNamespace("tools"), inherits = FALSE)
  base <- if (is.function(user_dir)) {
    user_dir("poliprep", which = "data")
  } else {
    file.path(path.expand("~"), ".poliprep")
  }
  file.path(base, "geoname_cache.rds")
}

#' Bring a cache from any version or file format into one shape
#'
#' Renames columns from older cache versions, adds missing columns and stores
#' everything as text, so caches saved as .rds, .csv or .xlsx combine cleanly.
#' Extra columns are kept.
#' @keywords internal
#' @noRd
.standardise_geoname_cache <- function(cache) {
  template <- stats::setNames(
    as.data.frame(
      replicate(length(.geoname_cache_cols), character(), simplify = FALSE),
      stringsAsFactors = FALSE
    ),
    .geoname_cache_cols
  )
  if (is.null(cache) || NROW(cache) == 0) {
    return(template)
  }

  cache <- as.data.frame(cache, stringsAsFactors = FALSE)
  if (inherits(cache, "sf")) cache <- sf::st_drop_geometry(cache)

  # column names used by older versions of the cache
  old_names <- c(
    country_prepped = "level0_prepped",
    province_prepped = "level1_prepped",
    district_prepped = "level2_prepped",
    subdistrict_prepped = "level3_prepped",
    settlements_prepped = "level4_prepped"
  )
  for (old in names(old_names)) {
    if (old %in% names(cache) && !(old_names[[old]] %in% names(cache))) {
      names(cache)[names(cache) == old] <- old_names[[old]]
    }
  }

  for (col in .geoname_cache_cols) {
    if (!col %in% names(cache)) cache[[col]] <- NA_character_
  }

  # times come back as dates from csv/xlsx; keep one text format
  if (inherits(cache$created_time, "POSIXt")) {
    cache$created_time <- format(
      cache$created_time, "%Y-%m-%d %H:%M:%S",
      tz = "UTC", usetz = TRUE
    )
  }
  for (col in .geoname_cache_cols) {
    cache[[col]] <- as.character(cache[[col]])
  }

  old_levels <- c(
    country = "level0", province = "level1", district = "level2",
    subdistrict = "level3", settlements = "level4"
  )
  is_old <- !is.na(cache$level) & cache$level %in% names(old_levels)
  cache$level[is_old] <- old_levels[cache$level[is_old]]

  # older caches may only hold the corrected name in levelX_prepped
  k <- suppressWarnings(as.integer(sub("^level", "", cache$level)))
  for (j in 0:4) {
    fill <- is.na(cache$replacement) & !is.na(k) & k == j
    cache$replacement[fill] <- cache[[paste0("level", j, "_prepped")]][fill]
  }

  cache[, c(.geoname_cache_cols, setdiff(names(cache), .geoname_cache_cols)),
    drop = FALSE
  ]
}

#' Key identifying one correction: level + parent names + original name
#' @keywords internal
#' @noRd
.geoname_cache_key <- function(cache) {
  k <- suppressWarnings(as.integer(sub("^level", "", cache$level)))
  parents <- rep("", nrow(cache))
  for (j in 0:3) {
    has_parent <- !is.na(k) & k > j
    parent_name <- .normalise_geoname(cache[[paste0("level", j, "_prepped")]])
    parents[has_parent] <- paste0(
      parents[has_parent], "\x1f", parent_name[has_parent]
    )
  }
  paste(cache$level, parents, .normalise_geoname(cache$name_to_match),
    sep = "\x1e"
  )
}

#' Keep only the most recent decision for each correction
#' @keywords internal
#' @noRd
.dedupe_geoname_cache <- function(cache) {
  cache <- .standardise_geoname_cache(cache)
  if (nrow(cache) == 0) {
    return(cache)
  }
  # oldest first; for equal times, later rows count as newer
  ord <- order(cache$created_time, seq_len(nrow(cache)), na.last = FALSE)
  keep <- ord[!duplicated(.geoname_cache_key(cache)[ord], fromLast = TRUE)]
  # keep the surviving rows in their original order
  cache <- cache[sort(keep), , drop = FALSE]
  rownames(cache) <- NULL
  cache
}

#' Apply saved corrections to admin name columns
#'
#' Goes level by level from the top, so a corrected state is in place before
#' the LGAs under it are looked up. Rows are never added, dropped or
#' reordered.
#'
#' @param df Data frame with the admin columns.
#' @param cache Cache rows (any version).
#' @param levels Admin column names, top level first.
#' @return A list: `data`, `rows` (rows changed) and `names` (distinct
#'   corrections used).
#' @keywords internal
#' @noRd
.apply_geoname_cache <- function(df, cache, levels) {
  cache <- .dedupe_geoname_cache(cache)
  if (nrow(cache) == 0 || NROW(df) == 0) {
    return(list(data = df, rows = 0L, names = 0L))
  }

  cache_key <- .geoname_cache_key(cache)
  replacement <- .normalise_geoname(cache$replacement)
  changed <- rep(FALSE, nrow(df))
  used <- character()
  parents <- rep("", nrow(df))

  for (k in seq_along(levels) - 1L) {
    col <- levels[[k + 1L]]
    current <- .normalise_geoname(df[[col]])
    level_rows <- which(cache$level == paste0("level", k))

    if (length(level_rows) > 0) {
      key <- paste(paste0("level", k), parents, current, sep = "\x1e")
      m <- match(key, cache_key[level_rows])
      hit <- !is.na(m) & !is.na(current) &
        !is.na(replacement[level_rows][m])
      if (any(hit)) {
        new_value <- replacement[level_rows][m[hit]]
        differs <- new_value != current[hit]
        df[[col]][hit] <- new_value
        current[hit] <- new_value
        changed[which(hit)[differs]] <- TRUE
        used <- c(used, key[hit][differs])
      }
    }

    parents <- paste0(parents, "\x1f", current)
  }

  list(data = df, rows = sum(changed), names = length(unique(used)))
}

#' Read a geoname cache file (missing file = empty cache)
#' @keywords internal
#' @noRd
.read_geoname_cache <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(.standardise_geoname_cache(NULL))
  }
  cache <- tryCatch(
    read(path),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to read the geoname cache {.file {path}}: {e$message}",
        "i" = "Fix or move the file, or pass another {.arg cache_path}."
      ))
    }
  )
  .standardise_geoname_cache(cache)
}

#' Merge new corrections into the cache file on disk and save it
#'
#' Re-reads the file just before writing, so corrections saved by someone else
#' sharing the file in the meantime are kept.
#' @keywords internal
#' @noRd
.save_geoname_cache <- function(new_rows, path) {
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE)
  }

  if (requireNamespace("filelock", quietly = TRUE)) {
    lock_path <- paste0(path, ".lock")
    lock <- filelock::lock(lock_path, timeout = 10000)
    if (!is.null(lock)) {
      on.exit(
        {
          filelock::unlock(lock)
          unlink(lock_path)
        },
        add = TRUE
      )
    }
  }

  existing <- tryCatch(
    .read_geoname_cache(path),
    error = function(e) NULL
  )
  if (is.null(existing)) {
    # never overwrite a cache we could not read: keep this session's work
    # in a separate file instead
    recovery <- sub(
      "(\\.[^.]+)?$",
      paste0("_recovered_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"),
      path
    )
    save(.dedupe_geoname_cache(new_rows), recovery)
    cli::cli_abort(c(
      "Could not read the existing cache {.file {path}}, so it was not changed.",
      "i" = "This session's corrections were saved to {.file {recovery}}."
    ))
  }

  merged <- .dedupe_geoname_cache(dplyr::bind_rows(existing, .standardise_geoname_cache(new_rows)))
  save(merged, path)
  invisible(merged)
}

#' Recovery file for a menu session in progress
#'
#' Each choice is appended here the moment it is made, so stopping a long
#' session part-way (Esc, an error, closing R) loses nothing: the next run
#' moves these choices into the cache before it starts. The file is removed
#' once the session is saved normally, or discarded with "Exit without
#' saving".
#' @keywords internal
#' @noRd
.geoname_session_path <- function(cache_path) {
  paste0(sub("\\.[^./\\\\]+$", "", cache_path), "_unsaved_session.rds")
}

#' Append cache rows to the recovery file
#' @keywords internal
#' @noRd
.append_geoname_session <- function(rows, path) {
  previous <- if (file.exists(path)) readRDS(path) else NULL
  saveRDS(
    dplyr::bind_rows(.standardise_geoname_cache(previous), .standardise_geoname_cache(rows)),
    path
  )
}

#' Turn menu choices into cache rows
#' @keywords internal
#' @noRd
.choices_to_cache <- function(choices) {
  if (is.null(choices) || NROW(choices) == 0) {
    return(.standardise_geoname_cache(NULL))
  }
  k <- as.integer(sub("^level", "", choices$level))
  # longname_corrected is "<parent>_<parent>_..._<replacement>"
  parts <- strsplit(as.character(choices$longname_corrected), "_", fixed = TRUE)

  out <- data.frame(
    level = choices$level,
    name_to_match = choices$name_to_match,
    replacement = choices$replacement,
    stringsAsFactors = FALSE
  )
  for (j in 0:4) {
    out[[paste0("level", j, "_prepped")]] <- vapply(
      seq_along(parts),
      function(i) {
        if (j < k[i] && length(parts[[i]]) > j) {
          parts[[i]][[j + 1]]
        } else if (j == k[i]) {
          choices$replacement[[i]]
        } else {
          NA_character_
        }
      },
      character(1)
    )
  }
  out$created_time <- as.character(choices$created_time)
  out$name_of_creator <- get_user_identity()
  if ("reviewed" %in% names(choices)) {
    out$reviewed <- as.character(choices$reviewed)
  }
  .standardise_geoname_cache(out)
}

# Merge check ----------------------------------------------------------------------
#
# A correction can wrongly fold one real place into another (e.g. ward
# AIGE SOLOMADE -> IPAKODO in Ikorodu). These helpers spot corrections that
# look like that, so they can be confirmed or undone.

#' How different two names must be (Jaro-Winkler distance, 0-1) before folding
#' one into the other counts as suspicious. Typos are well below this
#' (GARKII/GARKI ~ 0.03); different places are well above it
#' (AIGE SOLOMADE/IPAKODO ~ 0.5).
#' @keywords internal
#' @noRd
.geoname_merge_distance <- 0.25

#' Trailing number or letter that tells numbered places apart (GARKI I/II,
#' TUDUN WADA 1/2, UNGUWAN A/B)
#' @keywords internal
#' @noRd
.geoname_suffix_regex <- "\\s+(I{1,3}|IV|VI{0,3}|IX|X|[0-9]+|[A-D])$"

#' Is replacing `from` with `to` a suspicious merge?
#'
#' Suspicious when `to` already has rows of its own and either looks quite
#' different from `from`, or is a differently numbered place with the same
#' base name.
#' @keywords internal
#' @noRd
.is_suspicious_merge <- function(from, to, to_rows) {
  from <- .normalise_geoname(from)
  to <- .normalise_geoname(to)
  distance <- stringdist::stringdist(from, to, method = "jw")
  base_from <- sub(.geoname_suffix_regex, "", from)
  base_to <- sub(.geoname_suffix_regex, "", to)
  numbered <- base_from == base_to & from != to &
    (grepl(.geoname_suffix_regex, from) | grepl(.geoname_suffix_regex, to))
  from != to & to_rows > 0 & (distance > .geoname_merge_distance | numbered)
}

#' Find corrections that folded one place into another
#'
#' Compares names before and after cleaning, level by level. A row is
#' reported when a name was changed into another name that also appears in
#' the data in its own right (under the same parents), and the two look
#' different or are differently numbered (see `.is_suspicious_merge()`).
#' Corrections confirmed earlier (`reviewed == "yes"` in the cache) are left
#' out.
#'
#' @param before,after The same rows before and after cleaning.
#' @param levels Admin columns, top first.
#' @param cache Current cache (for the reviewed flags).
#' @return A data frame with one row per suspicious correction.
#' @keywords internal
#' @noRd
.find_geoname_merges <- function(before, after, levels, cache = NULL) {
  cache <- .dedupe_geoname_cache(cache)
  cache_key <- .geoname_cache_key(cache)
  reviewed_keys <- cache_key[!is.na(cache$reviewed) & cache$reviewed == "yes"]

  found <- list()
  parents <- rep("", NROW(after))
  parent_label <- rep("", NROW(after))

  for (k in seq_along(levels)) {
    col <- levels[[k]]
    from <- .normalise_geoname(before[[col]])
    to <- .normalise_geoname(after[[col]])
    ok <- !is.na(from) & !is.na(to)

    if (any(ok)) {
      counts <- data.frame(
        parent = parents[ok], parent_label = parent_label[ok],
        from = from[ok], to = to[ok], stringsAsFactors = FALSE
      ) |>
        dplyr::count(parent, parent_label, from, to, name = "rows")

      # rows that carried the target name before cleaning
      own <- counts[counts$from == counts$to, c("parent", "to", "rows")]
      names(own)[3] <- "to_own_rows"

      changed <- counts[counts$from != counts$to, , drop = FALSE] |>
        dplyr::left_join(own, by = c("parent", "to")) |>
        dplyr::mutate(to_own_rows = dplyr::coalesce(to_own_rows, 0L))

      if (nrow(changed) > 0) {
        changed$key <- paste(paste0("level", k - 1), changed$parent,
          changed$from,
          sep = "\x1e"
        )
        suspicious <- changed[
          .is_suspicious_merge(changed$from, changed$to, changed$to_own_rows) &
            !(changed$key %in% reviewed_keys), ,
          drop = FALSE
        ]
        if (nrow(suspicious) > 0) {
          suspicious$level <- col
          suspicious$level_index <- k - 1
          found[[length(found) + 1]] <- suspicious
        }
      }
    }

    parents <- paste0(parents, "\x1f", to)
    parent_label <- ifelse(
      parent_label == "", to, paste(parent_label, to, sep = " / ")
    )
  }

  if (length(found) == 0) {
    return(data.frame())
  }
  out <- dplyr::bind_rows(found)
  out$where <- sub("^ / ", "", out$parent_label)
  out[, c(
    "level", "where", "from", "to", "rows", "to_own_rows",
    "level_index", "parent", "key"
  )]
}

#' Describe suspicious merges for the console
#' @keywords internal
#' @noRd
.describe_geoname_merges <- function(merges) {
  sprintf(
    "%s: %s (%s rows) -> %s (already %s rows)%s",
    merges$level, merges$from, merges$rows, merges$to, merges$to_own_rows,
    ifelse(merges$where == "", "", paste0(" in ", merges$where))
  )
}

#' Change rows in a cache file in place (locked)
#' @keywords internal
#' @noRd
.update_geoname_cache <- function(path, update) {
  if (requireNamespace("filelock", quietly = TRUE)) {
    lock_path <- paste0(path, ".lock")
    lock <- filelock::lock(lock_path, timeout = 10000)
    if (!is.null(lock)) {
      on.exit(
        {
          filelock::unlock(lock)
          unlink(lock_path)
        },
        add = TRUE
      )
    }
  }
  cache <- .dedupe_geoname_cache(.read_geoname_cache(path))
  cache <- update(cache, .geoname_cache_key(cache))
  save(cache, path)
  invisible(cache)
}

#' Ask, for each suspicious merge, whether to keep it or undo it
#'
#' Keep: the cache entry is marked reviewed and not asked about again.
#' Undo: the cache entry is removed and the rows get their original name
#' back, so the name is offered in the menu again on the next run.
#'
#' @return `after`, with undone merges reverted.
#' @keywords internal
#' @noRd
.review_geoname_merges <- function(merges, before, after, levels, cache_path) {
  keep_keys <- character()
  undo_keys <- character()

  cli::cli_h2("Merge check")
  cli::cli_alert_warning(
    "{nrow(merges)} correction{?s} fold{?s/} one place into another that is already in the data:"
  )
  for (i in seq_len(nrow(merges))) {
    m <- merges[i, ]
    cli::cli_text("")
    cli::cli_text(
      "{.strong {m$level}} {.val {m$from}} ({m$rows} rows) {cli::symbol$arrow_right} ",
      "{.val {m$to}} (already {m$to_own_rows} rows)",
      if (m$where != "") " in {m$where}" else ""
    )
    answer <- ""
    while (!answer %in% c("k", "u")) {
      answer <- tolower(trimws(readline(
        "  [k] keep (they are the same place)  [u] undo: "
      )))
    }
    if (answer == "k") keep_keys <- c(keep_keys, m$key)
    if (answer == "u") undo_keys <- c(undo_keys, m$key)
  }

  # put the original names back on undone rows
  if (length(undo_keys) > 0) {
    undo <- merges[merges$key %in% undo_keys, , drop = FALSE]
    parents <- rep("", NROW(after))
    for (k in seq_along(levels)) {
      col <- levels[[k]]
      to <- .normalise_geoname(after[[col]])
      from <- .normalise_geoname(before[[col]])
      u <- undo[undo$level_index == k - 1, , drop = FALSE]
      if (nrow(u) > 0) {
        hit <- paste(parents, from, to, sep = "\x1e") %in%
          paste(u$parent, u$from, u$to, sep = "\x1e")
        after[[col]][hit] <- before[[col]][hit]
      }
      parents <- paste0(parents, "\x1f", .normalise_geoname(after[[col]]))
    }
  }

  if (!is.null(cache_path) && file.exists(cache_path)) {
    tryCatch(
      .update_geoname_cache(cache_path, function(cache, key) {
        cache$reviewed[key %in% keep_keys] <- "yes"
        cache[!(key %in% undo_keys), , drop = FALSE]
      }),
      error = function(e) {
        cli::cli_alert_danger(
          "Could not update {.file {cache_path}}: {conditionMessage(e)}"
        )
      }
    )
  }

  if (length(undo_keys) > 0) {
    cli::cli_alert_success(
      "Undid {length(undo_keys)} correction{?s}; you'll be asked about {?it/them} again next run."
    )
  }
  if (length(keep_keys) > 0) {
    cli::cli_alert_success(
      "Kept {length(keep_keys)} correction{?s}; marked as reviewed."
    )
  }
  after
}

#' Get Hierarchical Administrative Combinations
#'
#' Helper function to extract unique hierarchical combinations of administrative
#' levels from a dataframe. This ensures proper counting of admin units as
#' combinations rather than isolated values.
#'
#' @param df A dataframe containing administrative level columns
#' @param levels Character vector of column names representing hierarchical
#'  admin levels
#'
#' @return A dataframe with unique combinations of the specified levels,
#'         excluding rows with any NA values
#'
#' @details This function is used internally by calculate_match_stats to ensure
#'          administrative units are counted as hierarchical combinations
#'          (e.g., country-province-district) rather than as isolated unique
#'          values
#'
#' @examples
#' # df <- data.frame(
#' #   country = c("A", "A", "B"),
#' #   province = c("P1", "P2", "P1"),
#' #   district = c("D1", "D1", "D2")
#' # )
#' # get_hierarchical_combinations(df, c("country", "province", "district"))
#'
#' @keywords internal
#' @noRd
get_hierarchical_combinations <- function(df, levels) {
  if (length(levels) == 0 || is.null(levels)) {
    return(data.frame())
  }

  # If input is an sf object, drop geometry to avoid list columns
  if (inherits(df, "sf")) {
    df <- sf::st_drop_geometry(df)
  }

  # Filter to only the specified levels that exist in the dataframe
  existing_levels <- levels[levels %in% names(df)]
  if (length(existing_levels) == 0) {
    return(data.frame())
  }

  # Remove rows with any NA values and get unique combinations
  df_subset <- df[, existing_levels, drop = FALSE]
  df_clean <- df_subset[stats::complete.cases(df_subset), , drop = FALSE]
  unique(df_clean)
}

#' Calculate and report geo-naming match statistics
#'
#' Compares entries in a dataset against a lookup across specified admin levels
#' (e.g., country, province/state/region, district, subdistrict, settlement)
#' and reports match statistics to the console.
#'
#' @param data A data frame containing the target data to be matched.
#' @param lookup_data A data frame serving as the reference for matching.
#' @param level0 Column name (country) present in both `data` and `lookup_data`.
#' @param level1 Column name (province/state/region) present in both datasets.
#' @param level2 Column name (district) present in both datasets.
#' @param level3 Column name (subdistrict) present in both datasets.
#' @param level4 Column name (settlement) present in both datasets.
#'
#' @details
#' - Input columns supplied via `level*` are normalized to lower case before
#'   matching.
#' - *Base N* for each side is the count of **unique hierarchical names**
#'   formed from the supplied levels (e.g., `level0_level1_...`). Per-level
#'   rows show matches out of each side's Base N.
#' - Console output includes:
#'   1) a two-column summary (Target vs Lookup as base N),
#'   2) side-level completeness messages (success/info/warning),
#'   3) a per-level report of **missing names** (NA or empty strings) on
#'      either side. These missing names are **not included in N** only if the
#'      implementation drops them before counting (see Note below).
#'
#' @note
#' If you want missing names (NA/empty) **excluded from Base N**, ensure the
#' key-building step drops them before counting (see example patch below).
#'
#' @return Invisibly returns `NULL`. Output is produced via `cli` messages.
#'
#' @examples
#' # minimal runnable example (toy data)
#' data <- data.frame(
#'   country = c("Kenya", "Kenya", "Uganda"),
#'   district = c("Nairobi", "Kisumu", "Kampala")
#' )
#' lookup <- data.frame(
#'   country = c("Kenya", "Uganda"),
#'   district = c("Nairobi", "Kampala")
#' )
#' calculate_match_stats(
#'   data, lookup, level0 = "country", level2 = "district"
#' )
#' @export
calculate_match_stats <- function(
  data,
  lookup_data,
  level0 = NULL,
  level1 = NULL,
  level2 = NULL,
  level3 = NULL,
  level4 = NULL
) {

  # normalize case
  levels_vec <- c(level0, level1, level2, level3, level4) |>
    (\(v) v[!vapply(v, is.null, logical(1))])()

  if (length(levels_vec) > 0) {
    for (lv in levels_vec) {
      if (lv %in% names(data)) {
        data[[lv]] <- tolower(as.character(data[[lv]]))
      }
      if (lv %in% names(lookup_data)) {
        lookup_data[[lv]] <- tolower(as.character(lookup_data[[lv]]))
      }
    }
  }

  # helpers ---------------------------------------------------------------

  compose_fields <- function(...) {
    f <- c(...)
    f[!vapply(f, is.null, logical(1))]
  }

  build_keys <- function(df, fields) {
    if (length(fields) == 0) {
      return(character(0))
    }
    combos <- get_hierarchical_combinations(df, fields)
    if (nrow(combos) == 0) {
      return(character(0))
    }
    if (length(fields) == 1) {
      return(unique(combos[[fields]]))
    }
    unique(do.call(paste, c(combos[fields], sep = "_")))
  }

  resolve_label <- function(label, fallback) {
    if (is.null(label) || is.na(label) || identical(label, "")) {
      fallback
    } else {
      label
    }
  }

  big_mark <- function(n) {
    if (is.na(n)) "NA" else formatC(n, format = "d", big.mark = ",")
  }

  paint_matches <- function(matches, total) {
    num <- big_mark(matches)
    if (!is.na(matches) && !is.na(total) && matches < total) {
      cli::col_red(num)
    } else {
      num
    }
  }

  results <- list()

  process_level <- function(level_key, level_num, fields, label) {
    data_keys <- build_keys(data, fields)
    lookup_keys <- build_keys(lookup_data, fields)
    matches <- length(base::intersect(data_keys, lookup_keys))
    results[[level_key]] <<- list(
      label = resolve_label(label, level_key),
      level_num = level_num,
      matches = matches,
      total_data = length(data_keys),
      total_lookup = length(lookup_keys)
    )
  }

  # compute per-level
  if (!is.null(level0)) {
    process_level("level0", 0L, compose_fields(level0), level0)
  }
  if (!is.null(level1)) {
    f1 <- if (!is.null(level0)) {
      compose_fields(level0, level1)
    } else {
      compose_fields(level1)
    }
    process_level("level1", 1L, f1, level1)
  }
  if (!is.null(level2)) {
    process_level("level2", 2L, compose_fields(level0, level1, level2), level2)
  }
  if (!is.null(level3)) {
    process_level(
      "level3",
      3L,
      compose_fields(level0, level1, level2, level3),
      level3
    )
  }
  if (!is.null(level4)) {
    process_level(
      "level4",
      4L,
      compose_fields(level0, level1, level2, level3, level4),
      level4
    )
  }

  # enforce display order
  ordered_keys <- c("level0", "level1", "level2", "level3", "level4")
  rows <- results[ordered_keys]
  rows <- rows[!vapply(rows, is.null, logical(1))]

  # headers
  left_hdr <- "Target data as base N"
  right_hdr <- "Lookup data as base N"

  bullet <- "\u2022"

  build_left <- function(r) {
    glue::glue(
      "{bullet} {r$label} (level{r$level_num}): ",
      "{paint_matches(r$matches, r$total_data)} out of ",
      "{big_mark(r$total_data)} matched"
    )
  }
  build_right <- function(r) {
    glue::glue(
      "{bullet} {r$label} (level{r$level_num}): ",
      "{paint_matches(r$matches, r$total_lookup)} out of ",
      "{big_mark(r$total_lookup)} matched"
    )
  }

  left_lines <- if (length(rows)) {
    vapply(rows, build_left, character(1))
  } else {
    character(0)
  }
  right_lines <- if (length(rows)) {
    vapply(rows, build_right, character(1))
  } else {
    character(0)
  }

  # two columns
  content_vec <- cli::ansi_columns(
    c(
      cli::col_cyan(left_hdr),
      left_lines,
      cli::col_cyan(right_hdr),
      right_lines
    ),
    width = max(40L, cli::console_width() - 4L),
    max_cols = 2,
    fill = "col",
    align = "left",
    sep = "  "
  )

  cli::cli_h1(glue::glue("{cli::symbol$info} Match Summary"))
   cat("\n")
  # compute completeness by side across all reported levels
  # (complete = all matches equal denominators on that side)
  target_complete <- length(rows) > 0 &&
    all(vapply(
      rows,
      \(r) isTRUE(r$matches == r$total_data),
      logical(1)
    ))
  lookup_complete <- length(rows) > 0 &&
    all(vapply(
      rows,
      \(r) isTRUE(r$matches == r$total_lookup),
      logical(1)
    ))

  # emit side-by-side summary message
  if (target_complete && lookup_complete) {
    # both sides perfect
    cli::cli_alert_success(
      "Hierarchies are aligned across data and lookup."
    )
  } else if (target_complete && !lookup_complete) {
    # target perfect, lookup has extras
    cli::cli_alert_info("Lookup has extra names not in data.")
  } else if (!target_complete && lookup_complete) {
    # lookup perfect, target has extras
    cli::cli_alert_info("Data has names not in lookup.")
  } else {
    # both imperfect
    cli::cli_alert_warning(
      "Both sides have unmatched names; see per-level lines below."
    )
  }

  # check and report missing names per provided level columns
  # (counts of NA or empty-string values in each dataset)
  if (length(levels_vec) > 0) {
    # count missing in data
    miss_data <- vapply(
      levels_vec,
      \(lv) {
        if (lv %in% names(data)) {
          sum(is.na(data[[lv]]) | data[[lv]] == "", na.rm = TRUE)
        } else {
          0L
        }
      },
      integer(1)
    )

    # count missing in lookup
    miss_lookup <- vapply(
      levels_vec,
      \(lv) {
        if (lv %in% names(lookup_data)) {
          sum(is.na(lookup_data[[lv]]) | lookup_data[[lv]] == "", na.rm = TRUE)
        } else {
          0L
        }
      },
      integer(1)
    )

    any_missing <- any(miss_data > 0L | miss_lookup > 0L)

    if (isTRUE(any_missing)) {
      # header alert
      cli::cli_alert_warning(
        "Missing names detected in supplied levels (not included in N)."
      )

      # per-level report lines (only show sides with missing values)
      for (i in seq_along(levels_vec)) {
        msgs <- c()
        if (miss_data[i] > 0L) {
          msgs <- c(msgs, glue::glue("data = {miss_data[i]}"))
        }
        if (miss_lookup[i] > 0L) {
          msgs <- c(msgs, glue::glue("lookup = {miss_lookup[i]}"))
        }
        if (length(msgs) > 0L) {
          cli::cli_text(cli::col_silver(
            glue::glue("- {levels_vec[i]}: {paste(msgs, collapse = ', ')}")
          ))
        }
      }
    }
  }
   cat("\n")
  # final box, no borders
  cli::cat_line(content_vec)
}

#' Format a single choice for display
#'
#' @param index Numeric index of the choice
#' @param choice Character string of the choice text
#' @param width Total width of the formatted choice
#' @return Formatted choice string
#' @noRd
format_choice <- function(index, choice, width) {
  number_part <- sprintf("%3d: ", index)
  remaining_width <- width - nchar(number_part)
  if (nchar(choice) > remaining_width - 4) { # -4 for "... "
    choice_part <- paste0(substr(choice, 1, remaining_width - 4), "... ")
  } else {
    choice_part <- choice
  }
  choice_part <- stringr::str_pad(choice_part, remaining_width, "right")
  paste0(number_part, choice_part)
}

#' Format all choices into columns
#'
#' @param choices Vector of choice strings
#' @param num_columns Number of columns to display
#' @param column_width Width of each column
#' @return Vector of formatted choice strings, one per row
#' @noRd
format_choices <- function(choices, num_columns, column_width = 60) {
  num_choices <- length(choices)
  rows_per_column <- ceiling(num_choices / num_columns)

  formatted_choices <- character(rows_per_column)
  for (i in 1:rows_per_column) {
    row_parts <- character(num_columns)
    for (j in 1:num_columns) {
      index <- (j - 1) * rows_per_column + i
      if (index <= num_choices) {
        row_parts[j] <- format_choice(index, choices[index], column_width)
      } else {
        row_parts[j] <- paste(rep(" ", column_width), collapse = "")
      }
    }
    formatted_choices[i] <- paste(row_parts, collapse = "")
  }
  formatted_choices
}

#' Display a Custom Menu and Capture User Choice
#'
#' An alternative to base R's `menu` function, using `cli` for enhanced
#' interactivity and style. Displays a menu with given options and special
#' actions, capturing user selection through a custom prompt.
#'
#' @param title The menu title.
#' @param main_header The main header for the menu display.
#' @param choices_input Vector of option strings to display.
#' @param special_actions Named list of special actions with string identifiers.
#' @param prompt String to display for user input prompt.
#' @param column_width Numeric; the maximum width (in characters) for each
#'        column in the menu display. Default is 60.
#'
#' @return The selected option's identifier (numeric or special action key).
#' @importFrom cli cli_h1 cli_h2 cli_text cli_alert_warning
#' @examples
#' # display_custom_menu("Choose an option:", "Main Menu",
#' #                     c("Option 1", "Option 2"),
#' #                     list(x = "Skip", y = "Save"), "Your choice:", 20)
#'
#' @keywords internal
#' @noRd
display_custom_menu <- function(title, main_header, choices_input,
                                special_actions, prompt,
                                column_width = 60) {
  cli::cli_h1(main_header)
  cli::cli_h2(title)

  num_choices <- length(choices_input)
  num_columns <- if (num_choices > 50) 3 else if (num_choices > 25) 2 else 1

  formatted_choices <- format_choices(choices_input, num_columns,
    column_width = column_width
  )
  cat("\n")
  cat(formatted_choices, sep = "\n")
  cat("\n")

  for (key in names(special_actions)) {
    cat(sprintf("%s: %s\n", key, special_actions[[key]]))
  }

  cat("\n")
  repeat {
    choice <- tolower(readline(prompt = paste0(prompt)))
    if (choice %in% c(
      as.character(seq_along(choices_input)),
      tolower(names(special_actions))
    )) {
      break
    }
    cat("Invalid choice, please try again.\n")
  }

  choice
}

#' Calculate String distances Between Admin Names
#'
#' Computes the string distances between administrative names to be cleaned and
#' a set of lookup administrative names using the specified method. Returns the
#' top N closest matches for each name.
#'
#' @param admins_to_clean A vector of administrative names to be cleaned.
#' @param lookup_admins A vector of administrative names for lookup.
#' @param method The method used to calculate string distances (e.g., "jaro",
#'        "levenshtein"). It can take a number of alo
#'
#' @return A dataframe detailing top N matches for each name, including
#'        algorithm_name, name, matches, distances, and ranks.
#'
#'
#' @examples
#' # calculate_string_distance(c("New York", "Los Angeles"),
#' # c("New York", "Los Angeles", "Chicago"),
#' #   method = "lv"
#' # )
#'
#' @keywords internal
#' @noRd
calculate_string_distance <- function(
    admins_to_clean, lookup_admins, method) {
  # Create a data frame with all combinations
  results <- tidyr::expand_grid(
    name_to_match = admins_to_clean,
    matched_names = lookup_admins
  ) |> dplyr::distinct()

  # Calculate distances for each pair
  results <- results |>
    dplyr::rowwise() |>
    dplyr::mutate(
      distance = stringdist::stringdist(
        name_to_match, matched_names,
        method = method
      )
    ) |>
    dplyr::ungroup()

  # Sort results for each name_to_match based on distance
  results <- results |>
    dplyr::group_by(name_to_match) |>
    dplyr::arrange(distance, .by_group = TRUE) |>
    dplyr::mutate(match_rank = dplyr::row_number()) |>
    dplyr::ungroup()

  # Add algorithm name
  results$algorithm_name <- method

  # Reorder columns
  results <- results |>
    dplyr::select(
      algorithm_name, name_to_match,
      matched_names, distance, match_rank
    )

  results
}

#' Interact with Users for Data Cleaning Choices
#'
#' Presents an interactive CLI menu using `cli` for users to make selections on
#' data cleaning choices, particularly for administrative names like countries,
#' provinces, districts, subdistricts and settlements. It allows users to
#' replace, skip, save, or exit, incorporating user feedback into the data
#' cleaning process.
#'
#' @param input_data Data frame containing admin names etc.,.
#' @param levels The avaiable admin levels withnin the prep_geoname function
#' @param level The admins level being cleaned, i.ie level1 or even disrict.
#' @param clear_console Logical, whether to clear the console before showing
#'                  prompts; defaults to TRUE.
#' @param max_options Maximum number of options to display in the menu.
#'       Default is 200.
#' @param column_width Numeric; the maximum width (in characters) for each
#'       column in the interactive menu display. Default is 60.
#'
#' @return Data frame of user-selected replacements if any; otherwise,
#'          provides feedback based on user actions.
#'
#' @examples
#' # handle_user_interaction(my_data, "level1", TRUE, TRUE, 20)
#'
#' @keywords internal
#' @noRd
handle_user_interaction <- function(input_data, levels, level,
                                    clear_console = TRUE,
                                    max_options,
                                    column_width = 60,
                                    existing_counts = NULL,
                                    on_choice = NULL) {
  # Interactivity --------------------------------------------------------------

  # set up the messaging prompts at the start of the function
  prompts <- c(
    "What'll it be?:",
    "Your next move?:",
    "How shall we proceed?:",
    "Pick your path:",
    "Let's make a choice!:",
    "Forge ahead to:",
    "Plot your trajectory:",
    "Navigate your destiny:",
    "Select and soar!:",
    "Decisions, decisions!:",
    "Where to next?:"
  )
  prompt <- sample(prompts)[1]

  # filter out missing cachees
  input_data <- input_data |>
    dplyr::filter(
      !is.na(matched_names) & !is.na(name_to_match)
    )

  # set cachees for looping - use unique (name, context) pairs to avoid

  # cross-context contamination when same name appears in multiple hierarchies
  unique_contexts <- input_data |>
    dplyr::distinct(name_to_match, long_geo)

  # initialize empty lists to store user choices
  user_choices <- list()
  user_choice <- NULL
  save_and_exit <- FALSE

  # rows already using `name` in this context (parent names joined by "_")
  rows_in_data <- function(context, name) {
    if (is.null(existing_counts)) return(0L)
    n <- existing_counts[paste(context, toupper(name), sep = "\x1f")]
    ifelse(is.na(n), 0L, as.integer(n))
  }

  # NA = not a suspicious merge; TRUE/FALSE = the user's answer
  confirm_merge <- function(context, from, to) {
    n <- rows_in_data(context, to)
    if (!isTRUE(.is_suspicious_merge(from, to, n))) return(NA)
    cli::cli_alert_warning(paste0(
      "{.val {to}} already has {n} row{?s} here and does not look like ",
      "{.val {from}}. Choosing it merges the two into one."
    ))
    tolower(trimws(readline("Merge them? [y/n]: "))) == "y"
  }

  # loop through unmatched records in input_data
  # Initialize the index
  i <- 1
  while (i <= nrow(unique_contexts)) {
    # clear console
    if (clear_console) {
      .clear_console()
    }

    # Define color using crayon function
    red <- crayon::red
    bl <- crayon::blue
    gr <- crayon::green
    b <- crayon::bold
    p <- crayon::underline

    # set up choices -----------------------------------------------------------
    # select the cache to clean and suggested replacements
    name_to_clean <- unique_contexts$name_to_match[i]
    current_long_geo <- unique_contexts$long_geo[i]
    replacement_name <- input_data |>
      dplyr::filter(
        name_to_match == name_to_clean,
        long_geo == current_long_geo
      ) |>
      dplyr::distinct(matched_names) |>
      # narrow down to top max_options
      dplyr::slice(0:max_options) |>
      dplyr::pull() |>
      stringr::str_to_title()

    # get unique long names for this specific context
    unique_geo_long <- input_data |>
      dplyr::filter(
        name_to_match == name_to_clean,
        long_geo == current_long_geo
      ) |>
      dplyr::distinct()

    # set output title ---------------------------------------------------------

    # set up main header to keep track
    main_header <- glue::glue(
      "{stringr::str_to_title(level)} {i} of {nrow(unique_contexts)}"
    )

    if (!is.na(levels[2]) && level == levels[2]) {
      level_label <- "level1"
      long_geo <- unique_geo_long$long_geo[1]
      str_cache <- stringr::str_to_title(name_to_clean)
      str_long_geo <- stringr::str_to_title(long_geo)
      title <- glue::glue(
        "Which {level} name would you like to replace {b(red(str_cache))}",
        " within {bl(str_long_geo)}?"
      )
    } else if (!is.na(levels[3]) && level == levels[3]) {
      level_label <- "level2"
      long_geo <- unique_geo_long$long_geo[1]
      long_geo_country <- stringr::str_to_title(
        strsplit(long_geo, "_")[[1]][[1]]
      )
      long_geo_province <- stringr::str_to_title(
        strsplit(long_geo, "_")[[1]][[2]]
      )
      str_cache <- stringr::str_to_title(name_to_clean)
      title <- glue::glue(
        "Which {level} name would you like to replace {b(red(str_cache))}",
        " within the {gr(long_geo_province)} {levels[2]} ",
        "of {bl(long_geo_country)}?"
      )
    } else if (!is.na(levels[4]) && level == levels[4]) {
      level_label <- "level3"
      long_geo <- unique_geo_long$long_geo[1]
      long_geo_split <- strsplit(long_geo, "_")[[1]]
      long_geo_country <- stringr::str_to_title(long_geo_split[1])
      long_geo_province <- stringr::str_to_title(long_geo_split[2])
      long_geo_district <- stringr::str_to_title(long_geo_split[3])
      str_cache <- stringr::str_to_title(name_to_clean)
      title <- glue::glue(
        "Which {level} name would you like to replace {b(red(str_cache))}",
        " within the {gr(long_geo_district)} {levels[3]} of ",
        "{gr(long_geo_province)} {levels[2]} in {bl(long_geo_country)}?"
      )
    } else if (!is.na(levels[5]) && level == levels[5]) {
      level_label <- "level4"
      long_geo <- unique_geo_long$long_geo[1]
      long_geo_split <- strsplit(long_geo, "_")[[1]]
      long_geo_country <- stringr::str_to_title(long_geo_split[1])
      long_geo_province <- stringr::str_to_title(long_geo_split[2])
      long_geo_district <- stringr::str_to_title(long_geo_split[3])
      long_geo_ward <- stringr::str_to_title(long_geo_split[4])
      str_cache <- stringr::str_to_title(name_to_clean)
      title <- glue::glue(
        "Which {level} name would you like to replace {b(red(str_cache))}",
        " within the {gr(long_geo_ward)} {levels[4]} of ",
        "{gr(long_geo_district)} {levels[3]} of ",
        "{gr(long_geo_province)} {levels[2]} in {bl(long_geo_country)}?"
      )
    } else if (!is.na(levels[1]) && level == levels[1]) {
      level_label <- "level0"
      long_geo <- unique_geo_long$long_geo[1]
      str_cache <- stringr::str_to_title(name_to_clean)
      title <- glue::glue(
        "Which {level} name would you like to replace ",
        "{b(red(str_cache))} with?"
      )
    } else {
      long_geo <- unique_geo_long$long_geo[1]
      str_cache <- stringr::str_to_title(name_to_clean)
      title <- glue::glue(
        "Which {level} name would you like to replace ",
        "{b(red(str_cache))} with?"
      )
    }

    # action set up ------------------------------------------------------------
    special_actions <- list(
      "B" = "Go Back",
      "S" = "Skip this one",
      "E" = "Save and exit",
      "Q" = "Exit without saving",
      "M" = "Enter name manually"
    )

    # show how many rows already use each option here, so picking a place
    # that is already in the data (a merge) is visible
    context <- if (level == levels[1]) "" else current_long_geo
    option_rows <- vapply(
      replacement_name, function(nm) rows_in_data(context, nm), integer(1)
    )
    option_labels <- ifelse(
      option_rows > 0,
      paste0(replacement_name, " [", option_rows, " rows]"),
      replacement_name
    )

    # present the menu to the user ---------------------------------------------
    user_choice <- display_custom_menu(
      title, main_header,
      option_labels,
      special_actions,
      prompt = prompt,
      column_width = column_width
    )

    # handle user choices ------------------------------------------------------
    if (user_choice == "b") { # Go Back
      if (i > 1) {
        i <- i - 1
        next
      } else {
        cli::cli_alert_warning("You can't go back further.")
      }
    } else if (user_choice == "s") { # Skip this one
      cli::cli_alert_info("You are skipping this one...")
      i <- i + 1
      if (clear_console) .clear_console()
      next
    } else if (user_choice == "e") { # Save and exit
      if (length(user_choices) > 0) {
        cli::cli_alert_success("Choices saved successfully. Exiting...")
      } else {
        cli::cli_alert_warning("No choices to save.")
      }
      save_and_exit <- TRUE # tells prep_geonames() to stop after saving
      break # exit the loop entirely
    } else if (user_choice == "q") { # Exit without saving
      confirm_exit <- tolower(
        readline("Are you sure you want to exit without saving? [y/n]: ")
      )
      if (confirm_exit == "y") {
        cli::cli_alert_danger("You have exited without saving...")
        return(NULL)
      } else {
        cli::cli_alert_info("Returning to menu...")
      }
    } else if (user_choice == "m") { # Enter name manually
      manual_name <- readline(prompt = "Enter the name manually: ")
      replace_int <- .normalise_geoname(manual_name)
      merge_ok <- if (manual_name != "") {
        confirm_merge(context, name_to_clean, replace_int)
      } else {
        NA
      }
      if (isFALSE(merge_ok)) {
        cli::cli_alert_info("Not merged. Choose again.")
        next
      }
      if (manual_name != "") {
        user_choices[[length(user_choices) + 1]] <- data.frame(
          level = level_label,
          name_to_match = name_to_clean,
          replacement = replace_int,
          longname_to_match = ifelse(
            level != levels[1], paste0(long_geo, "_", name_to_clean),
            name_to_clean
          ),
          longname_corrected = ifelse(
            level != levels[1], paste0(long_geo, "_", replace_int), long_geo
          ),
          created_time = format(Sys.time(), tz = "UTC", usetz = TRUE),
          reviewed = if (isTRUE(merge_ok)) "yes" else NA_character_
        )
        if (is.function(on_choice)) on_choice(user_choices[[length(user_choices)]])
        cli::cli_alert_success("Manual name entered successfully.")
      } else {
        cli::cli_alert_warning("No name entered. Returning to menu...")
      }
      i <- i + 1
      if (clear_console) .clear_console()
    } else {
      replace_int <- toupper(replacement_name[suppressWarnings(as.integer(user_choice))])
      merge_ok <- confirm_merge(context, name_to_clean, replace_int)
      if (isFALSE(merge_ok)) {
        cli::cli_alert_info("Not merged. Choose again.")
        next
      }
      suppressWarnings({
        user_choices[[length(user_choices) + 1]] <- data.frame(
          level = level_label,
          name_to_match = name_to_clean,
          replacement = replace_int,
          longname_to_match = ifelse(
            level != levels[1], paste0(long_geo, "_", name_to_clean),
            name_to_clean
          ),
          longname_corrected = ifelse(
            level != levels[1], paste0(long_geo, "_", replace_int), long_geo
          ),
          created_time = format(Sys.time(), tz = "UTC", usetz = TRUE),
          reviewed = if (isTRUE(merge_ok)) "yes" else NA_character_
        )
      })
      if (is.function(on_choice)) on_choice(user_choices[[length(user_choices)]])
      i <- i + 1
      if (clear_console) .clear_console()
    }
  }

  # Aggregation user-chosen replacements into df -------------------------------

  # clear console
  if (clear_console) {
    .clear_console()
  }

  if (length(user_choices) != 0) {
    # Combine user choices into a single data frame
    user_choices_df <- dplyr::bind_rows(user_choices) |>
      # fix longname_corrected for country
      dplyr::mutate(
        longname_to_match = dplyr::if_else(
          level == "level0", name_to_match, longname_to_match
        ),
        longname_corrected = dplyr::if_else(
          level == "level0", replacement, longname_corrected
        )
      )
    # when the user went back and chose again, keep the latest choice
    user_choices_df <- user_choices_df[
      !duplicated(user_choices_df$longname_to_match, fromLast = TRUE), ,
      drop = FALSE
    ]

    cli::cli_alert_success(
      "Your selections have been successfully saved. Exiting..."
    )
    # return results
    attr(user_choices_df, "save_and_exit") <- save_and_exit
    user_choices_df
  } else {
    cli::cli_alert_warning(
      "No selections were made to save. Exiting..."
    )
    no_choices <- data.frame(
      level = NULL,
      name_to_match = NULL,
      replacement = NULL,
      longname_to_match = NULL,
      longname_corrected = NULL,
      created_time = NULL
    )
    attr(no_choices, "save_and_exit") <- save_and_exit
    no_choices
  }
}

#' Construct Long Geographic Names
#'
#' This function creates a composite geographic identifier by concatenating
#' values from specified administrative level columns within a dataframe.
#' @param data A dataframe containing the geographic data.
#' @param level0 level0 col name (country) in both 'data' and 'lookup_data'.
#' @param level1 level1 col name (province) in both 'data' and 'lookup_data'.
#' @param level2 level2 col name (district) in both 'data' and 'lookup_data'.
#' @param level3 level3 col name (subdistrict) in both 'data' and 'lookup_data'.
#' @param level4 level3 col name (settlements) in both 'data' and 'lookup_data'.
#'
#' @return Returns the dataframe with an additional column `long_geo` that
#'         contains the concatenated geographic identifiers.
#'
#' @examples
#' # Assuming `data` is a dataframe with columns 'country', 'state', 'city',
#' # and 'subdistrict':
#' # data <- data.frame(
#' # country = c("USA", "USA", "Canada"),
#' #  state = c("California", NA, "Ontario"),
#' #  city = c("Los Angeles", "New York", "Toronto"),
#' #  subdistrict = c("Downtown", "Manhattan", "Scarborough")
#' # )
#' # result <- construct_geo_names(data, "country", "state", "city",
#' # subdistrict")
#' @noRd
construct_geo_names <- function(data, level0, level1, level2,
                                level3 = NULL, level4 = NULL) {
  data |>
    dplyr::rowwise() |>
    dplyr::mutate(long_geo = {
      non_null_adms <- NULL
      if (!is.null(level0) && !is.na(.data[[level0]])) {
        non_null_adms <- c(non_null_adms, .data[[level0]])
      }
      if (!is.null(level1) && !is.na(.data[[level1]])) {
        non_null_adms <- c(non_null_adms, .data[[level1]])
      }
      if (!is.null(level2) && !is.na(.data[[level2]])) {
        non_null_adms <- c(non_null_adms, .data[[level2]])
      }
      if (!is.null(level3) && !is.na(.data[[level3]])) {
        non_null_adms <- c(non_null_adms, .data[[level3]])
      }
      if (!is.null(level4) && !is.na(.data[[level4]])) {
        non_null_adms <- c(non_null_adms, .data[[level4]])
      }
      paste(non_null_adms, collapse = "_")
    }) |>
    dplyr::ungroup()
}

#' Apply Case Mapping to Data Frame
#'
#' Internal helper function to apply original case from lookup data
#' to the matched values in the target data frame.
#'
#' @param df Data frame to apply case mapping to
#' @param case_mapping List of case mappings for each level
#' @param levels Vector of level column names
#'
#' @keywords internal
#' @noRd
apply_case_mapping <- function(df, case_mapping, levels) {
  if (is.null(case_mapping)) {
    return(df)
  }

  for (level in levels) {
    if (!is.null(level) && level %in% names(df) && level %in% names(case_mapping)) {
      mapping <- case_mapping[[level]]

      # Join with mapping to get original case
      df <- df |>
        dplyr::left_join(
          mapping,
          by = stats::setNames("uppercase", level)
        ) |>
        dplyr::mutate(
          !!rlang::sym(level) := dplyr::coalesce(original, .data[[level]])
        ) |>
        dplyr::select(-original)
    }
  }

  return(df)
}

#' Get User Identity
#'
#' Helper function to get the user identity from various environment variables
#'
#' @return Character string with the user identity
#' @keywords internal
#' @noRd
get_user_identity <- function() {
  if (nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) {
    Sys.getenv("RSTUDIO_USER_IDENTITY")
  } else if (nzchar(Sys.getenv("USER"))) {
    Sys.getenv("USER")
  } else if (nzchar(Sys.getenv("USERNAME"))) {
    Sys.getenv("USERNAME")
  } else {
    Sys.info()[["user"]]
  }
}

#' Export Unmatched Administrative Names
#'
#' Internal helper function to export complete rows with unmatched data
#' to a file with metadata, showing full hierarchical context.
#'
#' @param target_todo Data frame containing unmatched records
#' @param unmatched_export_path Path to save the unmatched data
#' @param level0 Name of level0 column
#' @param level1 Name of level1 column
#' @param level2 Name of level2 column
#' @param level3 Name of level3 column
#' @param level4 Name of level4 column
#'
#' @keywords internal
#' @noRd
export_unmatched_data <- function(target_todo, unmatched_export_path,
                                 level0, level1, level2, level3, level4) {

  if (nrow(target_todo) > 0) {
    # Get the source names from the tagged data
    target_name <- if ("target_data" %in% names(target_todo)) {
      unique(target_todo[["target_data"]])[1]
    } else {
      NA_character_
    }
    lookup_name <- if ("lookup_data" %in% names(target_todo)) {
      unique(target_todo[["lookup_data"]])[1]
    } else {
      NA_character_
    }

    # Build list of columns to keep - dynamic based on input
    cols_to_keep <- c()
    if (
      !is.null(level0) &&
        level0 %in% names(target_todo)
    ) {
      cols_to_keep <- c(cols_to_keep, level0)
    }
    if (
      !is.null(level1) &&
        level1 %in% names(target_todo)
    ) {
      cols_to_keep <- c(cols_to_keep, level1)
    }
    if (
      !is.null(level2) &&
        level2 %in% names(target_todo)
    ) {
      cols_to_keep <- c(cols_to_keep, level2)
    }
    if (
      !is.null(level3) &&
        level3 %in% names(target_todo)
    ) {
      cols_to_keep <- c(cols_to_keep, level3)
    }
    if (
      !is.null(level4) &&
        level4 %in% names(target_todo)
    ) {
      cols_to_keep <- c(cols_to_keep, level4)
    }

    # Identify the unmatched column (typically the lowest/most granular level)
    # Start from level4 (most granular) and work up
    unmatched_column <- NA
    if (!is.null(level4) && level4 %in% cols_to_keep) {
      unmatched_column <- level4  # e.g., "hf"
    } else if (!is.null(level3) && level3 %in% cols_to_keep) {
      unmatched_column <- level3  # e.g., "adm3"
    } else if (!is.null(level2) && level2 %in% cols_to_keep) {
      unmatched_column <- level2  # e.g., "adm2"
    } else if (!is.null(level1) && level1 %in% cols_to_keep) {
      unmatched_column <- level1  # e.g., "adm1"
    } else if (!is.null(level0) && level0 %in% cols_to_keep) {
      unmatched_column <- level0  # e.g., "adm0"
    }

    # Create the export dataframe with all context
    # Keep complete rows showing the hierarchical context
    unmatched_df <- target_todo |>
      dplyr::select(dplyr::all_of(cols_to_keep)) |>
      dplyr::distinct() |>
      dplyr::mutate(
        unmatched_column = unmatched_column,
        target_data = target_name,
        lookup_data = lookup_name,
        created_time = Sys.time(),
        name_of_creator = get_user_identity()
      ) |>
      # Reorganize columns - unmatched_column first, then admin levels,
      # then metadata
      dplyr::select(
        "unmatched_column",
        dplyr::all_of(cols_to_keep),
        "target_data",
        "lookup_data",
        "created_time",
        "name_of_creator"
      )

    # Save using save() function
    tryCatch({
      save(unmatched_df, unmatched_export_path)
      cli::cli_alert_success(
        "Unmatched data exported to: {.file {unmatched_export_path}}"
      )

      # Show summary of unmatched data
      # Extract the column name for display
      col_name <- if (!is.na(unmatched_column)) unmatched_column else "unknown"
      cli::cli_alert_info(
        "Exported {nrow(unmatched_df)} unmatched rows for column '{col_name}'"
      )
    }, error = function(e) {
      cli::cli_alert_warning(
        "Failed to export unmatched data: {e$message}"
      )
    })
  } else {
    cli::cli_alert_info("No unmatched values to export")
  }
}

#' Interactive Admin Name Cleaning and Matching
#'
#' This function streamlines the admin name cleaning process, leveraging both
#' algorithmic approaches and interactive user decisions. It combines string
#' distance algorithms for initial matching and offers user interactivity for
#' final decision-making, which are then saved for future reference and sharing.
#' Although the function does not require limiting name matching exclusively to
#' upper-level admins, optimal performance is achieved by confining to stricter
#' within-admin stratifications,
#' ensuring more accurate results. The function can also work with site names
#' or even any string matching that has lookup data.
#'
#' @param target_df Data frame containing the admin names to clean.
#' @param lookup_df Lookup data frame for verifying admin names. If this is not
#'                  provided, an internal version of WHO geoname data
#'                  attached to sntutils is used.
#' @param level0 level0 col name (country) in both 'data' and 'lookup_data'.
#' @param level1 level1 col name (province) in both 'data' and 'lookup_data'.
#' @param level2 level2 col name (district) in both 'data' and 'lookup_data'.
#' @param level3 level3 col name (subdistrict) in both 'data' and 'lookup_data'.
#' @param level4 level4 col name (settlement) in both 'data' and 'lookup_data'.
#' @param cache_path Optional; file holding the corrections you make in the
#'        interactive menu (.rds, .csv, .xlsx, ...). Every saved correction is
#'        applied automatically at the start of each run, before matching and
#'        before any questions, in interactive and non-interactive mode, and
#'        new corrections are added to it at the end of an interactive
#'        session. A correction is tied to where the name was found (e.g.
#'        MUNICIPAL AREA COUNCIL -> AMAC within FCT), so the same name can be
#'        corrected differently under different parents; if a name is
#'        corrected again, the newest correction wins. If NULL (the default),
#'        a per-user cache is used: `getOption("poliprep.geoname_cache")` if
#'        set, otherwise `geoname_cache.rds` in
#'        `tools::R_user_dir("poliprep", "data")`. Set the option to a shared
#'        file to let a team reuse each other's corrections.
#' @param unmatched_export_path Optional; path to save unmatched data after
#'        processing. The file will include complete rows with full hierarchical
#'        context, showing which column needs matching (typically the most
#'        granular level like health facilities) along with all administrative
#'        levels and metadata (timestamp, username, data sources). Supports all
#'        file formats supported by sntutils write() function based on the file
#'        extension.
#' @param method The string distance calculation method(s) to be used. Users
#'        can specify one or more algorithms from the
#'        \code{\link[stringdist]{stringdist}} package to compute
#'        string distances between admin names. The function by
#'        default uses \code{"jw"} (Jaro-Winkler). Other options include:
#'        \code{"lv"} (Levenshtein), \code{"dl"}
#'        (Damerau-Levenshtein), \code{"lcs"} (Longest Common Subsequence),
#'        \code{"qgram"} (Q-Gram), \code{"jw"} (Jaro-Winkler), and
#'        \code{"soundex"}.
#' @param interactive Logical; if TRUE, prompts the user for interactive
#'        matching decisions. Defaults to FALSE.
#' @param max_options Maximum number of options to output for string distance
#'        matching. Default is 200.
#' @param preserve_case Logical; if TRUE, preserves the original case of admin
#'        names from the lookup data when returning matched values. If FALSE
#'        (default), returns all admin names in uppercase as before.
#' @param column_width Numeric; the maximum width (in characters) for each
#'        column in the interactive menu. Controls how much text is displayed
#'        before truncation. Default is 60 characters. Note that the actual
#'        text display width is approximately 8 characters less to accommodate
#'        number labels and truncation markers ("...").
#' @param merge_check One of "review", "warn", "error" or "none". Guards against corrections that fold one place into
#'        another, e.g. ward AIGE SOLOMADE corrected to IPAKODO although IPAKODO
#'        already has rows of its own. A correction counts as a possible merge when
#'        the name it was changed to already appears in the data under the same
#'        parents and the two names look quite different, or are differently
#'        numbered places (GARKI I / GARKI II). Typo fixes are not affected. In the
#'        menu, each option shows how many rows already use it, and choosing such a
#'        name asks for confirmation. After every run the result is also checked
#'        (this catches merges coming from the cache):
#'        "review" (default) asks, in an interactive run, whether to keep each one
#'        (it is then marked as reviewed and not asked again) or undo it (the rows
#'        get their original name back and the cache entry is removed), and warns
#'        in a non-interactive run; "warn" always only warns; "error" stops with a
#'        list of the merges (useful in scheduled scripts); "none" turns the check
#'        off.
#'
#' @details
#' The function performs the following steps:
#' 1. Prepares the data by ensuring administrative names are in uppercase,
#'    with tidy spacing, for consistent matching.
#' 2. Applies every correction saved in the cache (see `cache_path`), so
#'    names corrected in earlier sessions are fixed before matching.
#' 3. Matches administrative names between `target_df` and `lookup_df` using
#'    string distance algorithms, running in parallel. Results are ranked
#'    by closeness.
#' 4. Engages the user through an interactive CLI menu to make decisions on
#'    ambiguous matches.
#' 5. Saves the user's decisions to the cache automatically (choose
#'    "Exit without saving" in the menu to discard them).
#' 6. Returns a cleaned data frame with updated administrative names based on
#'    user choices and algorithmic matching.
#'
#' @return A data frame with cleaned administrative names and saved data frame
#'        of user decisions.
#'
#' @examples
#' \donttest{
#' # Dummy target data
#' target_df <- data.frame(
#'   country = c("ANGOLA", "UGA", "ZAMBIA"),
#'   province = c("CABONDA", "TESO", "LUSAKA"),
#'   district = c("BALIZE", "BOKEDEA", "RAFUNSA"),
#'   subdistrict = c("AREA1", "AREA2", "AREA3")
#' )
#'
#' # Dummy lookup data with correct spellings
#' lookup_df <- data.frame(
#'   country = c("ANGOLA", "ANGOLA", "UGANDA", "UGANDA", "ZAMBIA", "ZAMBIA"),
#'   province = c("CABINDA", "CABINDA", "TESO", "TESO", "LUSAKA", "LUSAKA"),
#'   district = c("BELIZE", "BUCO-ZAU", "BUKEDEA", "KUMI", "KAFUE", "LUSAKA"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Interactively clean geonames
#' prep_geonames(
#'   target_df,
#'   lookup_df = lookup_df,
#'   level0 = "country", level1 = "province",
#'   level2 = "district",
#'   interactive = FALSE # replace with TRUE for interactivity
#' )
#' }
#'
#' @export
prep_geonames <- function(
  target_df,
  lookup_df = NULL,
  level0 = NULL,
  level1 = NULL,
  level2 = NULL,
  level3 = NULL,
  level4 = NULL,
  cache_path = NULL,
  unmatched_export_path = NULL,
  method = "jw",
  interactive = TRUE,
  max_options = 200,
  preserve_case = FALSE,
  column_width = 60,
  merge_check = c("review", "warn", "error", "none")
) {
  merge_check <- match.arg(merge_check)

  # Capture the names of the data frames at the beginning
  target_df_name <- paste(deparse(substitute(target_df)), collapse = "")
  lookup_df_name <- paste(deparse(substitute(lookup_df)), collapse = "")

  # Validation -----------------------------------------------------------------

  # Ensure higher levels cannot be used without corresponding lower levels
  if (!is.null(level1) && is.null(level0)) {
    cli::cli_abort("You cannot specify level1 without level0.")
  }
  if (!is.null(level2) && (is.null(level0) || is.null(level1))) {
    cli::cli_abort("You cannot specify level2 without both level0 and level1.")
  }
  if (
    !is.null(level3) && (is.null(level0) || is.null(level1) || is.null(level2))
  ) {
    cli::cli_abort(
      "You cannot specify level3 without level0, level1, and level2."
    )
  }

  # Prompt the user if using levels beyond level2 without a custom lookup_df
  if (is.null(lookup_df) && (!is.null(level3) || !is.null(level4))) {
    cli::cli_alert_warning(
      paste0(
        "The default lookup data only supports up to level2 (district). ",
        "Levels 3 and 4 will not have lookup matches unless a custom ",
        "lookup_df is provided."
      )
    )

    user_choice <- tolower(
      readline(
        paste0(
          "\nWould you like to:\n",
          "1. Continue with cleaning only up to level2",
          " (higher levels will be ignored)\n",
          "2. Exit and provide a custom lookup_df for level3 and level4\n",
          "Enter your choice (1 or 2): "
        )
      )
    )

    if (user_choice == "1") {
      cli::cli_alert_info(
        "Proceeding with cleaning up to level2 only. Ignoring level3 and level4."
      )
      level3 <- NULL
      level4 <- NULL
    } else {
      cli::cli_alert_info(
        paste0(
          "Exiting function. Please provide a valid ",
          "lookup_df to clean levels 3 and 4."
        )
      )
      return(invisible(NULL))
    }
  }

  # Ensure lookup_df contains necessary columns if provided
  if (!is.null(lookup_df)) {
    required_columns <- NULL
    if (!is.null(level0)) {
      required_columns <- c(required_columns, level0)
    }
    if (!is.null(level1)) {
      required_columns <- c(required_columns, level1)
    }
    if (!is.null(level2)) {
      required_columns <- c(required_columns, level2)
    }
    if (!is.null(level3)) {
      required_columns <- c(required_columns, level3)
    }
    if (!is.null(level4)) {
      required_columns <- c(required_columns, level4)
    }

    missing_columns <- setdiff(required_columns, colnames(lookup_df))
    if (length(missing_columns) > 0) {
      cli::cli_abort(
        paste(
          "The following columns are missing in lookup_df:",
          paste(missing_columns, collapse = ", ")
        )
      )
    }
  }

  # Ensure target_df contains necessary columns
  required_columns <- NULL
  if (!is.null(level0)) {
    required_columns <- c(required_columns, level0)
  }
  if (!is.null(level1)) {
    required_columns <- c(required_columns, level1)
  }
  if (!is.null(level2)) {
    required_columns <- c(required_columns, level2)
  }
  if (!is.null(level3)) {
    required_columns <- c(required_columns, level3)
  }
  if (!is.null(level4)) {
    required_columns <- c(required_columns, level4)
  }

  missing_columns <- setdiff(required_columns, colnames(target_df))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      paste(
        "The following columns are missing in target_df:",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  # Ensure method is supported
  supported_methods <- c(
    "jw",
    "osa",
    "lv",
    "dl",
    "hamming",
    "lcs",
    "qgram",
    "cosine",
    "jaccard",
    "soundex"
  )
  if (!(method %in% supported_methods)) {
    cli::cli_abort(
      paste(
        "Unsupported method:",
        method,
        ".
        Supported methods are:",
        paste(supported_methods, collapse = ", ")
      )
    )
  }

  # Ensure interactive is logical
  if (!is.logical(interactive)) {
    cli::cli_abort("interactive must be a logical value (TRUE or FALSE).")
  }

  # Ensure cache_path is a valid file path if provided
  if (!is.null(cache_path) && !dir.exists(dirname(cache_path))) {
    cli::cli_abort("The directory for cache_path does not exist.")
  }

  # Validation: Ensure lookup_df is not empty if provided
  if (!is.null(lookup_df) && nrow(lookup_df) == 0) {
    cli::cli_abort("The lookup_df is empty.")
  }

  # Ensure level0, level1, level2, and level3 are valid column names
  if (!is.null(level0) && !(level0 %in% colnames(target_df))) {
    cli::cli_abort(paste("The column", level0, "is not in target_df."))
  }
  if (!is.null(level1) && !(level1 %in% colnames(target_df))) {
    cli::cli_abort(paste("The column", level1, "is not in target_df."))
  }
  if (!is.null(level2) && !(level2 %in% colnames(target_df))) {
    cli::cli_abort(paste("The column", level2, "is not in target_df."))
  }
  if (!is.null(level3) && !(level3 %in% colnames(target_df))) {
    cli::cli_abort(paste("The column", level3, "is not in target_df."))
  }
  if (!is.null(level4) && !(level4 %in% colnames(target_df))) {
    cli::cli_abort(paste("The column", level4, "is not in target_df."))
  }

  # Step 0: Setup target and lookup datasets -----------------------------------

  # Get the internal shapefile if lookup data is not provided
  if (is.null(lookup_df)) {
    cli::cli_abort(
      c(
        "No lookup data provided.",
        "i" = "The internal WHO shapefile (shp_global) has been removed from this package.",
        "i" = "Please provide your own lookup data using the 'lookup_df' parameter.",
        "i" = "You can download WHO administrative boundaries from:",
        "i" = "https://hub.arcgis.com/datasets/WHO::polio-administrative-boundaries"
      )
    )
  }

  # Create the levels vector
  levels <- c(
    if (exists("level0")) level0 else NULL,
    if (exists("level1")) level1 else NULL,
    if (exists("level2")) level2 else NULL,
    if (exists("level3")) level3 else NULL,
    if (exists("level4")) level4 else NULL
  )

  # Store original case lookup values if preserve_case is TRUE
  lookup_case_mapping <- NULL
  if (preserve_case && !is.null(lookup_df)) {
    # Create a mapping from uppercase to original case for each level
    lookup_case_mapping <- list()

    for (level in levels) {
      if (level %in% names(lookup_df)) {
        # Get unique mappings from uppercase to original case
        original_values <- lookup_df[[level]]
        uppercase_values <- .normalise_geoname(original_values)

        # Create mapping dataframe (one original spelling per name, so the
        # join in apply_case_mapping() can't duplicate rows)
        mapping_df <- data.frame(
          uppercase = uppercase_values,
          original = original_values,
          stringsAsFactors = FALSE
        ) |>
          dplyr::distinct(uppercase, .keep_all = TRUE)

        lookup_case_mapping[[level]] <- mapping_df
      }
    }
  }

  # Ensure administrative names are uppercase, with tidy spacing, for matching
  target_df <- target_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(levels),
        .normalise_geoname
      )
    )

  lookup_df <- lookup_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(levels),
        .normalise_geoname
      )
    )

  # names as they arrived (tidied), to check the result against at the end
  input_df <- target_df

  # Step 1: Apply corrections saved in earlier sessions ------------------------
  #
  # Every name corrected in the interactive menu is saved to the cache. Before
  # anything is matched, all saved corrections are applied, so a name fixed
  # once (e.g. MUNICIPAL AREA COUNCIL -> AMAC in FCT) is fixed automatically in
  # every later run, interactive or not.

  using_default_cache <- is.null(cache_path)
  if (using_default_cache) {
    cache_path <- .default_geoname_cache_path()
  }

  # choices from a menu session that was stopped part-way go into the cache
  # first, so they are applied now and not asked again
  session_path <- .geoname_session_path(cache_path)
  if (file.exists(session_path)) {
    recovered <- tryCatch(readRDS(session_path), error = function(e) NULL)
    if (!is.null(recovered) && NROW(recovered) > 0) {
      tryCatch(
        {
          .save_geoname_cache(recovered, cache_path)
          unlink(session_path)
          cli::cli_alert_success(paste0(
            "Recovered {nrow(.dedupe_geoname_cache(recovered))} correction{?s} ",
            "from a menu session that was stopped before it finished."
          ))
        },
        error = function(e) {
          cli::cli_alert_danger(
            "Could not recover {.file {session_path}}: {conditionMessage(e)}"
          )
        }
      )
    } else {
      unlink(session_path)
    }
  }

  saved_cache_df <- .read_geoname_cache(cache_path)

  # caches saved by earlier versions without a cache_path landed in the
  # working directory; fold them in so those corrections aren't lost
  legacy_cache_path <- file.path(getwd(), "prepped_geoname_cache.rds")
  if (
    using_default_cache &&
      file.exists(legacy_cache_path) &&
      normalizePath(legacy_cache_path) != normalizePath(cache_path, mustWork = FALSE)
  ) {
    legacy_cache <- tryCatch(
      .read_geoname_cache(legacy_cache_path),
      error = function(e) NULL
    )
    if (!is.null(legacy_cache) && nrow(legacy_cache) > 0) {
      cli::cli_alert_info(
        "Also using {nrow(legacy_cache)} correction{?s} from {.file {legacy_cache_path}}."
      )
      saved_cache_df <- .dedupe_geoname_cache(
        dplyr::bind_rows(legacy_cache, saved_cache_df)
      )
    }
  }

  if (nrow(saved_cache_df) > 0) {
    applied <- .apply_geoname_cache(target_df, saved_cache_df, levels)
    target_df <- applied$data
    cli::cli_alert_success(
      paste0(
        "Applied {applied$names} saved correction{?s} to {applied$rows} ",
        "row{?s} from {.file {cache_path}}."
      )
    )
  } else {
    cli::cli_alert_info(
      paste0(
        "No saved corrections yet. Corrections you make will be saved to ",
        "{.file {cache_path}} and applied automatically next time."
      )
    )
  }

  # Step 2: Filter out for those where there is a match ------------------------

  # get the original data (with cache applied if it was loaded); the final
  # result is built from this, so rows keep their order and none are dropped
  orig_df <- target_df

  # Every exit goes through here: check for corrections that folded one place
  # into another (e.g. ward AIGE SOLOMADE -> IPAKODO), then restore case.
  finish <- function(out) {
    if (merge_check != "none") {
      cache_now <- tryCatch(
        .dedupe_geoname_cache(dplyr::bind_rows(
          saved_cache_df, .read_geoname_cache(cache_path)
        )),
        error = function(e) saved_cache_df
      )
      merges <- .find_geoname_merges(input_df, out, levels, cache_now)

      if (nrow(merges) > 0) {
        lines <- .describe_geoname_merges(merges)
        if (merge_check == "error") {
          cli::cli_abort(c(
            "{nrow(merges)} correction{?s} merge{?s/} one place into another:",
            stats::setNames(lines, rep("x", length(lines))),
            "i" = paste0(
              "Run interactively to keep or undo them, or remove them from ",
              "{.file {cache_path}}."
            )
          ))
        } else if (merge_check == "review" && interactive) {
          out <- .review_geoname_merges(
            merges, input_df, out, levels, cache_path
          )
        } else {
          warning(
            paste0(
              nrow(merges), " geoname correction(s) merge one place into ",
              "another; check them (run interactively to keep or undo):\n",
              paste0("  ", lines, collapse = "\n")
            ),
            call. = FALSE
          )
        }
      }
    }

    if (preserve_case) {
      out <- apply_case_mapping(out, lookup_case_mapping, levels)
    }
    out
  }

  # dynamically construct the long geonames on target data
  target_df <- construct_geo_names(
    target_df,
    level0,
    level1,
    level2,
    level3,
    level4
  )
  lookup_df <- construct_geo_names(
    lookup_df,
    level0,
    level1,
    level2,
    level3,
    level4
  )

  # filter to matched rows
  target_done <- target_df |>
    dplyr::filter(
      (long_geo %in%
        unique(lookup_df[["long_geo"]]))
    )

  # reduce down to only unmatched rows
  target_todo <- target_df |>
    dplyr::filter(
      !(long_geo %in% unique(lookup_df[["long_geo"]]))
    )

  calculate_match_stats(
    target_df,
    lookup_df,
    level0,
    level1,
    level2,
    level3,
    level4
  )

  # Early return with finalised_df
  if (nrow(target_todo) == 0) {
    cli::cli_alert_success(
      "All records matched; process completed. Exiting..."
    )

    return(finish(orig_df))
  }

  # return if non-interactive.
  if (!interactive) {
    cli::cli_alert_success(
      "In non-interactive mode. Exiting after matching with cache..."
    )

    # Export unmatched data before returning in non-interactive mode
    if (!is.null(unmatched_export_path) && nrow(target_todo) > 0) {
      # Tag the target_todo with source names
      target_todo_tagged <- target_todo |>
        dplyr::mutate(
          target_data = target_df_name,
          lookup_data = lookup_df_name
        )

      export_unmatched_data(
        target_todo_tagged,
        unmatched_export_path,
        level0,
        level1,
        level2,
        level3,
        level4
      )
    }

    return(finish(orig_df))
  }

  cli::cli_alert_info(
    "Partial match completed. There are still matches to be made."
  )

  user_input <-
    readline("Would you like to do interactive matching? (yes/no):")

  if (!(tolower(user_input) %in% c("yes", "y"))) {
    cli::cli_alert_info(
      "Exiting without interactive matching..."
    )

    return(finish(orig_df))
  }

  # Step 3: String distance matching in interactivity --------------------------

  # rank lookup names by closeness to each unmatched name
  rank_candidates <- function(names_to_match, candidates, context) {
    calculate_string_distance(names_to_match, candidates, method = method) |>
      dplyr::select(-match_rank, -algorithm_name) |>
      dplyr::group_by(name_to_match, matched_names) |>
      dplyr::slice_min(distance, with_ties = FALSE) |>
      dplyr::distinct(name_to_match, .keep_all = TRUE) |>
      dplyr::arrange(name_to_match, distance) |>
      dplyr::select(name_to_match, matched_names) |>
      dplyr::ungroup() |>
      dplyr::mutate(long_geo = context)
  }

  # "NIGERIA_FCT" style key of all levels above `upto`
  parent_key <- function(df, parent_levels) {
    do.call(
      paste,
      c(lapply(parent_levels, function(l) df[[l]]), sep = "_")
    )
  }

  new_cache_rows <- .standardise_geoname_cache(NULL)
  skipped_parents <- character()
  exit_without_saving <- FALSE

  for (k in seq_along(levels)) {
    level <- levels[[k]]

    if (k > 1) {
      # Compare names only within their full parent (country/state/...), so
      # e.g. NASARAWA LGA in Kano and NASARAWA LGA in Nasarawa state stay
      # separate and each gets the right list of wards.
      parent_levels <- levels[seq_len(k - 1)]
      todo_parent <- parent_key(target_todo, parent_levels)
      lookup_parent <- parent_key(lookup_df, parent_levels)

      unmatched <- !is.na(target_todo[[level]]) &
        !(paste(todo_parent, target_todo[[level]], sep = "\x1f") %in%
          paste(lookup_parent, lookup_df[[level]], sep = "\x1f"))

      top_res_list <- list()
      for (group in unique(todo_parent[unmatched])) {
        if (!(group %in% lookup_parent)) {
          # the parent itself isn't in the lookup (e.g. skipped earlier):
          # leave these names for now, carry on with the other groups
          skipped_parents <- c(skipped_parents, group)
          next
        }
        names_here <- unique(target_todo[[level]][unmatched & todo_parent == group])
        candidates <- unique(lookup_df[[level]][lookup_parent == group])
        top_res_list[[group]] <- rank_candidates(names_here, candidates, group)
      }
      top_res <- dplyr::bind_rows(top_res_list)
    } else {
      names_here <- unique(target_todo[[level]][
        !is.na(target_todo[[level]]) &
          !(target_todo[[level]] %in% lookup_df[[level]])
      ])
      top_res <- if (length(names_here) > 0) {
        rank_candidates(names_here, unique(lookup_df[[level]]), NA_character_) |>
          dplyr::mutate(long_geo = name_to_match)
      } else {
        NULL
      }
    }

    if (is.null(top_res) || nrow(top_res) == 0) {
      next
    }

    # how many rows already use each name at this level, by parent, with
    # this session's corrections applied (shown in the menu, and used to
    # ask before merging two places)
    current_all <- .apply_geoname_cache(orig_df, new_cache_rows, levels)$data
    current_parent <- if (k > 1) {
      parent_key(current_all, levels[seq_len(k - 1)])
    } else {
      rep("", nrow(current_all))
    }
    has_name <- !is.na(current_all[[level]])
    existing_counts <- table(paste(
      current_parent[has_name], current_all[[level]][has_name],
      sep = "\x1f"
    ))
    existing_counts <- stats::setNames(
      as.integer(existing_counts), names(existing_counts)
    )

    cli::cli_alert_info("Handling user interaction for level: {level}")
    replacement_df <- handle_user_interaction(
      input_data = top_res,
      levels = levels,
      level = level,
      max_options = max_options,
      column_width = column_width,
      existing_counts = existing_counts,
      # write each choice to the recovery file straight away
      on_choice = function(choice) {
        tryCatch(
          .append_geoname_session(.choices_to_cache(choice), session_path),
          error = function(e) NULL
        )
      }
    )

    # "Exit without saving": stop and keep none of this session's choices
    if (is.null(replacement_df)) {
      exit_without_saving <- TRUE
      break
    }

    if (nrow(replacement_df) > 0) {
      choice_rows <- .choices_to_cache(replacement_df)
      new_cache_rows <- dplyr::bind_rows(new_cache_rows, choice_rows)
      # apply straight away (matched on name AND parents) so the next level
      # is matched against the corrected parents
      target_todo <- .apply_geoname_cache(target_todo, choice_rows, levels)$data
      target_todo <- construct_geo_names(
        target_todo, level0, level1, level2, level3, level4
      )
      cli::cli_alert_success("Replacements made for level: {level}")
    } else {
      cli::cli_alert_warning("No replacements made for level: {level}")
    }

    # "Save and exit": stop here, keep what was chosen so far
    if (isTRUE(attr(replacement_df, "save_and_exit"))) {
      break
    }
  }

  if (length(skipped_parents) > 0) {
    skipped_parents <- unique(skipped_parents)
    cli::cli_alert_warning(
      paste0(
        "{length(skipped_parents)} group{?s} skipped because the higher ",
        "level isn't in the lookup: {.val {utils::head(skipped_parents, 10)}}",
        if (length(skipped_parents) > 10) " ..." else ""
      )
    )
  }

  # Step 4: save this session's corrections to the cache -----------------------

  if (exit_without_saving) {
    unlink(session_path)
    new_cache_rows <- .standardise_geoname_cache(NULL)
    cli::cli_alert_warning(
      "Exited without saving: this session's choices were not applied or saved."
    )
  } else if (nrow(new_cache_rows) > 0) {
    tryCatch(
      {
        # saved_cache_df also carries any corrections from an old
        # working-directory cache, so they move into the cache file now
        .save_geoname_cache(
          dplyr::bind_rows(saved_cache_df, new_cache_rows),
          cache_path
        )
        unlink(session_path)
        cli::cli_alert_success(
          paste0(
            "Saved {nrow(new_cache_rows)} correction{?s} to {.file {cache_path}}; ",
            "they will be applied automatically next time."
          )
        )
      },
      error = function(e) {
        cli::cli_alert_danger(paste0(
          "Could not save corrections to {.file {cache_path}}: {conditionMessage(e)}. ",
          "They are kept in {.file {session_path}} and will be added next run."
        ))
      }
    )
  } else {
    unlink(session_path)
    cli::cli_alert_warning(
      "No cleanings were made. Cache will not be updated."
    )
  }

  # Step 5: Apply this session's corrections to the full data ------------------

  finalised_df <- .apply_geoname_cache(orig_df, new_cache_rows, levels)$data

  # get stats
  calculate_match_stats(
    finalised_df,
    lookup_df,
    level0,
    level1,
    level2,
    level3,
    level4
  )

  # Step 6: Export unmatched data after all matching is complete ---------------
  # Export the final unmatched data (after interactive matching) if requested
  if (!is.null(unmatched_export_path) && nrow(target_todo) > 0) {
    # Tag the target_todo with source names
    target_todo_tagged <- target_todo <- target_todo |>
      # filter out the matched
      dplyr::filter(!(long_geo %in% unique(lookup_df$long_geo))) |>
      dplyr::mutate(
        target_data = target_df_name,
        lookup_data = lookup_df_name
      )

    export_unmatched_data(
      target_todo_tagged,
      unmatched_export_path,
      level0,
      level1,
      level2,
      level3,
      level4
    )
  }

  gc() # clean up memory

  # merge check, then case mapping; return the final data frame
  finish(finalised_df)
}

#' Impute higher administrative level using a lookup table
#'
#' This function imputes a higher-level administrative unit (e.g., adm1) based
#' on a lower-level administrative unit (e.g., adm2) using a lookup table.
#' It allows for dynamic column names and lookup table structures.
#'
#' @param target_df A dataframe containing the lower-level administrative unit
#'    column.
#' @param target_lower_col A string specifying the name of the lower-level admin
#'    column (e.g., adm2).
#' @param target_higher_col A string specifying the name of the higher-level
#'    admin column to be created.
#' @param lookup_df A dataframe containing the mapping of lower to higher-level
#'    administrative units.
#' @param lookup_lower_col A string specifying the name of the lower-level
#'    column in the lookup table.
#' @param lookup_higher_col A string specifying the name of the higher-level
#'    column in the lookup table.
#'
#' @return A dataframe with an additional higher-level column assigned based on
#'    the lookup.
#'
#' @examples
#' # Example lookup table
#' adm1_lookup <- dplyr::tribble(
#'   ~adm2, ~adm1,
#'   "Boffa", "Bok\u00e9",
#'   "Boke", "Bok\u00e9",
#'   "Fria", "Bok\u00e9"
#' )
#'
#' # Example dataset
#' data <- dplyr::tibble(
#'   district = c("Boffa", "Fria", "Unknown")
#' )
#'
#' # Impute higher-level administrative unit
#' cleaned_data <- impute_higher_admin(
#'   target_df = data,
#'   target_lower_col = "district",
#'   target_higher_col = "region",
#'   lookup_df = adm1_lookup,
#'   lookup_lower_col = "adm2",
#'   lookup_higher_col = "adm1"
#' )
#'
#' @export
impute_higher_admin <- function(target_df,
                                target_lower_col,
                                target_higher_col,
                                lookup_df,
                                lookup_lower_col,
                                lookup_higher_col) {
  target_df <- target_df |>
    dplyr::mutate(
      !!rlang::sym(target_higher_col) := dplyr::recode(
        .data[[target_lower_col]],
        !!!stats::setNames(
          lookup_df[[lookup_higher_col]],
          lookup_df[[lookup_lower_col]]
        )
      )
    )
  target_df
}
