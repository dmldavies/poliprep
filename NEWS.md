# poliprep 1.4.0

## prep_geonames(): saved corrections now apply automatically

- Corrections made in the interactive menu are saved automatically and
  applied at the start of every later run (interactive or not), before any
  questions. Without `cache_path`, a per-user cache is used
  (`options(poliprep.geoname_cache = ...)` to change it, e.g. to a shared
  file); a `prepped_geoname_cache.rds` left in the working directory by
  earlier versions is picked up and merged in.
- Corrections are tied to their parent areas, so the same ward name can be
  corrected differently in two LGAs. Previously the second one overwrote the
  first in the cache, and within a session the replacement was matched on the
  name alone, which duplicated rows and applied answers to the wrong LGA.
- Names are compared within their full parent (e.g. state + LGA), so LGAs
  that share a name across states (e.g. Nasarawa) are no longer mixed up.
- When a name is corrected again, the newest correction now wins.
- Spacing differences (double, leading/trailing or non-breaking spaces) no
  longer stop a saved correction from matching.
- A higher-level name missing from the lookup now skips only that group
  instead of ending the session for all remaining names.
- "Save and exit" now ends the session; "Exit without saving" discards the
  session's choices.
- The result keeps every input row in its original order.
- Caches saved as .csv/.xlsx combine correctly with new corrections, and a
  cache that can't be read is never overwritten.
- New merge check (`merge_check`) against corrections that fold one place
  into another (e.g. ward AIGE SOLOMADE -> IPAKODO when IPAKODO already has
  its own rows, or GARKI II -> GARKI I). The menu shows how many rows already
  use each option and asks before such a merge; after every run the result is
  checked, and suspicious corrections are offered for keep/undo (interactive),
  reported as a warning (batch) or, with `merge_check = "error"`, stop the
  run. Confirmed merges are marked `reviewed` in the cache and not raised
  again.

# poliprep 1.3.0

## Improvements

- Refactored `prep_geonames()` to warn users about unsupported levels in the default look-up file.

## Bug Fixes

- Fixed issue where `handle_file_save()` overwrote existing cache data.
- Resolved incorrect merging behavior in `prep_geonames()`.

## Deprecations

- Deprecated `get_multi_ona_data()` in favor of `get_ona_data()`

### Added
- **`init_folders()`**: A new function to create a standardized project folder structure.
  - Automatically generates directories for organizing data, scripts, and outputs.
  - Ensures consistency and reproducibility in data-related workflows.
  - Provides clear feedback messages using `cli` for successful and existing folders.

#### Example Usage
```r
# Create the project folder structure in the current working directory
init_folders()

# Specify a custom directory
init_folders(base_path = "~/my_project")
```
