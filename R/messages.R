inform_geocoding_without_cache <- function(verbose) {
  if (verbose) {
    cli::cli_inform(
      "Proceeding to geocode without cache.",
      class = "geocoding_without_cache",
      call = rlang::caller_env()
    )
  }
}

inform_writing_input_to_csv <- function(verbose) {
  if (verbose) {
    cli::cli_inform(
      c("*" = "Writing input to csv."),
      class = "writing_input_to_csv",
      call = rlang::caller_env()
    )
  }
}

inform_geocoding <- function(verbose) {
  if (verbose) {
    cli::cli_inform(
      c("*" = "Geocoding."),
      class = "geocoding",
      call = rlang::caller_env()
    )
  }
}

inform_reading_to_sf <- function(verbose) {
  if (verbose) {
    cli::cli_inform(
      c("*" = "Reading geocode result to sf."),
      class = "reading_to_sf",
      call = rlang::caller_env()
    )
  }
}

inform_geocoding_with_cache <- function(verbose, cache_path) {
  if (verbose) {
    cli::cli_inform(
      "Geocoding with cache found at {.path {cache_path}}.",
      class = "geocoding_with_cache",
      call = rlang::caller_env()
    )
  }
}

inform_initializing_cache <- function(verbose, cache_path) {
  if (verbose) {
    cli::cli_inform(
      "Geocoding with cache. Initializing cache at {.path {cache_path}}.",
      class = "initializing_cache",
      call = rlang::caller_env()
    )
  }
}

inform_reading_cache <- function(verbose) {
  if (verbose) {
    cli::cli_inform(
      c("*" = "Reading cache."),
      class = "reading_cache",
      call = rlang::caller_env()
    )
  }
}

inform_merging_cache <- function(verbose) {
  if (verbose) {
    cli::cli_inform(
      c("*" = "Merging cache with location data."),
      class = "merging_cache",
      call = rlang::caller_env()
    )
  }
}

inform_creating_sf_from_cache <- function(verbose, n_rows) {
  if (verbose) {
    cli::cli_inform(
      c(
        "*" = paste0(
          "Creating {.code sf} from cached data. ",
          "Found {.val {n_rows}} address{?es} in cache."
        )
      ),
      class = "creating_sf_from_cache",
      call = rlang::caller_env()
    )
  }
}

inform_geocoding_uncached <- function(verbose, n_rows) {
  if (verbose) {
    cli::cli_inform(
      c("*" = "Geocoding {.val {n_rows}} address{?es} missing from cache."),
      class = "geocoding_uncached",
      call = rlang::caller_env()
    )
  }
}

inform_appending_to_cache <- function(verbose) {
  if (verbose) {
    cli::cli_inform(
      c("*" = "Appending geocode results to cache."),
      class = "appending_to_cache",
      call = rlang::caller_env()
    )
  }
}

inform_saving_cache <- function(verbose) {
  if (verbose) {
    cli::cli_inform(
      c("*" = "Saving cache."),
      class = "saving_cache",
      call = rlang::caller_env()
    )
  }
}

inform_formatting_data <- function(verbose) {
  if (verbose) {
    cli::cli_inform(
      c("*" = "Formatting data to cache format."),
      class = "formatting_data",
      call = rlang::caller_env()
    )
  }
}
