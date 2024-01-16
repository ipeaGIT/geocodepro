assert_and_assign_cache <- function(cache) {
  checkmate::assert(
    checkmate::check_logical(cache, any.missing = FALSE, len = 1),
    checkmate::check_string(cache),
    combine = "or"
  )

  if (isTRUE(cache)) cache <- "_cache"

  return(cache)
}

assert_address_fields <- function(address_fields, locations) {
  checkmate::assert_character(address_fields, any.missing = FALSE)
  checkmate::assert_names(
    names(address_fields),
    type = "unique",
    subset.of = c(
      "Address_or_Place",
      "Address2",
      "Address3",
      "Neighborhood",
      "City",
      "County",
      "State",
      "ZIP",
      "ZIP4",
      "Country"
    )
  )
  checkmate::assert_names(address_fields, subset.of = names(locations))

  return(invisible(TRUE))
}
