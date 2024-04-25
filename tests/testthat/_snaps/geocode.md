# outputs correct messages when not using cache

    Code
      result <- tester(cache = FALSE)
    Message
      Proceeding to geocode without cache.
      * Writing input to csv.
      * Geocoding.
      * Reading geocode result to sf.

# outputs correct messages when using cache

    Code
      result <- tester(cache = "_test_cache")
    Message
      Geocoding with cache. Initializing cache at 'C:/Users/b14912846767/AppData/Local/R/cache/R/geocodepro/_test_cache'.
      * Writing input to csv.
      * Geocoding.
      * Reading geocode result to sf.
      * Formatting data to cache format.
      * Saving cache.

---

    Code
      result <- tester(cache = "_test_cache")
    Message
      Geocoding with cache found at 'C:/Users/b14912846767/AppData/Local/R/cache/R/geocodepro/_test_cache'.
      * Reading cache.
      * Merging cache with location data.
      * Creating `sf` from cached data. Found 1 address in cache.

---

    Code
      result <- tester(addresses, cache = "_test_cache")
    Message
      Geocoding with cache found at 'C:/Users/b14912846767/AppData/Local/R/cache/R/geocodepro/_test_cache'.
      * Reading cache.
      * Merging cache with location data.
      * Creating `sf` from cached data. Found 1 address in cache.
      * Geocoding 1 address missing from cache.
      * Writing input to csv.
      * Geocoding.
      * Reading geocode result to sf.
      * Appending geocode results to cache.
      * Formatting data to cache format.
      * Saving cache.

