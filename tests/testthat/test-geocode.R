reticulate::use_condaenv(
  "C://Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3"
)

address <- data.frame(
  logradouro = "Avenida Venceslau Brás, 72",
  bairro = "Botafogo",
  cidade = "Rio de Janeiro",
  uf = "RJ",
  cep = "22290-140"
)

tester <- function(locations = address,
                   locator = "C://StreetMap/NewLocators/BRA/BRA.loc",
                   address_fields = address_fields_const(
                     Address_or_Place = "logradouro",
                     Neighborhood = "bairro",
                     City = "cidade",
                     State = "uf",
                     ZIP = "cep"
                   ),
                   location_type = "ROUTING_LOCATION",
                   output_fields = "MINIMAL",
                   cache = TRUE,
                   verbose = TRUE) {
  geocode(
    locations = locations,
    locator = locator,
    address_fields = address_fields,
    location_type = location_type,
    output_fields = output_fields,
    cache = cache,
    verbose = verbose
  )
}

# tests

test_that("fails with wrong input", {
  expect_error(tester("oi"))

  expect_error(tester(locator = "oi.loc"))

  expect_error(tester(address_fields = c("logradouro", NA)))
  expect_error(tester(address_fields = "logradouro"))
  expect_error(tester(address_fields = c(oie = "logradouro")))
  expect_error(tester(address_fields = c(Address_or_Place = "oie")))
  expect_error(
    tester(address_fields = address_fields_const()),
    class = "address_fields_must_not_be_null"
  )

  expect_error(tester(location_type = 1))
  expect_error(tester(location_type = c("oie", "oie")))

  expect_error(tester(output_fields = 1))
  expect_error(tester(output_fields = c("oie", "oie")))

  expect_error(tester(cache = 1))
  expect_error(tester(cache = NA))
  expect_error(tester(cache = c(TRUE, TRUE)))

  expect_error(tester(verbose = 1))
  expect_error(tester(verbose = NA))
  expect_error(tester(verbose = c(TRUE, TRUE)))
})

test_that("outputs correct messages when not using cache", {
  expect_snapshot(result <- tester(cache = FALSE))
})

test_that("outputs correct messages when using cache", {
  cache_name <- "_test_cache"
  cache_path <- file.path(
    tools::R_user_dir("geocodepro", "cache"),
    paste0(cache_name, ".rds")
  )
  expect_false(file.exists(cache_path))
  on.exit(
    expect_true(file.remove(cache_path)),
    add = TRUE
  )

  # initializing cache
  expect_snapshot(result <- tester(cache = "_test_cache"))

  # all locations cached
  expect_snapshot(result <- tester(cache = "_test_cache"))

  additional_address <- data.frame(
    logradouro = "Avenida Venceslau Brás, 71",
    bairro = "Botafogo",
    cidade = "Rio de Janeiro",
    uf = "RJ",
    cep = "22290-140"
  )
  addresses <- rbind(address, additional_address)

  # using existing cache
  expect_snapshot(result <- tester(addresses, cache = "_test_cache"))
})
