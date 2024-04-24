#' Geolocalização
#'
#' Geolocaliza um dataframe de endereços com base em diferentes atributos.
#' Documentação da ferramenta:
#' https://pro.arcgis.com/en/pro-app/latest/tool-reference/geocoding/geocode-addresses.htm.
#'
#' @param locations Um dataframe. Os endereços que devem ser geolocalizados.
#'   Deve conter todas as colunas especificadas em `address_fields`.
#' @param locator Uma string. O caminho até o localizador que deve ser
#'   utilizado. Por padrão, utiliza o localizador incluído no ArcGIS Pro para o
#'   Brasil.
#' @param address_fields Um vetor de caracteres. A correspondência entre as
#'   colunas do dataframe e os campos de geolocalização utilizados pelo ArcGIS
#'   Pro, em que os nomes do vetor são os campos esperados pela ferramenta e os
#'   valores são as colunas correspondentes. A função `address_fields_const()`
#'   serve para facilitar a criação deste vetor, fazendo também algumas
#'   verificações em seu conteúdo. Note que itens com o valor `NULL` são
#'   ignorados, e que a função deve receber ao menos um valor diferente de nulo.
#'   Caso esteja criando o vetor manualmente, note que os nomes dos elementos
#'   devem ser os mesmos nomes dos parâmetros da função
#'   `address_fields_const()`.
#' @param location_type Uma string. Interfere na localização final de cada
#'   endereço geolocalizado. As opções são `"ROUTING_LOCATION"` (padrão), em que
#'   os endereços são posicionados ao longo da rua, e é considerado mais
#'   apropriado para roteamento, ou `"ADDRESS_LOCATION"`, em que endereços são
#'   localizados em uma posição equivalente ao "terraço" do endereço (no
#'   centroide do polígono correspondente).
#' @param output_fields Uma string. As colunas que serão retornadas no output.
#'   Possíveis opções são `"MINIMAL"` (padrão), que inclui as colunas do
#'   dataframe de input e outros campos que descrevem o quão boa foi a
#'   correspondência entre a geolocalização e o endereço, `"ALL"`, que inclui
#'   todas as opções disponibilizadas pelo localizador, e `"LOCATION_ONLY"`, que
#'   inclui apenas a geolocalização do endereço e as colunas do input.
#' @param cache Um logical (por padrão, `TRUE`) ou uma string. Se `TRUE`, é
#'   utilizado um cache "geral" tanto para buscar localizações que já tenham
#'   sido geolocalizadas antes quanto para adicionar quaisquer pontos que tenham
#'   sido geolocalizados na operação corrente. Se `FALSE`, nenhum cache é usado.
#'   Caso seja uma string, seu valor é utilizado para identificar um cache
#'   específico. Por exemplo, pode ser útil usar um cache apenas para endereços
#'   da RAIS, caso em que temos que passar `"rais"`. Caso desejemos invalidar
#'   este cache e começar um novo cache da RAIS do zero, podemos passar
#'   `"rais_v2"`.
#' @param verbose Um logical. Se mensagens informativas devem ser printadas
#'   (padrão) ou não.
#'
#' @return Um objeto `sf` do tipo `POINT` com a localização dos endereços
#'   especificados.
#'
#' @examples
#' reticulate::use_condaenv(
#'   "C://Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3"
#' )
#'
#' locations <- data.frame(
#'   logradouro = "Avenida Venceslau Brás, 72",
#'   bairro = "Botafogo",
#'   cidade = "Rio de Janeiro",
#'   uf = "RJ",
#'   cep = "22290-140"
#' )
#'
#' result <- geocode(
#'   locations,
#'   address_fields = address_fields_const(
#'     Address_or_Place = "logradouro",
#'     Neighborhood = "bairro",
#'     City = "cidade",
#'     State = "uf",
#'     ZIP = "cep"
#'   ),
#'   cache = "example_cache"
#' )
#'
#' @export
geocode <- function(locations,
                    locator = "C://StreetMap/NewLocators/BRA/BRA.loc",
                    address_fields = address_fields_const(),
                    location_type = "ROUTING_LOCATION",
                    output_fields = "MINIMAL",
                    cache = TRUE,
                    verbose = TRUE) {
  checkmate::assert_data_frame(locations)
  checkmate::assert_file_exists(locator, extension = ".loc")
  checkmate::assert_string(location_type)
  checkmate::assert_string(output_fields)
  checkmate::assert_logical(verbose, any.missing = FALSE, len = 1)
  assert_address_fields(address_fields, locations)
  cache <- assert_and_assign_cache(cache)

  geocoded_data <- geocode_addresses(
    locations,
    locator = locator,
    address_fields = address_fields,
    location_type = location_type,
    output_fields = output_fields,
    cache = cache,
    verbose = verbose
  )

  return(geocoded_data)
}

geocode_addresses <- function(locations,
                              locator,
                              address_fields,
                              location_type,
                              output_fields,
                              cache,
                              verbose) {
  if (isFALSE(cache)) {
    inform_geocoding_without_cache(verbose)

    output_file <- do_geocode(
      locations,
      locator = locator,
      address_fields = address_fields,
      location_type = location_type,
      output_fields = output_fields,
      verbose = verbose
    )

    geocoded_data <- read_geocoded_data_from_path(
      output_file,
      address_fields,
      verbose = verbose
    )

    return(geocoded_data)
  }

  # para usar o cache, precisamos:
  #  1) checar se o cache existe.
  #  2) caso exista, fazemos um match entre os endereços a serem localizados e o
  #     cache.
  #  3) chamamos a função de geolocalização apenas para os endereços não
  #     presentes no cache (se o cache não existia previamente, enviamos todos
  #     os endereços)
  #  4) ao final, adicionamos os endereços recém geolocalizados ao cache

  cache_dir <- tools::R_user_dir("geocodepro", which = "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  cache_path <- fs::path_norm(file.path(cache_dir, paste0(cache, ".csv")))

  if (file.exists(cache_path)) {
    inform_geocoding_with_cache(verbose, cache_path)

    geocoded_data <- geocode_with_previous_cache(
      locations,
      locator = locator,
      address_fields = address_fields,
      location_type = location_type,
      output_fields = output_fields,
      cache_path = cache_path,
      verbose = verbose
    )
  } else {
    inform_initializing_cache(verbose, cache_path)

    geocoded_data <- geocode_without_previous_cache(
      locations,
      locator = locator,
      address_fields = address_fields,
      location_type = location_type,
      output_fields = output_fields,
      cache_path = cache_path,
      verbose = verbose
    )
  }

  return(geocoded_data)
}

glbl_all_address_fields <- c(
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

geocode_with_previous_cache <- function(locations,
                                        locator,
                                        address_fields,
                                        location_type,
                                        output_fields,
                                        cache_path,
                                        verbose) {
  cache_data <- read_cache_data(cache_path, address_fields, verbose)

  inform_merging_cache(verbose)

  lookup_expression <- build_lookup_expression()

  locations_with_data <- format_location_data(locations, address_fields)
  locations_with_data[
    cache_data,
    on = glbl_all_address_fields,
    eval(lookup_expression)
  ]

  cached_data <- locations_with_data[!is.na(Score)]
  cached_data <- format_cached_addresses(
    cached_data,
    address_fields = address_fields,
    locations_names = names(locations),
    verbose = verbose
  )

  uncached_locations <- locations_with_data[is.na(Score)]
  new_geocoded_data <- geocode_and_append_to_cache(
    uncached_locations,
    cache_data = cache_data,
    cache_path = cache_path,
    locator = locator,
    address_fields = address_fields,
    location_type = location_type,
    output_fields = output_fields,
    verbose = verbose
  )

  geocoded_data <- rbind(
    cached_data,
    new_geocoded_data
  )

  return(geocoded_data)
}

format_location_data <- function(locations, address_fields) {
  formatted_locations <- data.table::as.data.table(locations)

  data.table::setnames(
    formatted_locations,
    old = address_fields,
    new = names(address_fields)
  )

  missing_fields <- setdiff(glbl_all_address_fields, names(formatted_locations))
  formatted_locations[, (missing_fields) := NA_character_]

  formatted_locations <- data.table::setkeyv(
    formatted_locations,
    glbl_all_address_fields
  )

  return(formatted_locations)
}

read_cache_data <- function(cache_path, address_fields, verbose) {
  inform_reading_cache(verbose)

  sample_row <- data.table::fread(cache_path, na.strings = "", nrows = 1)
  numeric_cols <- c("Score", "Lon", "Lat")
  char_cols <- setdiff(names(sample_row), numeric_cols)

  cache_data <- data.table::fread(
    cache_path,
    na.strings = "",
    select = list(numeric = numeric_cols, character = char_cols)
  )
  data.table::setkeyv(cache_data, glbl_all_address_fields)

  return(cache_data)
}

build_lookup_expression <- function() {
  geocode_cols <- c(
    "Status",
    "Score",
    "Match_type",
    "Match_addr",
    "Addr_type",
    "Lon",
    "Lat"
  )

  expr_text <- paste0(
    "`:=`(",
    paste0(geocode_cols, " = i.", geocode_cols, collapse = ", "),
    ")"
  )
  expr <- parse(text = expr_text)

  return(expr)
}

format_cached_addresses <- function(cached_data,
                                    address_fields,
                                    locations_names,
                                    verbose) {
  inform_creating_sf_from_cache(verbose, nrow(cached_data))

  filled_fields <- setdiff(glbl_all_address_fields, names(address_fields))
  cached_data[, (filled_fields) := NULL]

  data.table::setnames(
    cached_data,
    old = names(address_fields),
    new = address_fields
  )
  data.table::setkey(cached_data, NULL)

  data.table::setcolorder(
    cached_data,
    c(setdiff(names(cached_data), locations_names), locations_names)
  )

  # the latlon columns may be empty if the address is unmatched. in such cases,
  # st_as_sf() would fail, so we use sfheaders::sf_points() to deal with NAs

  if (any(is.na(cached_data$Lon))) {
    non_na_coords_data <- sf::st_as_sf(
      cached_data[!is.na(Lon)],
      coords = c("Lon", "Lat"),
      crs = 4326
    )

    na_coords_data <- sfheaders::sf_point(
      cached_data[is.na(Lon)],
      x = "Lon",
      y = "Lat",
      keep = TRUE
    )
    na_coords_data <- sf::st_set_crs(na_coords_data, 4326)

    cached_data <- rbind(non_na_coords_data, na_coords_data)
  } else {
    # raises a warning if cached_data is empty, but we don't need to worry about
    # this because this sf is later bound to another with data and the correct
    # bounding box is then calculated

    suppressWarnings(
      cached_data <- sf::st_as_sf(
        cached_data,
        coords = c("Lon", "Lat"),
        crs = 4326
      )
    )
  }

  cached_data <- dplyr::rename(cached_data, geom = geometry)

  return(cached_data)
}

geocode_and_append_to_cache <- function(uncached_locations,
                                        cache_data,
                                        cache_path,
                                        locator,
                                        address_fields,
                                        location_type,
                                        output_fields,
                                        verbose) {
  if (nrow(uncached_locations) == 0) {
    return(empty_geocoded_data(address_fields))
  }

  inform_geocoding_uncached(verbose, nrow(uncached_locations))

  geocode_cols <- c(
    "Status",
    "Score",
    "Match_type",
    "Match_addr",
    "Addr_type",
    "Lon",
    "Lat"
  )

  uncached_locations[, (geocode_cols) := NULL]

  filled_fields <- setdiff(glbl_all_address_fields, names(address_fields))
  uncached_locations[, (filled_fields) := NULL]

  data.table::setnames(
    uncached_locations,
    old = names(address_fields),
    new = address_fields
  )
  data.table::setkey(uncached_locations, NULL)

  new_geocoded_data_path <- do_geocode(
    uncached_locations,
    locator = locator,
    address_fields = address_fields,
    location_type = location_type,
    output_fields = output_fields,
    verbose = verbose
  )

  new_geocoded_data <- read_geocoded_data_from_path(
    new_geocoded_data_path,
    address_fields,
    verbose = verbose
  )

  append_to_cache(
    new_geocoded_data,
    cache_data = cache_data,
    cache_path = cache_path,
    address_fields = address_fields,
    location_type = location_type,
    verbose = verbose
  )

  return(new_geocoded_data)
}

empty_geocoded_data <- function(address_fields) {
  relevant_cols <- c(
    address_fields,
    "Status",
    "Score",
    "Match_type",
    "Match_addr",
    "Addr_type",
    "Lon",
    "Lat"
  )
  empty_list <- lapply(1:length(relevant_cols), function(i) character(0))
  empty_table <- data.table::setDT(empty_list)
  names(empty_table) <- relevant_cols
  empty_table[, c("Score", "Lon", "Lat") := numeric(0)]

  # st_as_sf() raises warnings when trying to calculate the bounding box of this
  # object, since its empty.
  # warnings in the format:
  #   - In min(cc[[1]], na.rm = TRUE) : no non-missing arguments to min;
  #     returning Inf

  suppressWarnings(
    empty_table <- sf::st_as_sf(
      empty_table,
      coords = c("Lon", "Lat"),
      crs = 4326,
      sf_column_name = "geom"
    )
  )

  return(empty_table)
}

append_to_cache <- function(new_geocoded_data,
                            cache_data,
                            cache_path,
                            address_fields,
                            location_type,
                            verbose) {
  inform_appending_to_cache(verbose)

  new_data_in_cache_structure <- to_cache_structure(
    new_geocoded_data,
    address_fields = address_fields,
    location_type = location_type,
    lookup_only = FALSE,
    verbose = verbose
  )
  new_data_in_cache_structure <- unique(new_data_in_cache_structure)

  cache_data <- rbind(cache_data, new_data_in_cache_structure)

  inform_saving_cache(verbose)
  data.table::fwrite(cache_data, cache_path)

  return(invisible(cache_path))
}

geocode_without_previous_cache <- function(locations,
                                           locator,
                                           address_fields,
                                           location_type,
                                           output_fields,
                                           cache_path,
                                           verbose) {
  geocoded_path <- do_geocode(
    locations,
    locator = locator,
    address_fields = address_fields,
    location_type = location_type,
    output_fields = output_fields,
    verbose = verbose
  )

  geocoded_data <- read_geocoded_data_from_path(
    geocoded_path,
    address_fields,
    verbose
  )

  create_geocode_cache(
    geocoded_data,
    address_fields = address_fields,
    location_type = location_type,
    cache_path = cache_path,
    verbose = verbose
  )

  return(geocoded_data)
}

do_geocode <- function(locations,
                       locator,
                       address_fields,
                       location_type,
                       output_fields,
                       verbose) {
  inform_writing_input_to_csv(verbose)
  tmpfile_input <- tempfile("geocode_input", fileext = ".csv")
  data.table::fwrite(locations, tmpfile_input)

  tmpfile_output <- tempfile("geocode_output", fileext = ".gdb")
  tmpfile_basename <- basename(tmpfile_output)
  create_gdb_file(tmpfile_basename)

  address_fields_string <- fields_to_string(address_fields)

  # a função retorna uma string listando o enderço de output, mas com uma
  # formatação extra e superficial. a variável abaixo não é usada pra nada, só
  # pra "silenciar" o output da função. estamos realmente interessados no
  # "side-effect" da função abaixo, que é geolocalizar os endereços e escrever
  # o output na camada geocode_result do geodatabase de output

  inform_geocoding(verbose)

  geocode_fn_output <- arcpy$geocoding$GeocodeAddresses(
    in_table = tmpfile_input,
    address_locator = normalizePath(locator),
    in_address_fields = address_fields_string,
    out_feature_class = file.path(tmpfile_output, "geocode_result"),
    out_relationship_type = "STATIC",
    country = "BRA",
    location_type = location_type,
    output_fields = output_fields
  )

  return(tmpfile_output)
}

create_gdb_file <- function(tmpfile_basename) {
  # por algum motivo a chamada da função do arcpy muda o locale da sessão, o que
  # impacta em como as strings e os inputs são processados. essa função,
  # portanto, transforma o locale de volta pro default do sistema ao final de
  # sua chamada
  on.exit(Sys.setlocale(locale = ""), add = TRUE)
  tmpfile_creation_result <- arcpy$CreateFileGDB_management(
    out_folder_path = tempdir(),
    out_name = tmpfile_basename
  )

  return(invisible(TRUE))
}

fields_to_string <- function(address_fields) {
  field_names <- c(
    "'Address or Place'",
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

  address_or_place_index <- grep("Address_or_Place", names(address_fields))

  if (!identical(address_or_place_index, integer(0))) {
    names(address_fields)[address_or_place_index] <- "'Address or Place'"
  }

  missing_fields <- setdiff(field_names, names(address_fields))

  fields_to_fill <- rep("<None>", length(missing_fields))
  names(fields_to_fill) <- missing_fields

  filled_fields <- c(address_fields, fields_to_fill)

  fields_string <- paste(
    paste(
      names(filled_fields),
      filled_fields,
      "VISIBLE NONE"
    ),
    collapse = "; "
  )

  return(fields_string)
}

read_geocoded_data_from_path <- function(output_file, address_fields, verbose) {
  inform_reading_to_sf(verbose)

  geocoded_data <- sf::st_read(
    output_file,
    layer = "geocode_result",
    quiet = TRUE
  )

  # a coluna de cep, se presente e se não contiver caracteres como '-', é
  # convertida para int ao usar o st_read(). alguns ceps podem começar com 0, o
  # que acaba fazendo com que ceps convertidos tenham um caractere a menos. por
  # isso nós precisamos converter essa coluna para char e fazer um padding com
  # 0s à esquerda para 8 dígitos

  if ("ZIP" %in% names(address_fields)) {
    zip_values <- geocoded_data[[address_fields["ZIP"]]]

    if (is.numeric(zip_values)) {
      geocoded_data[[address_fields["ZIP"]]] <- formatC(
        zip_values,
        width = 8,
        flag = "0",
        format = "d"
      )
    }
  }

  geocoded_data <- dplyr::rename(geocoded_data, geom = Shape)

  return(geocoded_data)
}

create_geocode_cache <- function(geocoded_data,
                                 address_fields,
                                 location_type,
                                 cache_path,
                                 verbose) {
  data_in_cache_structure <- to_cache_structure(
    geocoded_data,
    address_fields = address_fields,
    location_type = location_type,
    lookup_only = FALSE,
    verbose = verbose
  )
  data_in_cache_structure <- unique(data_in_cache_structure)

  inform_saving_cache(verbose)
  data.table::fwrite(data_in_cache_structure, cache_path)

  return(invisible(cache_path))
}

to_cache_structure <- function(geocoded_data,
                               address_fields,
                               location_type,
                               lookup_only,
                               verbose) {
  inform_formatting_data(verbose)

  arcpy_params <- "location_type"
  all_address_fields <- c(
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
  geocode_cols <- c(
    "Status",
    "Score",
    "Match_type",
    "Match_addr",
    "Addr_type",
    "Lon",
    "Lat"
  )

  structure_fields <- if (lookup_only) {
    c(arcpy_params, all_address_fields)
  } else {
    c(arcpy_params, all_address_fields, geocode_cols)
  }

  empty_list <- lapply(1:length(structure_fields), function(i) character(0))
  empty_table <- data.table::setDT(empty_list)
  names(empty_table) <- structure_fields

  if (!lookup_only) empty_table[, c("Score", "Lon", "Lat") := numeric(0)]

  cache_data <- data.table::as.data.table(geocoded_data)
  cache_data <- point_to_coords(geocoded_data)
  cache_data <- data.table::setnames(
    cache_data,
    old = address_fields,
    new = names(address_fields)
  )
  cache_data[
    ,
    setdiff(names(cache_data), c(all_address_fields, geocode_cols)) := NULL
  ]

  env <- environment()
  cache_data <- rbind(empty_table, cache_data, fill = TRUE)
  cache_data[, location_type := get("location_type", envir = env)]

  return(cache_data[])
}

point_to_coords <- function(geocoded_data) {
  coords <- data.table::as.data.table(sf::st_coordinates(geocoded_data))
  names(coords) <- c("Lon", "Lat")

  geocoded_data <- sf::st_drop_geometry(geocoded_data)
  geocoded_data <- data.table::setDT(cbind(geocoded_data, coords))

  return(geocoded_data)
}
