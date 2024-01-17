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
#'   )
#' )
#'
#' @export
geocode <- function(locations,
                    locator = "C://StreetMap/NewLocators/BRA/BRA.loc",
                    address_fields = address_fields_const(),
                    location_type = "ROUTING_LOCATION",
                    output_fields = "MINIMAL",
                    cache = TRUE) {
  checkmate::assert_data_frame(locations)
  checkmate::assert_file_exists(locator, extension = ".loc")
  checkmate::assert_string(location_type)
  checkmate::assert_string(output_fields)
  assert_address_fields(address_fields, locations)
  cache <- assert_and_assign_cache(cache)

  geocoded_data <- geocode_addresses(
    locations,
    locator = locator,
    address_fields = address_fields,
    location_type = location_type,
    output_fields = output_fields,
    cache = cache
  )

  return(geocoded_data)
}

geocode_addresses <- function(locations,
                              locator,
                              address_fields,
                              location_type,
                              output_fields,
                              cache) {
  if (isFALSE(cache)) {
    output_file <- do_geocode(
      locations,
      locator = locator,
      address_fields = address_fields,
      location_type = location_type,
      output_fields = output_fields
    )

    geocoded_data <- read_geocoded_data_from_path(output_file, address_fields)

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

  cache_path <- file.path("../../data-raw/geocode_cache", paste0(cache, ".rds"))

  if (file.exists(cache_path)) {
    geocoded_data <- geocode_with_previous_cache(
      locations,
      locator = locator,
      address_fields = address_fields,
      location_type = location_type,
      output_fields = output_fields,
      cache_path = cache_path
    )
  } else {
    geocoded_data <- geocode_without_previous_cache(
      locations,
      locator = locator,
      address_fields = address_fields,
      location_type = location_type,
      output_fields = output_fields,
      cache_path = cache_path
    )
  }

  return(geocoded_data)
}

geocode_with_previous_cache <- function(locations,
                                        locator,
                                        address_fields,
                                        location_type,
                                        output_fields,
                                        cache_path) {
  cache_data <- readRDS(cache_path)

  lookup_vector <- names(address_fields)
  names(lookup_vector) <- address_fields

  lookup_expression <- build_lookup_expression()
  locations_with_data <- data.table::as.data.table(locations)
  locations_with_data[cache_data, on = lookup_vector, eval(lookup_expression)]

  cached_data <- locations_with_data[!is.na(Score)]
  cached_data <- format_cached_addresses(cached_data, names(locations))

  uncached_locations <- locations_with_data[is.na(Score)]
  new_geocoded_data <- geocode_and_append_to_cache(
    uncached_locations,
    cache_data = cache_data,
    cache_path = cache_path,
    locator = locator,
    address_fields = address_fields,
    location_type = location_type,
    output_fields = output_fields
  )

  geocoded_data <- rbind(
    cached_data,
    new_geocoded_data
  )

  return(geocoded_data)
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

format_cached_addresses <- function(cached_data, locations_names) {
  data.table::setcolorder(
    cached_data,
    c(setdiff(names(cached_data), locations_names), locations_names)
  )
  cached_data <- sf::st_as_sf(cached_data, coords = c("Lon", "Lat"), crs = 4326)
  cached_data <- dplyr::rename(cached_data, geom = geometry)

  return(cached_data)
}

geocode_and_append_to_cache <- function(uncached_locations,
                                        cache_data,
                                        cache_path,
                                        locator,
                                        address_fields,
                                        location_type,
                                        output_fields) {
  if (nrow(uncached_locations) == 0) {
    return(empty_geocoded_data(address_fields))
  }

  uncached_locations[
    ,
    setdiff(names(uncached_locations), address_fields) := NULL
  ]

  new_geocoded_data_path <- do_geocode(
    uncached_locations,
    locator = locator,
    address_fields = address_fields,
    location_type = location_type,
    output_fields = output_fields
  )

  new_geocoded_data <- read_geocoded_data_from_path(
    new_geocoded_data_path,
    address_fields
  )

  append_to_cache(
    new_geocoded_data,
    cache_data = cache_data,
    cache_path = cache_path,
    address_fields = address_fields,
    location_type = location_type
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
                            location_type) {
  new_data_in_cache_structure <- to_cache_structure(
    new_geocoded_data,
    address_fields = address_fields,
    location_type = location_type,
    lookup_only = FALSE
  )
  new_data_in_cache_structure <- unique(new_data_in_cache_structure)

  cache_data <- rbind(cache_data, new_data_in_cache_structure)
  saveRDS(cache_data, cache_path)

  return(invisible(cache_path))
}

geocode_without_previous_cache <- function(locations,
                                           locator,
                                           address_fields,
                                           location_type,
                                           output_fields,
                                           cache_path) {
  geocoded_path <- do_geocode(
    locations,
    locator = locator,
    address_fields = address_fields,
    location_type = location_type,
    output_fields = output_fields
  )

  geocoded_data <- read_geocoded_data_from_path(geocoded_path, address_fields)

  create_geocode_cache(
    geocoded_data,
    address_fields = address_fields,
    location_type = location_type,
    cache_path = cache_path
  )

  return(geocoded_data)
}

do_geocode <- function(locations,
                       locator,
                       address_fields,
                       location_type,
                       output_fields) {
  tmpfile_input <- tempfile("geocode_input", fileext = ".csv")
  data.table::fwrite(locations, tmpfile_input)

  tmpfile_output <- tempfile("geocode_output", fileext = ".gdb")
  tmpfile_basename <- basename(tmpfile_output)
  tmpfile_creation_result <- arcpy$CreateFileGDB_management(
    out_folder_path = tempdir(),
    out_name = tmpfile_basename
  )

  address_fields_string <- fields_to_string(address_fields)

  # a função retorna uma string listando o enderço de output, mas com uma
  # formatação extra e superficial. a variável abaixo não é usada pra nada, só
  # pra "silenciar" o output da função. estamos realmente interessados no
  # "side-effect" da função abaixo, que é geolocalizar os endereços e escrever
  # o output na camada geocode_result do geodatabase de output

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

read_geocoded_data_from_path <- function(output_file, address_fields) {
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
                                 cache_path) {
  data_in_cache_structure <- to_cache_structure(
    geocoded_data,
    address_fields = address_fields,
    location_type = location_type,
    lookup_only = FALSE
  )
  data_in_cache_structure <- unique(data_in_cache_structure)

  saveRDS(data_in_cache_structure, cache_path)

  return(invisible(cache_path))
}

to_cache_structure <- function(geocoded_data,
                               address_fields,
                               location_type,
                               lookup_only) {
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
