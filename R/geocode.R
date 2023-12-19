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
#' @param country Uma string. O país no qual os endereços especificados estão
#'   localizados. Pode aumentar a precisão da geolocalização. Por padrão,
#'   `"BRA"` (Brasil).
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
                    country = "BRA",
                    location_type = "ROUTING_LOCATION",
                    output_fields = "MINIMAL") {
  checkmate::assert_data_frame(locations)
  checkmate::assert_file_exists(locator, extension = ".loc")
  checkmate::assert_string(country)
  checkmate::assert_string(location_type)
  checkmate::assert_string(output_fields)
  assert_address_fields(address_fields, locations)

  address_fields_string <- fields_to_string(address_fields)

  tmpfile_input <- tempfile("geocode_input", fileext = ".csv")
  data.table::fwrite(locations, tmpfile_input)

  tmpfile_output <- tempfile("geocode_output", fileext = ".shp")

  arcpy$env$overwriteOutput <- TRUE

  geocode_fn_output <- arcpy$geocoding$GeocodeAddresses(
    in_table = tmpfile_input,
    address_locator = normalizePath(locator),
    in_address_fields = address_fields_string,
    out_feature_class = tmpfile_output,
    out_relationship_type = "STATIC",
    country = country,
    location_type = location_type,
    output_fields = output_fields
  )

  geocoded_locations <- sf::st_read(tmpfile_output, quiet = TRUE)

  return(geocoded_locations)
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

#' Construtor de especificação de colunas
#'
#' Auxilia a criar um vetor de caracteres que especifica as colunas que
#' representam cada campo de endereço no dataframe de localizações.
#'
#' @param Address_or_Place,Address2,Address3,Neighborhood,City,County,State,ZIP,ZIP4,Country
#'   Uma string. O nome da coluna que representa o respectivo campo de endereço
#'   no dataframe de localizações. Pode ser `NULL`, no caso do campo não ser
#'   especificado no dataframe. O valor de ao menos um dos campos não deve ser
#'   nulo.
#'
#' @return Um vetor de caracteres com o nome da coluna que representa cada campo
#'   no dataframe de localizações especificado.
#'
#' @export
address_fields_const <- function(Address_or_Place = NULL,
                                 Address2 = NULL,
                                 Address3 = NULL,
                                 Neighborhood = NULL,
                                 City = NULL,
                                 County = NULL,
                                 State = NULL,
                                 ZIP = NULL,
                                 ZIP4 = NULL,
                                 Country = NULL) {
  col <- checkmate::makeAssertCollection()
  checkmate::assert_string(Address_or_Place, null.ok = TRUE, add = col)
  checkmate::assert_string(Address2, null.ok = TRUE, add = col)
  checkmate::assert_string(Address3, null.ok = TRUE, add = col)
  checkmate::assert_string(Neighborhood, null.ok = TRUE, add = col)
  checkmate::assert_string(City, null.ok = TRUE, add = col)
  checkmate::assert_string(County, null.ok = TRUE, add = col)
  checkmate::assert_string(State, null.ok = TRUE, add = col)
  checkmate::assert_string(ZIP, null.ok = TRUE, add = col)
  checkmate::assert_string(ZIP4, null.ok = TRUE, add = col)
  checkmate::assert_string(Country, null.ok = TRUE, add = col)
  checkmate::reportAssertions(col)

  address_fields_vec <- c(
    Address_or_Place = Address_or_Place,
    Address2 = Address2,
    Address3 = Address3,
    Neighborhood = Neighborhood,
    City = City,
    County = County,
    State = State,
    ZIP = ZIP,
    ZIP4 = ZIP4,
    Country = Country
  )

  if (is.null(address_fields_vec)) {
    error_address_fields_must_not_be_null()
  }

  return(address_fields_vec)
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


error_address_fields_must_not_be_null <- function() {
  cli::cli_abort(
    paste0(
      "At least one of {.fn address_fields_const} ",
      "arguments must not be {.code NULL}."
    ),
    class = "address_fields_must_not_be_null",
    call = rlang::caller_env()
  )
}
