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
