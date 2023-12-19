arcpy <- NULL

.onLoad <- function(libname, pkgname) {
  arcpy <<- reticulate::import("arcpy", delay_load = TRUE)
}
