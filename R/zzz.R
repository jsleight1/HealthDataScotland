.onLoad <- function(libname, pkgname) {
  resources <- system.file("www", package = "HealthDataScotland")
  addResourcePath("www", resources)
}
