Sys.setenv(CHROMOTE_CHROME = "/usr/bin/brave-browser")

expect_snapshot_json <- function(x, name = "json", ...) {
  path <- tempfile(fileext = ".json")
  jsonlite::write_json(x, path)
  expect_snapshot_file(path, name = name, ...)
}