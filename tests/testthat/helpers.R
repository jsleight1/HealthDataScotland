

expect_snapshot_plotly <- function(p, name, ...) {
    wd <- getwd()
    dir <- setwd(tempdir())
    on.exit(setwd(dir))
    fs::dir_create(name)
    htmlwidgets::saveWidget(p, file = paste0(name, "/index.html"))
    file <- paste0(name, ".png")
    webshot2::webshot(paste0(name, "/index.html"), file, quiet = TRUE)
    expect_snapshot_file(file)
    setwd(wd)
}