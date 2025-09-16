virtual_select_input <- function(...) {
  virtualSelectInput(
    ...,
    search = TRUE,
    html = TRUE,
    showSelectedOptionsFirst = TRUE,
    updateOn = "close"
  )
}

help_popover <- function(id, ...) {
  popover(id = id, bs_icon("question-circle"), ...)
}

settings_popover <- function(id, ...) {
  popover(id = id, bs_icon("gear", class = "ms-auto"), ...)
}
