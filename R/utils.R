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

e_pyramid <- function(x) {
  x |>
    e_charts_("Age", timeline = TRUE, reorder = FALSE) |>
    e_timeline_opts(autoPlay = TRUE) |>
    e_bar_("Male", stack = "quantity") |>
    e_bar_("Female", stack = "quantity") |>
    e_flip_coords() |>
    e_x_axis(
      axisLabel = list(
        formatter = htmlwidgets::JS(
          "function (value) {
            return(Math.abs(value))
          }"
        )
      )
    ) |>
    e_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS("
        function(params){
          return(
            '<strong>' + 'Age: ' + '</strong>' + params.name + ' years' + '<br />' +
            '<strong>' + 'Population: ' + '</strong>' + Math.abs(params.value[0])
          )
        }
        ")
    ) |>
    e_legend(
      top = 10,
      left = "center",
      data = c("Female", "Male")
    )
}

pyramid_data <- function(x, ...) {
  x |>
    filter(.data[["Sex"]] != "All") |>
    select("Date",
      "Gender" = "Sex", matches("Ages\\d"), -contains("QF"),
      -"AllAges"
    ) |>
    pivot_longer(
      -c("Gender", "Date"),
      names_to = "Age",
      values_to = "Population"
    ) |>
    summarise_population(...) |>
    pivot_wider(names_from = "Gender", values_from = "Population") |>
    factor_age() |>
    mutate(Female = .data[["Female"]] * -1) |>
    arrange(.data[["Date"]], desc(.data[["Age"]])) |>
    group_by(.data[["Date"]])
}

factor_age <- function(x) {
  x |>
    mutate(Age = factor(.data[["Age"]], levels = names(gp_age_columns())))
}

gp_age_columns <- function() {
  list(
    "Ages85plus" = c("Ages85plus"),
    "Ages75to84" = c("Ages75to84", "Ages75to79", "Ages80to84"),
    "Ages65to74" = c("Ages65to74", "Ages65to69", "Ages70to74"),
    "Ages45to64" = c("Ages45to64", "Ages45to49", "Ages50to54", "Ages55to59", "Ages60to64"),
    "Ages25to44" = c("Ages25to44", "Ages25to29", "Ages30to34", "Ages35to39", "Ages40to44"),
    "Ages15to24" = c("Ages15to24", "Ages15to19", "Ages20to24"),
    "Ages5to14" = c("Ages5to14", "Ages05to09", "Ages10to14"),
    "Ages0to4" = c("Ages0to4", "Ages00to04")
  )
}

summarise_population <- function(x, groups = colnames(x)) {
  x |>
    group_by_at(groups) |>
    summarise(Population = sum(.data[["Population"]], na.rm = TRUE)) |>
    ungroup()
}

e_trend <- function(x, x_axis, y_axis) {
  x |>
    e_charts_(x_axis) |>
    e_line_(y_axis) |>
    e_tooltip(trigger = "axis") |>
    e_y_axis(name = y_axis) |>
    e_x_axis(name = x_axis) |>
    e_datazoom() |>
    e_toolbox(show = FALSE)
}

e_output_spinner <- function(..., height = "600px") {
  withSpinner(echarts4rOutput(..., height = height))
}

downloadable_datatable <- function(x, ...) {
  datatable(
    x,
    rownames = FALSE,
    escape = FALSE,
    filter = "top",
    selection = "none",
    extensions = "Buttons",
    options = list(
      dom = "frtipB",
      buttons = c("csv", "excel")
    ),
    ...
  )
}
