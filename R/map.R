#' R6 class for creating interactive map of health units.
#'
#' This R6 class is designed to store sf spatial data for GP practices,
#' hospitals and health boards. This class is used to create a map
#' with interactive pins of health centres.
#'
#' @examples
#' x <- example_map_unit()
#' x[["id"]]()
#' x[["title"]]()
#' x[["sf"]]()
#' x[["plot"]](type = "interactive_map")
#' x[["plot_data"]](type = "interactive_map")
#' x[["plot_info"]](type = "interactive_map")
map <- R6Class("map",
  private = list(
    required_sf_cols = function() {
      c("ID", "type")
    },
    interactive_map = function(...) {
      data <- self[["plot_data"]](type = "interactive_map", ...)
      boards <- self[["sf"]]()[self[["sf"]]()[["type"]] == "board", ]
      leaflet() |>
        addTiles() |>
        addAwesomeMarkers(
          layerId = ~ paste0(as.character(ID), ":", as.character(type)),
          icon = ~ private[["pin_icon"]](type),
          clusterOptions = markerClusterOptions(
            showCoverageOnHover = FALSE
          ),
          label = ~ as.character(ID),
          data = data
        ) |>
        addPolygons(
          fillOpacity = 0.01,
          smoothFactor = 0.1,
          weight = 2,
          color = "#377EB8",
          label = as_tibble(boards)[["HBName"]],
          layerId = ~ as.character(ID),
          data = boards
        )
    },
    pin_icon = function(type, ...) {
      awesomeIcons(
        icon = "ios-close",
        iconColor = "white",
        library = "ion",
        markerColor = ifelse(type == "gp", "blue", "red"),
        ...
      )
    },
    interactive_map_data = function(ids = private[["all_pin_ids"]]()) {
      self[["sf"]]()[self[["sf"]]()[["ID"]] %in% ids, ]
    },
    all_pin_ids = function() {
      self[["sf"]]()[self[["sf"]]()[["type"]] != "board", ][["ID"]]
    },
    interactive_map_info = function() {
      "The interactive map below can be used to visualise individual GP
        practice or hospital data sets"
    }
  ),
  public = list(
    #' @field .sf A sf spatial data.frame storing spatial data for health units.
    .sf = NA,
    #' @field .id Character ID of object.
    .id = NA,
    #' @field .title Character title of object.
    .title = NA,
    #' @description
    #' Create instance of map.
    #' @param .sf (`sf`)\cr
    #'   A sf spatial data.frame storing spatial data for health units.
    #' @param .id (character(1))\cr
    #'   Character id of object. Default is "map".
    #' @param .title (character(1))\cr
    #'   Character title of object. Default is "Interactive Map".
    initialize = function(.sf, .id = "map", .title = "Interactive Map") {
      self[[".sf"]] <- .sf
      self[[".id"]] <- .id
      self[[".title"]] <- .title
      self[["validate"]]()
    },
    #' @description
    #' Validate structure of map object.
    validate = function() {
      assert_that(inherits(self[["sf"]](), "sf"),
        msg = "sf must be sf spatial data.frame"
      )
      col_check <- private[["required_sf_cols"]]() %in% colnames(self[["sf"]]())
      assert_that(all(col_check),
        msg = paste(
          paste(private[["required_sf_cols"]]()[!col_check], collapse = ", "),
          glue("column missing from sf")
        )
      )
      self
    },
    #' @description
    #' Get sf spatial data.frame of map object.
    sf = function() {
      self[[".sf"]]
    },
    #' @description
    #' Get id of map object.
    id = function() {
      self[[".id"]]
    },
    #' @description
    #' Get title of map object.
    title = function() {
      self[[".title"]]
    },
    #' @description
    #' Get character vector of plot types for map object.
    plot_types = function() {
      "interactive_map"
    },
    #' @description
    #' Plot map object.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `plot_types`
    #'   for options.
    #' @param ... Passed to plot functions.
    #' @examples
    #' x <- example_map_unit()
    #' x[["plot"]](type = "interactive_map")
    plot = function(type, ...) {
      type <- arg_match(type, values = self[["plot_types"]]())
      switch(type,
        "interactive_map" = private[["interactive_map"]](...)
      )
    },
    #' @description
    #' Generate plot data for map object.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `plot_types`
    #'   for options.
    #' @param ... Passed to plot data functions.
    #' @examples
    #' x <- example_map_unit()
    #' x[["plot_data"]](type = "interactive_map")
    plot_data = function(type, ...) {
      type <- arg_match(type, values = self[["plot_types"]]())
      switch(type,
        "interactive_map" = private[["interactive_map_data"]]
      )(...)
    },
    #' @description
    #' Get plot infot for map object.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `plot_types`
    #'   for options.
    #' @param ... Passed to plot data functions.
    #' @examples
    #' x <- example_map_unit()
    #' x[["plot_info"]](type = "interactive_map")
    plot_info = function(type, ...) {
      type <- arg_match(type, values = self[["plot_types"]]())
      switch(type,
        "interactive_map" = private[["interactive_map_info"]]
      )(...)
    },
    #' @description
    #' Create ui for map object.
    #' @param ... Passed to methods.
    ui = function(...) {
      ns <- NS(self[["id"]]())
      card(
        full_screen = TRUE,
        card_header(
          self[["title"]](),
          help_popover(
            id = ns("map-help"),
            self[["plot_info"]](type = "interactive_map")
          ),
          settings_popover(
            id = ns("map-settings"),
            virtual_select_input(
              ns("health_select"),
              label = "Display health centre types",
              choices = c("General practice" = "gp", "Hospital" = "hospital"),
              selected = c("gp", "hospital"),
              inline = TRUE,
              multiple = TRUE
            ),
            title = "Map settings"
          )
        ),
        leafletOutput(ns("map"), width = "100%", height = "100%")
      )
    },
    #' @description
    #' Create server for map object.
    #' @param data (list)\cr
    #'    List of with gp_grp and hospital_grp objects to use for interactive
    #'    map pop-up modals.
    server = function(data) {
      moduleServer(
        self[["id"]](),
        function(input, output, session) {
          ns <- session[["ns"]]

          observe({
            log_info("Creating selected map pin pop-up")
            id <- self[["id"]]()
            leafletProxy(id) |> clearPopups()
            event <- input[[paste0(id, "_marker_click")]]
            if (is.null(event)) {
              return()
            }
            id <- strsplit(event[["id"]], ":")[[1]][[1]]
            type <- strsplit(event[["id"]], ":")[[1]][[2]]
            obj <- data[[type]][["health_unit"]](id)
            isolate(obj[["popup_modal"]](ns))
          })

          pin_ids <- reactive({
            centre_types <- req(input[["health_select"]])
            pin_sf <- self[["sf"]]()
            pin_sf[pin_sf[["type"]] %in% centre_types, ][["ID"]]
          })

          output[["map"]] <- renderLeaflet({
            log_info("Creating interactive map")
            self[["plot"]](type = "interactive_map", ids = pin_ids())
          })
        }
      )
    }
  )
)

#' Get example map object.
#' @examples
#' example_map_unit()
#' @export
example_map_unit <- function() {
  filtered_sf <- function(x, type) {
    sf <- get_sf(type)
    sf <- sf[sf[["ID"]] %in% x[["IDs"]](), ]
  }
  gp_sf <- filtered_sf(example_gp_grp_unit(), "gp")
  hosp_sf <- filtered_sf(example_hospital_grp_unit(), "hospital")
  create_map_unit(list(list("sf" = gp_sf), list("sf" = hosp_sf)))
}
