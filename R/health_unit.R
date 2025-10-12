#' R6 class representing a health unit e.g. a GP practice or hospital.
health_unit <- R6Class("health_unit",
  public = list(
    #' @field .metadata A data.frame storing meatadata for health unit
    .metadata = NA,
    #' @field .data A data.frame storing health statistics for health unit.
    .data = NA,
    #' @description
    #' Create instance of health unit.
    #' @param .metadata (`data.frame`)\cr
    #'     A data.frame storing metadata for health unit.
    #' @param .data (`data.frame`)\cr
    #'     A data.frame storing health statistics for health unit.
    initialize = function(.metadata, .data) {
      self[[".metadata"]] <- .metadata
      self[[".data"]] <- .data
      self[["validate"]]()
    },
    #' @description
    #' Validate structure of health unit.
    validate = function() {
      assert_that(inherits(self[["metadata"]](), "data.frame"),
        msg = "Metadata set must be in data.frame"
      )
      assert_that(inherits(self[["data"]](), "data.frame"),
        msg = "Data set must be in data.frame"
      )
      for (req in c("metadata", "data")) {
        getter <- glue("required_{req}_cols")
        col_check <- private[[getter]]() %in% colnames(self[[req]]())
        assert_that(all(col_check),
          msg = paste(
            paste(private[[getter]]()[!col_check], collapse = ", "),
            glue("column missing from {req}")
          )
        )
      }
      assert_that(length(self[["metadata"]]()[["ID"]]) == 1,
        msg = paste("Metadata set must contain only one unique ID")
      )
      self
    },
    #' @description
    #' Get metadata of health unit.
    metadata = function() {
      self[[".metadata"]]
    },
    #' @description
    #' Get data of health unit.
    data = function() {
      self[[".data"]]
    },
    #' @description
    #' Get combined metadata and data in single data.frame.
    combine_data = function() {
      self[["metadata"]]() |>
        inner_join(self[["data"]](), by = "ID") |>
        distinct()
    },
    #' @description
    #' Get ID of health unit.
    ID = function() {
      self[["metadata"]]()[["ID"]]
    },
    #' @description
    #' Get title of health unit.
    title = function() {
      self[["metadata"]]()[[private[["title_col"]]()]]
    },
    #' @description
    #' Get address of health unit.
    address = function() {
      self[["metadata"]]() |>
        select(contains("Address"), -contains("QF"), "Postcode") |>
        select_if(~ !any(is.na(.))) |>
        distinct() |>
        unite(col = "x", sep = ", ") |>
        pull("x")
    },
    #' @description
    #' Get health board of health unit.
    health_board = function() {
      self[["metadata"]]()[["HBName"]]
    },
    #' @description
    #' Initialise model popup of health unit.
    #' @param ns (character(1))\cr
    #'      Namespace to create health unit UI.
    popup_modal = function(ns) {
      showModal(modalDialog(
        self[["ui"]](ns),
        size = "l",
        easyClose = TRUE
      ))
      self[["server"]]()
    },
    #' @description
    #' Create datatable of health unit data
    #' @param type (character(1))\cr
    #'   Character specifying summary type. See `summary_types` for options.
    #' @param ... Passed to method.
    #' @examples
    #' x <- example_gp_unit()
    #' x[["datatable"]](type = "population_summary")
    #' x <- example_hospital_unit()
    #' x[["datatable"]](type = "specialty_summary")
    datatable = function(type, ...) {
      type <- arg_match(type, values = self[["summary_types"]]())
      downloadable_datatable(self[["summary"]](type), ...)
    }
  )
)
