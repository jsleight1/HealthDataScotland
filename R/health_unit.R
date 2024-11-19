
#' R6 class representing a health unit e.g. a GP practice or hospital.
health_unit <- R6Class("health_unit", 
    public = list(
        #' @field .data A data.frame storing health statistics.
        .data = NA, 
        #' @description
        #' Create instance of health unit.
        #' @param .data (`data.frame`)\cr
        #'     A data.frame storing health statistics.
        initialize = function(.data) {
            self[[".data"]] = .data
            self[["validate"]]()
        }, 
        #' @description
        #' Validate structure of health unit.
        validate = function() {
            assert_that(inherits(self[["data"]](), "data.frame"), 
                msg = "Data set must be in data.frame")
            col_check <- private[["required_cols"]]() %in% colnames(self[["data"]]())
            assert_that(all(col_check), 
                msg = paste(
                    paste(private[["required_cols"]]()[!col_check], collapse = ", "), 
                    "column missing from data"
                )
            )    
            assert_that(length(unique(self[["data"]]()[["ID"]])) == 1, 
                msg = paste("Data set must contain only one unique ID")
            )   
            self
        },
        #' @description
        #' Get data of health unit.
        data = function() {
            self[[".data"]]
        },
        #' @description
        #' Get ID of health unit.
        ID = function() {
            unique(self[["data"]]()[["ID"]])
        },
        #' @description
        #' Get title of health unit.
        title = function() {
            paste(unique(self[["data"]]()[[private[["title_col"]]()]]))
        },
        #' @description
        #' Get address of health unit.
        address = function() {
            self[["data"]]() |> 
                select(contains("Address"), -contains("QF"), "Postcode") |> 
                select_if(~ !any(is.na(.))) |>
                distinct() |> 
                unite(col = "x", sep = ", ") |> 
                pull("x")
        },
        #' @description
        #' Get health board of health unit.
        health_board = function() {
            unique(self[["data"]]()[["HBName"]])
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
        }
    )
)