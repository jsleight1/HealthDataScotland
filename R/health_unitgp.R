
#' R6 class storing health statistics for a list of health units.
health_unitgrp <- R6Class(
    "health_unitgrp", 
    public = list(
         #' @field .data A list of health units in health unit grp.
        .data = NA,
        #' @field .json A SpatialPointsDataFrame storing spatial information.
        .json = NA,
        #' @description
        #' Create instance of health unit grp.
        #' @param .data (`list`)\cr
        #'     A list of health units in health unit grp.
        #' @param .json (`SpatialPointsDataFrame`)
        #'     A SpatialPointsDataFrame storing spatial information for 
        #'     health units.
        initialize = function(.data, .json) {
            self[[".data"]] = .data
            self[[".json"]] = .json
            self[["validate"]]()
        }, 
        #' @description
        #' Validate structure of health unit grp.
        validate = function() {
            assert_that(
                length(unique(purrr::map(self[["data"]](), class))) == 1,
                msg = "group must contain the same class of health units"   
            )
            assert_that(
                inherits(self[["json"]](), "SpatialPointsDataFrame"), 
                msg = "JSON must be SpatialPointsDataFrame object"
            )
            assert_that(
                all(self[["ids"]]() %in% self[["json"]]()[["id"]]), 
                msg = "All all health units present in JSON"
            )
            self
        },
        #' @description
        #' Get data of health unit grp.
        data = function() {
            self[[".data"]]
        },
        #' @description
        #' Get JSON of health unit grp.
        json = function() {
            self[[".json"]]
        },
        #' @description
        #' Get ids of stored health units
        ids = function() {
            map_chr(self[["data"]](), function(i) as.character(i[["id"]]()))
        }, 
        #' @description
        #' Get stored health unit by unit.
        #' @param id (character(1))\cr
        #'     Character specifying ID of object to obtain from group.
        health_unit = function(id) {
            self[["data"]]()[[which(self[["ids"]]() == id)]]
        }
    )
)
