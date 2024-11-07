
#' R6 class storing health statistics for a list of health units.
health_unitgrp <- R6Class(
    "health_unitgrp", 
    public = list(
         #' @field .data A list of health units in health unit grp.
        .data = NA,
        #' @field .sf A sf storing spatial information.
        .sf = NA,
        #' @description
        #' Create instance of health unit grp.
        #' @param .data (`list`)\cr
        #'     A list of health units in health unit grp.
        #' @param .sf (`sf`)
        #'     A sf storing spatial information for 
        #'     health units.
        initialize = function(.data, .sf) {
            self[[".data"]] = .data
            self[[".sf"]] = .sf
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
                inherits(self[["sf"]](), "sf"), 
                msg = "sf must be sf object"
            )
            assert_that(
                identical(
                    sort(self[["ids"]]()),
                    sort(self[["sf"]]()[["id"]])
                ), 
                msg = "All all health units present in sf"
            )
            self
        },
        #' @description
        #' Get data of health unit grp.
        data = function() {
            self[[".data"]]
        },
        #' @description
        #' Get sf of health unit grp.
        sf = function() {
            self[[".sf"]]
        },
        #' @description
        #' Get ids of stored health units
        ids = function() {
            unname(map_chr(self[["data"]](), function(i) i[["id"]]()))
        }, 
        #' @description
        #' Get stored health unit.
        #' @param id (character(1))\cr
        #'     Character specifying ID of object to obtain from group.
        health_unit = function(id) {
            self[["data"]]()[[which(self[["ids"]]() == id)]]
        }, 
        #' @description
        #' Subset health unit group.
        #' @param id (character())\cr
        #'     Character specifying ID (or IDs) of object to obtain from group.
        subset = function(id) {
            assert_that(all(id %in% self[["ids"]]()), 
                msg = "ids are not found in health unit group")
            self[[".data"]] <- self[[".data"]][which(self[["ids"]]() %in% id)]
            self[[".sf"]] <- self[[".sf"]][self[[".sf"]][["id"]] %in% id, ]
            self[["validate"]]()
        }
    )
)
