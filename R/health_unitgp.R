
#' R6 class storing health statistics for a list of health units.
health_unitgrp <- R6Class(
    "health_unitgrp",
    public = list(
         #' @field .data A list of health units in health unit grp.
        .data = NA,
        #' @field .sf A sf storing spatial information.
        .sf = NA,
        #' @field .id A character ID of object.
        .id = NA,
        #' @description
        #' Create instance of health unit grp.
        #' @param .data (`list`)\cr
        #'     A list of health units in health unit grp.
        #' @param .sf (`sf`)
        #'     A sf storing spatial information for
        #'     health units.
        #' @param .id (`characer`)
        #'     A character ID of object.
        initialize = function(.data, .sf, .id) {
            self[[".data"]] = .data
            self[[".sf"]] = .sf
            self[[".id"]] = .id
            self[["validate"]]()
        },
        #' @description
        #' Validate structure of health unit grp.
        validate = function() {
            assert_that(
                inherits(self[["id"]](), "character") && length(self[["id"]]()) == 1,
                msg = "ID must be character of length 1"
            )
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
                    sort(self[["sf"]]()[["ID"]])
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
        #' Get ID of health unit grp.
        id = function() {
            self[[".id"]]
        },
        #' @description
        #' Get ids of stored health units
        ids = function() {
            unname(map_chr(self[["data"]](), ~.x[["ID"]]()))
        },
        #' @description
        #' Get names of stored health units
        titles = function() {
            unname(map_chr(self[["data"]](), ~.x[["title"]]()))
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
            out <- self[["clone"]](deep = TRUE)
            assert_that(all(id %in% out[["ids"]]()),
                msg = "ids are not found in health unit group")
            out[[".data"]] <- out[[".data"]][which(out[["ids"]]() %in% id)]
            out[[".sf"]] <- out[[".sf"]][out[[".sf"]][["ID"]] %in% id, ]
            out[["validate"]]()
        },
        #' @description
        #' Get downloadable data.frame of health unit group.
        get_download = function() {
            self[["data"]]() |>
               map_dfr(~.x[["data"]]())
        },
        #' @description
        #' Create UI for health unit group object.
        #' @param ns
        #'     Namespace of shiny application page.
        #' @param title
        #'     Character title of box. Default is "Health data"
        download_ui = function(ns, title = "Health data") {
            ns <- NS(ns(paste0(self[["id"]](), "_download")))
            box(
                title = title,
                column(
                    spinner(
                        reactableOutput(ns("table"))
                    ),
                    tags[["button"]](
                        "Download as CSV",
                        onclick = paste0(
                            "Reactable.downloadDataCSV('",
                            ns("health_data"),
                            "', 'health_data.csv')"
                        )
                    ),
                    width = 12
                ),
                width = 12,
                status = "primary",
                solidHeader = TRUE
            )
        },
        #' @description
        #' Create server for health unit group object.
        download_server = function() {
            moduleServer(
                paste0(self[["id"]](), "_download"),
                function(input, output, session) {
                    ns <- session[["ns"]]
                    output[["table"]] <- renderReactable({
                        reactable(
                            self[["get_download"]](),
                            filterable = TRUE,
                            elementId = ns("health_data")
                        )
                    })
                }
            )
        }
    )
)
