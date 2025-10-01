#' R6 class storing health statistics for a list of health units.
health_unitgrp <- R6Class(
  "health_unitgrp",
  private = list(
    map_combine = function(func, nms = self[["IDs"]](), id = "ID", ...) {
      args <- list(...)
      self[["data"]]() |>
        map(~ do.call(.x[[func]], args)) |>
        set_names(nms) |>
        bind_rows(.id = id)
    },
    id_name_labels = function(x, name) {
      deframe(distinct(select(x, "ID", all_of(name))))
    },
    id_name_selection = function() {
      set_names(self[["IDs"]](), self[["titles"]]())
    },
    trend_echart = function(x, x_axis, y_axis) {
      e_trend(x, x_axis, y_axis) |>
        e_legend(show = FALSE)
    },
    bar_echart = function(x, group, x_axis, y_axis) {
      plt <- x |>
        e_charts_(group, timeline = TRUE) |>
        e_timeline_opts(autoPlay = TRUE, top = 0) |>
        e_tooltip(trigger = "axis") |>
        e_legend(show = FALSE) |>
        e_y_axis(name = y_axis) |>
        e_x_axis(name = x_axis)
      for (col in setdiff(colnames(x), c("Date", group))) {
        plt <- e_bar_(plt, col)
      }
      plt
    },
    health_board_choices = function() {
      unique(self[["metadata"]]()[["HBName"]])
    },
    unit_choices = function() {
      unique(paste(self[["IDs"]](), "-", self[["titles"]]()))
    }
  ),
  public = list(
    #' @field .data A list of health units in health unit grp.
    .data = NA,
    #' @field .id A character ID of object.
    .id = NA,
    #' @description
    #' Create instance of health unit grp.
    #' @param .data (`list`)\cr
    #'     A list of health units in health unit grp.
    #' @param .id (`characer`)
    #'     A character ID of object.
    initialize = function(.data, .id) {
      self[[".data"]] <- .data
      self[[".id"]] <- .id
      self[["validate"]]()
    },
    #' @description
    #' Validate structure of health unit grp.
    validate = function() {
      assert_that(
        inherits(self[["ID"]](), "character") && length(self[["ID"]]()) == 1,
        msg = "ID must be character of length 1"
      )
      assert_that(
        length(unique(map(self[["data"]](), class))) == 1,
        msg = "group must contain the same class of health units"
      )
      assert_that(!any(duplicated(self[["IDs"]]())),
        msg = "Health units must not be duplicated"
      )
      self
    },
    #' @description
    #' Get combine metadata of health unit grp.
    metadata = function() {
      private[["map_combine"]]("metadata")
    },
    #' @description
    #' Get data of health unit grp.
    data = function() {
      self[[".data"]]
    },
    #' @description
    #' Get combined metadata and data in single data.frame.
    combine_data = function() {
      private[["map_combine"]]("data") |>
        inner_join(self[["metadata"]](), by = "ID") |>
        distinct()
    },
    #' @description
    #' Get ID of health unit grp.
    ID = function() {
      self[[".id"]]
    },
    #' @description
    #' Get IDs of stored health units
    IDs = function() {
      unname(map_chr(self[["data"]](), ~ .x[["ID"]]()))
    },
    #' @description
    #' Get names of stored health units
    titles = function() {
      unname(map_chr(self[["data"]](), ~ .x[["title"]]()))
    },
    #' @description
    #' Get stored health unit.
    #' @param id (character(1))\cr
    #'     Character specifying ID of object to obtain from group.
    health_unit = function(id) {
      self[["data"]]()[[which(self[["IDs"]]() == id)]]
    },
    #' @description
    #' Subset health unit group.
    #' @param id (character())\cr
    #'     Character specifying ID (or IDs) of object to obtain from group.
    subset = function(id) {
      out <- self[["clone"]](deep = TRUE)
      assert_that(all(id %in% out[["IDs"]]()),
        msg = "IDs are not found in health unit group"
      )
      out[[".data"]] <- out[[".data"]][which(out[["IDs"]]() %in% id)]
      out[["validate"]]()
    },
    #' @description
    #' Get download handler function of health unit group.
    download_handler = function() {
      downloadHandler(
        filename = function() glue('{self[["ID"]]()}_data.csv'),
        content = function(con) {
          write.csv(self[["combine_data"]](), con)
        }
      )
    }
  )
)
