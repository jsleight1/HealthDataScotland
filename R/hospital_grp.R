#' R6 class storing health statistics for a list of hospital health units.
hospital_grp <- R6Class("hospital_grp", 
    inherit = health_unitgrp, 
    private = list(
        specialty_bar_data = function(specialties = "All Specialties") {
            self[["data"]]() %>% 
                map(function(i) i[["plot_data"]]("specialty_bar", specialties)) %>% 
                setNames(self[["ids"]]()) %>% 
                bind_rows(.id = "ID")
        },
        specialty_bar = function(specialties = "All Specialties", ...) {
            plot <- self[["plot_data"]]("specialty_bar", specialties, ...) %>%
                ggplot(
                    aes(
                        x = .data[["FinancialYear"]], 
                        y = .data[["AllStaffedBeds"]], 
                        fill = .data[["SpecialtyName"]], 
                        ID = .data[["ID"]]
                    )
                ) + 
                    geom_bar(stat = "identity") + 
                    theme_bw() + 
                    theme(axis.text.x = element_text(angle = 90)) +
                    facet_wrap(~ID)
            ggplotly(plot, tooltip = c("FinancialYear", "SpecialtyName", 
                "AllStaffedBeds", "ID"))
        }
    ),
    public = list(
        #' @description
        #' Get character vector of available plots for hospital grp. Options 
        #'   are either "specialty_bar" plot.
        available_plots = function() {
            c("specialty_bar")
        },
        #' @description
        #' Plot hospital grp.
        #' @param type (character(1))\cr
        #'     Character specifying plot type. See `available_plots` for options.
        #' @param ... Passed to plot functions.
        plot = function(type, ...) {
            type <- arg_match(type, values = self[["available_plots"]]())
            switch(type, 
                "specialty_bar" = private[["specialty_bar"]](...)
            )
        }, 
        #' @description
        #' Generate plot data for hospital grp.
        #' @param type (character(1))\cr
        #'     Character specifying plot type. See `available_plots` for options.
        #' @param ... Passed to plot functions.
        plot_data = function(type, ...) {
            type <- arg_match(type, values = self[["available_plots"]]())
            switch(type, 
                "specialty_bar" = private[["specialty_bar_data"]](...)
            )
        }, 
        #' @description
        #' Summarise hospital grp unit.
        #' @param id (character(1))\cr
        #'     Character specifying the ID to assign to output data.frame.
        #' @param ... Passed to plot_data.
        summary = function(id = "Scotland national", ...) {
            self[["plot_data"]]("specialty_bar", ...) %>% 
                mutate(ID = id) %>%
                select("ID", "FinancialYear", "SpecialtyName", "AllStaffedBeds") %>% 
                group_by_if(is.character) %>%
                summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
                ungroup()
        }
    )
)
