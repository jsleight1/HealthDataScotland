# hospital plot info works

    Code
      hosp_unit[["plot_info"]]("specialty_line")
    Output
      [1] "This line chart shows the number of\n          available staffed beds and the number occupied (y-axis)\n          across time (x-axis). Settings can be used to show data for\n          different specialties (default is all specialities) and\n          for different statistics ('annual' for the annual number of\n          beds and 'daily' for the average daily number of beds)."

# hospital summary info works

    Code
      hosp_unit[["summary_info"]](type = smy)
    Output
      [1] "This summary table presents the annual and daily average number of available staffed beds,\n        the number of annual and daily average beds occupied, and the percentage occupancy for\n        the selected hospital"

