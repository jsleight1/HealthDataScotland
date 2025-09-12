# hospital plot info works

    Code
      hosp_unit[["plot_info"]]("specialty_line", "annual")
    Output
      [1] "This line chart shows the annual number of\n                    available staffed beds and the number occupied (y-axis)\n                    across time (x-axis). Settings can be used to show data for\n                    different specialties (default is all specialities)."

---

    Code
      hosp_unit[["plot_info"]]("specialty_line", "daily")
    Output
      [1] "This line chart shows the daily average number of\n                    available staffed beds and the number occupied (y-axis)\n                    across time (x-axis). Settings can be used to show data for\n                    different specialties (default is all specialities)."

