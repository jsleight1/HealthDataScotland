# gp plot info works

    Code
      gp_unit[["plot_info"]](type = plt)
    Output
      [1] "This bar chart shows a population pyramid of the total number of\n        GP registered patients (x-axis) across age category\n        (y-axis) for each gender (colour)."

---

    Code
      gp_unit[["plot_info"]](type = plt)
    Output
      [1] "This line chart shows the number of registered GP patients (y-axis)\n        across time (x-axis) for each gender (colour)."

# gp summary info works

    Code
      gp_unit[["summary_info"]](type = smy)
    Output
      [1] "This summary table presents the number of registered patients for the\n        selected GP practice per gender per year."

