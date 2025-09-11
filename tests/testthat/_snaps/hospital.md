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

# hospital plot data works

         ID FinancialYear                HospitalName    SpecialtyName
    1 A101H       2022/23 Arran War Memorial Hospital General Medicine
    2 A101H       2022/23 Arran War Memorial Hospital General Medicine
    3 A101H       2022/23 Arran War Memorial Hospital  All Specialties
    4 A101H       2022/23 Arran War Memorial Hospital  All Specialties
    5 A101H       2023/24 Arran War Memorial Hospital General Medicine
    6 A101H       2023/24 Arran War Memorial Hospital General Medicine
    7 A101H       2023/24 Arran War Memorial Hospital  All Specialties
    8 A101H       2023/24 Arran War Memorial Hospital  All Specialties
      PercentageOccupancy                                    name value
    1                  NA Annual number of available staffed beds     0
    2                  NA          Annual number of occupied beds     0
    3                77.4 Annual number of available staffed beds  3263
    4                77.4          Annual number of occupied beds  2526
    5                  NA Annual number of available staffed beds     0
    6                  NA          Annual number of occupied beds     0
    7                68.5 Annual number of available staffed beds  3631
    8                68.5          Annual number of occupied beds  2488

---

         ID FinancialYear                HospitalName    SpecialtyName
    1 A101H       2022/23 Arran War Memorial Hospital General Medicine
    2 A101H       2022/23 Arran War Memorial Hospital General Medicine
    3 A101H       2022/23 Arran War Memorial Hospital  All Specialties
    4 A101H       2022/23 Arran War Memorial Hospital  All Specialties
    5 A101H       2023/24 Arran War Memorial Hospital General Medicine
    6 A101H       2023/24 Arran War Memorial Hospital General Medicine
    7 A101H       2023/24 Arran War Memorial Hospital  All Specialties
    8 A101H       2023/24 Arran War Memorial Hospital  All Specialties
      PercentageOccupancy                                           name     value
    1                  NA Daily average number of available staffed beds  0.000000
    2                  NA          Daily average number of occupied beds  0.000000
    3                77.4 Daily average number of available staffed beds  9.000000
    4                77.4          Daily average number of occupied beds  6.920548
    5                  NA Daily average number of available staffed beds  0.000000
    6                  NA          Daily average number of occupied beds  0.000000
    7                68.5 Daily average number of available staffed beds 10.000000
    8                68.5          Daily average number of occupied beds  6.797814

