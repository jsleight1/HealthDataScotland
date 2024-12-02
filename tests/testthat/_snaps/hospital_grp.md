# hospital_grp plot data works

                           PHS metadata ID                          ID
    1 c698f450-eeed-41a0-88f7-c1e40a568acc Arran War Memorial Hospital
    2 c698f450-eeed-41a0-88f7-c1e40a568acc Arran War Memorial Hospital
    3 c698f450-eeed-41a0-88f7-c1e40a568acc Arran War Memorial Hospital
    4 c698f450-eeed-41a0-88f7-c1e40a568acc Arran War Memorial Hospital
                     HospitalName AddressLine1  AddressLine2 AddressLine2QF
    1 Arran War Memorial Hospital      Lamlash Isle of Arran           <NA>
    2 Arran War Memorial Hospital      Lamlash Isle of Arran           <NA>
    3 Arran War Memorial Hospital      Lamlash Isle of Arran           <NA>
    4 Arran War Memorial Hospital      Lamlash Isle of Arran           <NA>
      AddressLine3 AddressLine3QF AddressLine4 AddressLine4QF Postcode HealthBoard
    1         <NA>              z         <NA>              z  KA278LF   S08000015
    2         <NA>              z         <NA>              z  KA278LF   S08000015
    3         <NA>              z         <NA>              z  KA278LF   S08000015
    4         <NA>              z         <NA>              z  KA278LF   S08000015
           HSCP CouncilArea IntermediateZone  DataZone
    1 S37000020   S12000021        S02002097 S01011176
    2 S37000020   S12000021        S02002097 S01011176
    3 S37000020   S12000021        S02002097 S01011176
    4 S37000020   S12000021        S02002097 S01011176
                               PHS data ID FinancialYear FinancialYearQF        HB
    1 d719af13-5fb3-430f-810e-ab3360961107       2022/23            <NA> S08000015
    2 d719af13-5fb3-430f-810e-ab3360961107       2022/23            <NA> S08000015
    3 d719af13-5fb3-430f-810e-ab3360961107       2023/24               p S08000015
    4 d719af13-5fb3-430f-810e-ab3360961107       2023/24               p S08000015
      HBQF LocationQF Specialty SpecialtyQF    SpecialtyName SpecialtyNameQF
    1 <NA>       <NA>        A1        <NA> General Medicine            <NA>
    2 <NA>       <NA>      <NA>           z  All Specialties               d
    3 <NA>       <NA>        A1        <NA> General Medicine            <NA>
    4 <NA>       <NA>      <NA>           z  All Specialties               d
      SpecialtyGrouping SpecialtyGroupingQF AverageAvailableStaffedBeds
    1  Medical Grouping                <NA>                           0
    2   All Specialties                   d                           9
    3  Medical Grouping                <NA>                           0
    4   All Specialties                   d                          10
      AverageAvailableStaffedBedsQF PercentageOccupancy PercentageOccupancyQF
    1                          <NA>                  NA                     z
    2                          <NA>                77.4                  <NA>
    3                          <NA>                  NA                     z
    4                          <NA>                68.5                  <NA>
      AllStaffedBeds AllStaffedBedsQF AverageOccupiedBeds AverageOccupiedBedsQF
    1              0             <NA>            0.000000                  <NA>
    2           3263             <NA>            6.920548                  <NA>
    3              0             <NA>            0.000000                  <NA>
    4           3631             <NA>            6.797814                  <NA>
      TotalOccupiedBeds TotalOccupiedBedsQF             HBName
    1                 0                <NA> Ayrshire and Arran
    2              2526                <NA> Ayrshire and Arran
    3                 0                <NA> Ayrshire and Arran
    4              2488                <NA> Ayrshire and Arran

---

                               ID FinancialYear                HospitalName
    1 Arran War Memorial Hospital       2022/23 Arran War Memorial Hospital
    2 Arran War Memorial Hospital       2022/23 Arran War Memorial Hospital
    3 Arran War Memorial Hospital       2023/24 Arran War Memorial Hospital
    4 Arran War Memorial Hospital       2023/24 Arran War Memorial Hospital
        SpecialtyName PercentageOccupancy                                    name
    1 All Specialties                77.4 Annual number of available staffed beds
    2 All Specialties                77.4          Annual number of occupied beds
    3 All Specialties                68.5 Annual number of available staffed beds
    4 All Specialties                68.5          Annual number of occupied beds
      value
    1  3263
    2  2526
    3  3631
    4  2488
                                                                                                                                                                                        text
    1 Financial year: 2022/23\nHospital: Arran War Memorial Hospital\nSpecialty: All Specialties\nAnnual number of available staffed beds: 3263\nPercentage of daily occupancy of beds: 77.4
    2          Financial year: 2022/23\nHospital: Arran War Memorial Hospital\nSpecialty: All Specialties\nAnnual number of occupied beds: 2526\nPercentage of daily occupancy of beds: 77.4
    3 Financial year: 2023/24\nHospital: Arran War Memorial Hospital\nSpecialty: All Specialties\nAnnual number of available staffed beds: 3631\nPercentage of daily occupancy of beds: 68.5
    4          Financial year: 2023/24\nHospital: Arran War Memorial Hospital\nSpecialty: All Specialties\nAnnual number of occupied beds: 2488\nPercentage of daily occupancy of beds: 68.5

---

                               ID FinancialYear                HospitalName
    1 Arran War Memorial Hospital       2022/23 Arran War Memorial Hospital
    2 Arran War Memorial Hospital       2022/23 Arran War Memorial Hospital
    3 Arran War Memorial Hospital       2023/24 Arran War Memorial Hospital
    4 Arran War Memorial Hospital       2023/24 Arran War Memorial Hospital
        SpecialtyName PercentageOccupancy
    1 All Specialties                77.4
    2 All Specialties                77.4
    3 All Specialties                68.5
    4 All Specialties                68.5
                                                name     value
    1 Daily average number of available staffed beds  9.000000
    2          Daily average number of occupied beds  6.920548
    3 Daily average number of available staffed beds 10.000000
    4          Daily average number of occupied beds  6.797814
                                                                                                                                                                                                  text
    1       Financial year: 2022/23\nHospital: Arran War Memorial Hospital\nSpecialty: All Specialties\nDaily average number of available staffed beds: 9\nPercentage of daily occupancy of beds: 77.4
    2 Financial year: 2022/23\nHospital: Arran War Memorial Hospital\nSpecialty: All Specialties\nDaily average number of occupied beds: 6.92054794520548\nPercentage of daily occupancy of beds: 77.4
    3      Financial year: 2023/24\nHospital: Arran War Memorial Hospital\nSpecialty: All Specialties\nDaily average number of available staffed beds: 10\nPercentage of daily occupancy of beds: 68.5
    4 Financial year: 2023/24\nHospital: Arran War Memorial Hospital\nSpecialty: All Specialties\nDaily average number of occupied beds: 6.79781420765027\nPercentage of daily occupancy of beds: 68.5

