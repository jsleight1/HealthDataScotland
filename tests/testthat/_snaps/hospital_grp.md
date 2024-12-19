# hospital_grp plot data works

                           PHS metadata ID    ID                HospitalName
    1 c698f450-eeed-41a0-88f7-c1e40a568acc A101H Arran War Memorial Hospital
    2 c698f450-eeed-41a0-88f7-c1e40a568acc A101H Arran War Memorial Hospital
    3 c698f450-eeed-41a0-88f7-c1e40a568acc A101H Arran War Memorial Hospital
    4 c698f450-eeed-41a0-88f7-c1e40a568acc A101H Arran War Memorial Hospital
      AddressLine1  AddressLine2 AddressLine2QF AddressLine3 AddressLine3QF
    1      Lamlash Isle of Arran           <NA>         <NA>              z
    2      Lamlash Isle of Arran           <NA>         <NA>              z
    3      Lamlash Isle of Arran           <NA>         <NA>              z
    4      Lamlash Isle of Arran           <NA>         <NA>              z
      AddressLine4 AddressLine4QF Postcode HealthBoard      HSCP CouncilArea
    1         <NA>              z  KA278LF   S08000015 S37000020   S12000021
    2         <NA>              z  KA278LF   S08000015 S37000020   S12000021
    3         <NA>              z  KA278LF   S08000015 S37000020   S12000021
    4         <NA>              z  KA278LF   S08000015 S37000020   S12000021
      IntermediateZone  DataZone                          PHS data ID FinancialYear
    1        S02002097 S01011176 d719af13-5fb3-430f-810e-ab3360961107       2022/23
    2        S02002097 S01011176 d719af13-5fb3-430f-810e-ab3360961107       2022/23
    3        S02002097 S01011176 d719af13-5fb3-430f-810e-ab3360961107       2023/24
    4        S02002097 S01011176 d719af13-5fb3-430f-810e-ab3360961107       2023/24
      FinancialYearQF        HB HBQF LocationQF Specialty SpecialtyQF
    1            <NA> S08000015 <NA>       <NA>        A1        <NA>
    2            <NA> S08000015 <NA>       <NA>      <NA>           z
    3               p S08000015 <NA>       <NA>        A1        <NA>
    4               p S08000015 <NA>       <NA>      <NA>           z
         SpecialtyName SpecialtyNameQF SpecialtyGrouping SpecialtyGroupingQF
    1 General Medicine            <NA>  Medical Grouping                <NA>
    2  All Specialties               d   All Specialties                   d
    3 General Medicine            <NA>  Medical Grouping                <NA>
    4  All Specialties               d   All Specialties                   d
      AverageAvailableStaffedBeds AverageAvailableStaffedBedsQF PercentageOccupancy
    1                           0                          <NA>                  NA
    2                           9                          <NA>                77.4
    3                           0                          <NA>                  NA
    4                          10                          <NA>                68.5
      PercentageOccupancyQF AllStaffedBeds AllStaffedBedsQF AverageOccupiedBeds
    1                     z              0             <NA>            0.000000
    2                  <NA>           3263             <NA>            6.920548
    3                     z              0             <NA>            0.000000
    4                  <NA>           3631             <NA>            6.797814
      AverageOccupiedBedsQF TotalOccupiedBeds TotalOccupiedBedsQF
    1                  <NA>                 0                <NA>
    2                  <NA>              2526                <NA>
    3                  <NA>                 0                <NA>
    4                  <NA>              2488                <NA>
                  HBName
    1 Ayrshire and Arran
    2 Ayrshire and Arran
    3 Ayrshire and Arran
    4 Ayrshire and Arran

---

         ID FinancialYear                HospitalName   SpecialtyName
    1 A101H       2022/23 Arran War Memorial Hospital All Specialties
    2 A101H       2022/23 Arran War Memorial Hospital All Specialties
    3 A101H       2023/24 Arran War Memorial Hospital All Specialties
    4 A101H       2023/24 Arran War Memorial Hospital All Specialties
      PercentageOccupancy                                    name value
    1                77.4 Annual number of available staffed beds  3263
    2                77.4          Annual number of occupied beds  2526
    3                68.5 Annual number of available staffed beds  3631
    4                68.5          Annual number of occupied beds  2488
                                                                                                                                                                                                                 text
    1 Financial year: 2022/23\nHospital id: A101H\nHospital name: Arran War Memorial Hospital\nSpecialty: All Specialties\nAnnual number of available staffed beds: 3263\nPercentage of daily occupancy of beds: 77.4
    2          Financial year: 2022/23\nHospital id: A101H\nHospital name: Arran War Memorial Hospital\nSpecialty: All Specialties\nAnnual number of occupied beds: 2526\nPercentage of daily occupancy of beds: 77.4
    3 Financial year: 2023/24\nHospital id: A101H\nHospital name: Arran War Memorial Hospital\nSpecialty: All Specialties\nAnnual number of available staffed beds: 3631\nPercentage of daily occupancy of beds: 68.5
    4          Financial year: 2023/24\nHospital id: A101H\nHospital name: Arran War Memorial Hospital\nSpecialty: All Specialties\nAnnual number of occupied beds: 2488\nPercentage of daily occupancy of beds: 68.5

---

         ID FinancialYear                HospitalName   SpecialtyName
    1 A101H       2022/23 Arran War Memorial Hospital All Specialties
    2 A101H       2022/23 Arran War Memorial Hospital All Specialties
    3 A101H       2023/24 Arran War Memorial Hospital All Specialties
    4 A101H       2023/24 Arran War Memorial Hospital All Specialties
      PercentageOccupancy                                           name     value
    1                77.4 Daily average number of available staffed beds  9.000000
    2                77.4          Daily average number of occupied beds  6.920548
    3                68.5 Daily average number of available staffed beds 10.000000
    4                68.5          Daily average number of occupied beds  6.797814
                                                                                                                                                                                                                           text
    1       Financial year: 2022/23\nHospital id: A101H\nHospital name: Arran War Memorial Hospital\nSpecialty: All Specialties\nDaily average number of available staffed beds: 9\nPercentage of daily occupancy of beds: 77.4
    2 Financial year: 2022/23\nHospital id: A101H\nHospital name: Arran War Memorial Hospital\nSpecialty: All Specialties\nDaily average number of occupied beds: 6.92054794520548\nPercentage of daily occupancy of beds: 77.4
    3      Financial year: 2023/24\nHospital id: A101H\nHospital name: Arran War Memorial Hospital\nSpecialty: All Specialties\nDaily average number of available staffed beds: 10\nPercentage of daily occupancy of beds: 68.5
    4 Financial year: 2023/24\nHospital id: A101H\nHospital name: Arran War Memorial Hospital\nSpecialty: All Specialties\nDaily average number of occupied beds: 6.79781420765027\nPercentage of daily occupancy of beds: 68.5

