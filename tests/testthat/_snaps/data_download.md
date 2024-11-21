# process_gp_meta works

         ID                                 GPPracticeName PracticeListSize
    1 10002                        Muirhead Medical Centre             8251
    2 10017                              The Blue Practice             7272
    3 10036 Aberfeldy And Kinloch Rannoch Medical Practice             4843
    4 10106                            Grove Health Centre             6429
    5 10125                            Alyth Health Centre             4590
    6 10182                    Arbroath Medical Centre LLP             9901
                                        AddressLine1          AddressLine2
    1                        Muirhead Medical Centre             Liff Road
    2                              The Blue Practice Crieff Medical Centre
    3 Aberfeldy And Kinloch Rannoch Medical Practice        Taybridge Road
    4                            Grove Health Centre       129 Dundee Road
    5                            Alyth Health Centre        New Alyth Road
    6                    Arbroath Medical Centre LLP          7 Hill Place
        AddressLine3 AddressLine4 Postcode   TelephoneNumber PracticeType
    1       Muirhead         <NA>  DD2 5NH 01382   580   264          17J
    2    King Street       Crieff  PH7 3SA 01764   652   283          17J
    3      Aberfeldy   Perthshire PH15 2BL 01887   820   366          17J
    4 Broughty Ferry       Dundee  DD5 1DU 01382   778   881          17J
    5          Alyth         <NA> PH11 8EQ 01828   632   317          17J
    6       Arbroath         <NA> DD11 1AD 01241   431   144          17J
      Dispensing        HB      HSCP  DataZone             GPCluster  HBName
    1         NA S08000030 S37000007 S01007129              Dundee 4 Tayside
    2         NA S08000030 S37000033 S01011870    Strathearn Cluster Tayside
    3         NA S08000030 S37000033 S01012014 North West Perthshire Tayside
    4         NA S08000030 S37000007 S01007758              Dundee 2 Tayside
    5         NA S08000030 S37000033 S01011975    Strathmore Cluster Tayside
    6         NA S08000030 S37000003 S01007187      Angus South East Tayside

# process_gp_data works

            Date    ID    Sex AllAges Ages0to4 Ages5to14 Ages15to24 Ages25to44
    1 2024-04-01 10002   Male    4056      186       456        415        945
    2 2024-04-01 10002 Female    4195      176       397        425        961
    3 2024-04-01 10002    All    8251      362       853        840       1906
    4 2024-04-01 10017   Male    3652      134       393        512        780
    5 2024-04-01 10017 Female    3620      115       351        457        777
    6 2024-04-01 10017    All    7272      249       744        969       1557
      Ages45to64 Ages65to74 Ages75to84 Ages85plus
    1       1160        494        312         88
    2       1256        535        344        101
    3       2416       1029        656        189
    4        991        422        315        105
    5        959        456        334        171
    6       1950        878        649        276

# process_hopsital_meta works

         ID                                              HospitalName
    1 A101H                               Arran War Memorial Hospital
    2 A103H                                 Ayrshire Central Hospital
    3 A110H                                    Lady Margaret Hospital
    4 A111H                            University Hospital Crosshouse
    5 A114H Warrix Avenue Mental Health Community Rehabilitation Unit
    6 A201H                                            Ailsa Hospital
            AddressLine1  AddressLine2 AddressLine2QF    AddressLine3
    1            Lamlash Isle of Arran           <NA>            <NA>
    2    Kilwinning Road        Irvine           <NA>            <NA>
    3         College St      Millport           <NA> Isle of Cumbrae
    4    Kilmarnock Road    Kilmarnock           <NA>        Ayrshire
    5      Warrix Avenue        Irvine           <NA>            <NA>
    6 Dalmellington Road           Ayr           <NA>            <NA>
      AddressLine3QF AddressLine4 AddressLine4QF Postcode HealthBoard      HSCP
    1              z         <NA>              z  KA278LF   S08000015 S37000020
    2              z         <NA>              z  KA128SS   S08000015 S37000020
    3           <NA>         <NA>              z  KA280HF   S08000015 S37000020
    4           <NA>         <NA>              z  KA2 0BE   S08000015 S37000008
    5              z         <NA>              z  KA120DP   S08000015 S37000020
    6              z         <NA>              z  KA6 6AB   S08000015 S37000027
      CouncilArea IntermediateZone  DataZone             HBName
    1   S12000021        S02002097 S01011176 Ayrshire and Arran
    2   S12000021        S02002105 S01011213 Ayrshire and Arran
    3   S12000021        S02002128 S01011328 Ayrshire and Arran
    4   S12000008        S02001504 S01008027 Ayrshire and Arran
    5   S12000021        S02002109 S01011229 Ayrshire and Arran
    6   S12000028        S02002333 S01012444 Ayrshire and Arran

# process_hopsital_data works

      FinancialYear FinancialYearQF        HB HBQF    ID LocationQF Specialty
    1       2022/23            <NA> S08000015 <NA> A101H       <NA>        A1
    2       2022/23            <NA> S08000015 <NA> A101H       <NA>        A2
    3       2022/23            <NA> S08000015 <NA> A101H       <NA>        AB
    4       2022/23            <NA> S08000015 <NA> A101H       <NA>        C1
    5       2022/23            <NA> S08000015 <NA> A101H       <NA>        C8
    6       2022/23            <NA> S08000015 <NA> A101H       <NA>       E12
      SpecialtyQF                  SpecialtyName SpecialtyNameQF  SpecialtyGrouping
    1        <NA>               General Medicine            <NA>   Medical Grouping
    2        <NA>                     Cardiology            <NA>   Medical Grouping
    3        <NA>             Geriatric Medicine            <NA>   Medical Grouping
    4        <NA>                General Surgery            <NA>   Surgery Grouping
    5        <NA> Trauma and Orthopaedic Surgery            <NA>   Surgery Grouping
    6        <NA>       GP Other than Obstetrics            <NA> Community Grouping
      SpecialtyGroupingQF AverageAvailableStaffedBeds PercentageOccupancy
    1                <NA>                           0                  NA
    2                <NA>                           0                  NA
    3                <NA>                           0                  NA
    4                <NA>                           0                  NA
    5                <NA>                           0                  NA
    6                <NA>                           9                77.4
      PercentageOccupancyQF AllStaffedBeds AverageOccupiedBeds TotalOccupiedBeds
    1                     z              0            0.000000                 0
    2                     z              0            0.000000                 0
    3                     z              0            0.000000                 0
    4                     z              0            0.000000                 0
    5                     z              0            0.000000                 0
    6                  <NA>           3263            6.920548              2526

