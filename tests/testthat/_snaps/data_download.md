# process_gp_meta works

                                 datasetID    ID
    1 b3b126d3-3b0c-4856-b348-0b37f8286367 10002
    2 b3b126d3-3b0c-4856-b348-0b37f8286367 10017
    3 b3b126d3-3b0c-4856-b348-0b37f8286367 10036
    4 b3b126d3-3b0c-4856-b348-0b37f8286367 10106
    5 b3b126d3-3b0c-4856-b348-0b37f8286367 10125
    6 b3b126d3-3b0c-4856-b348-0b37f8286367 10182
                                      GPPracticeName PracticeListSize
    1                        Muirhead Medical Centre             8251
    2                              The Blue Practice             7272
    3 Aberfeldy And Kinloch Rannoch Medical Practice             4843
    4                            Grove Health Centre             6429
    5                            Alyth Health Centre             4590
    6                    Arbroath Medical Centre LLP             9901
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

                                 datasetID       Date    ID    Sex AllAges Ages0to4
    1 3306ab5a-cd22-494a-be76-ee6753cef92d 2024-04-01 10002   Male    4056      186
    2 3306ab5a-cd22-494a-be76-ee6753cef92d 2024-04-01 10002 Female    4195      176
    3 3306ab5a-cd22-494a-be76-ee6753cef92d 2024-04-01 10002    All    8251      362
    4 3306ab5a-cd22-494a-be76-ee6753cef92d 2024-04-01 10017   Male    3652      134
    5 3306ab5a-cd22-494a-be76-ee6753cef92d 2024-04-01 10017 Female    3620      115
    6 3306ab5a-cd22-494a-be76-ee6753cef92d 2024-04-01 10017    All    7272      249
      Ages5to14 Ages15to24 Ages25to44 Ages45to64 Ages65to74 Ages75to84 Ages85plus
    1       456        415        945       1160        494        312         88
    2       397        425        961       1256        535        344        101
    3       853        840       1906       2416       1029        656        189
    4       393        512        780        991        422        315        105
    5       351        457        777        959        456        334        171
    6       744        969       1557       1950        878        649        276

# process_hopsital_meta works

                                 datasetID    ID
    1 c698f450-eeed-41a0-88f7-c1e40a568acc A101H
    2 c698f450-eeed-41a0-88f7-c1e40a568acc A103H
    3 c698f450-eeed-41a0-88f7-c1e40a568acc A110H
    4 c698f450-eeed-41a0-88f7-c1e40a568acc A111H
    5 c698f450-eeed-41a0-88f7-c1e40a568acc A114H
    6 c698f450-eeed-41a0-88f7-c1e40a568acc A201H
                                                   HospitalName       AddressLine1
    1                               Arran War Memorial Hospital            Lamlash
    2                                 Ayrshire Central Hospital    Kilwinning Road
    3                                    Lady Margaret Hospital         College St
    4                            University Hospital Crosshouse    Kilmarnock Road
    5 Warrix Avenue Mental Health Community Rehabilitation Unit      Warrix Avenue
    6                                            Ailsa Hospital Dalmellington Road
       AddressLine2 AddressLine2QF    AddressLine3 AddressLine3QF AddressLine4
    1 Isle of Arran           <NA>            <NA>              z         <NA>
    2        Irvine           <NA>            <NA>              z         <NA>
    3      Millport           <NA> Isle of Cumbrae           <NA>         <NA>
    4    Kilmarnock           <NA>        Ayrshire           <NA>         <NA>
    5        Irvine           <NA>            <NA>              z         <NA>
    6           Ayr           <NA>            <NA>              z         <NA>
      AddressLine4QF Postcode HealthBoard      HSCP CouncilArea IntermediateZone
    1              z  KA278LF   S08000015 S37000020   S12000021        S02002097
    2              z  KA128SS   S08000015 S37000020   S12000021        S02002105
    3              z  KA280HF   S08000015 S37000020   S12000021        S02002128
    4              z  KA2 0BE   S08000015 S37000008   S12000008        S02001504
    5              z  KA120DP   S08000015 S37000020   S12000021        S02002109
    6              z  KA6 6AB   S08000015 S37000027   S12000028        S02002333
       DataZone             HBName
    1 S01011176 Ayrshire and Arran
    2 S01011213 Ayrshire and Arran
    3 S01011328 Ayrshire and Arran
    4 S01008027 Ayrshire and Arran
    5 S01011229 Ayrshire and Arran
    6 S01012444 Ayrshire and Arran

# process_hopsital_data works

                                 datasetID FinancialYear FinancialYearQF        HB
    1 d719af13-5fb3-430f-810e-ab3360961107       2022/23            <NA> S08000015
    2 d719af13-5fb3-430f-810e-ab3360961107       2022/23            <NA> S08000015
    3 d719af13-5fb3-430f-810e-ab3360961107       2022/23            <NA> S08000015
    4 d719af13-5fb3-430f-810e-ab3360961107       2022/23            <NA> S08000015
    5 d719af13-5fb3-430f-810e-ab3360961107       2022/23            <NA> S08000015
    6 d719af13-5fb3-430f-810e-ab3360961107       2022/23            <NA> S08000015
      HBQF    ID LocationQF Specialty SpecialtyQF                  SpecialtyName
    1 <NA> A101H       <NA>        A1        <NA>               General Medicine
    2 <NA> A101H       <NA>        A2        <NA>                     Cardiology
    3 <NA> A101H       <NA>        AB        <NA>             Geriatric Medicine
    4 <NA> A101H       <NA>        C1        <NA>                General Surgery
    5 <NA> A101H       <NA>        C8        <NA> Trauma and Orthopaedic Surgery
    6 <NA> A101H       <NA>       E12        <NA>       GP Other than Obstetrics
      SpecialtyNameQF  SpecialtyGrouping SpecialtyGroupingQF
    1            <NA>   Medical Grouping                <NA>
    2            <NA>   Medical Grouping                <NA>
    3            <NA>   Medical Grouping                <NA>
    4            <NA>   Surgery Grouping                <NA>
    5            <NA>   Surgery Grouping                <NA>
    6            <NA> Community Grouping                <NA>
      AverageAvailableStaffedBeds PercentageOccupancy PercentageOccupancyQF
    1                           0                  NA                     z
    2                           0                  NA                     z
    3                           0                  NA                     z
    4                           0                  NA                     z
    5                           0                  NA                     z
    6                           9                77.4                  <NA>
      AllStaffedBeds AverageOccupiedBeds TotalOccupiedBeds
    1              0            0.000000                 0
    2              0            0.000000                 0
    3              0            0.000000                 0
    4              0            0.000000                 0
    5              0            0.000000                 0
    6           3263            6.920548              2526

