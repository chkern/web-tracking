*Preparation of background data
clear all
usespss "U:\respondi sinus daten\background_info\orig\profile data uni mannheim.sav"

rename pseudonym panelist_id
*no info
drop md_9010 md_2573
*drop -- sensitive information
drop md_1600-md_1605

foreach v of varlist md_1275 md_1748 md_2830 md_2860 {
rename `v' x`v'
}

foreach v of varlist md_9009 md_9008 md_9007 md_9006 md_9004 md_9000 md_2918 md_2917 md_2916 md_2915 md_2914 md_2912 md_2911 md_2910 md_2909 md_2908 md_2907 md_2906 md_2905 md_2904 md_2903 md_2902 md_2901 md_2899 md_2898 md_2897 md_2896 md_2895 md_2865 md_2864 md_2863 md_2862 md_2861 md_2859 md_2835 md_2834 md_2833 md_2832 md_2831 md_2829 md_2824 md_2810 md_2809 md_2806 md_2789 md_2788 md_2787 md_2786 md_2785 md_2784 md_2783 md_2569 md_2568 md_2561 md_2559 md_2558 md_2557 md_2556 md_2555 md_2554 md_2553 md_2552 md_2551 md_2550 md_2549 md_2548 md_2547 md_2546 md_2545 md_2544 md_2543 md_2542 md_2541 md_2540 md_2539 md_2538 md_2473 md_2417 md_2415 md_2414 md_2413 md_2412 md_2411 md_2410 md_2409 md_2392 md_2390 md_2388 md_2386 md_2380 md_2378 md_2377 md_2375 md_2374 md_2372 md_2371 md_2370 md_2367 md_2363 md_2362 md_2361 md_2359 md_2357 md_2356 md_2350 md_2348 md_2344 md_2342 md_2339 md_2328 md_2326 md_2324 md_2319 md_2318 md_2317 md_2311 md_2309 md_2307 md_2306 md_2304 md_2302 md_2249 md_2247 md_2245 md_2243 md_2242 md_2241 md_2240 md_2239 md_2189 md_2186 md_2176 md_2175 md_2174 md_2173 md_2172 md_2036 md_2034 md_2032 md_2030 md_2028 md_2026 md_2018 md_2016 md_2014 md_2012 md_2010 md_2007 md_2005 md_2003 md_1999 md_1997 md_1994 md_1986 md_1984 md_1980 md_1976 md_1966 md_1964 md_1962 md_1960 md_1954 md_1952 md_1950 md_1948 md_1946 md_1945 md_1944 md_1943 md_1942 md_1940 md_1937 md_1935 md_1933 md_1931 md_1930 md_1929 md_1928 md_1926 md_1924 md_1923 md_1922 md_1921 md_1920 md_1919 md_1918 md_1917 md_1916 md_1915 md_1914 md_1911 md_1910 md_1909 md_1908 md_1907 md_1906 md_1905 md_1904 md_1903 md_1902 md_1899 md_1897 md_1896 md_1895 md_1892 md_1891 md_1890 md_1889 md_1886 md_1885 md_1884 md_1883 md_1881 md_1880 md_1877 md_1876 md_1875 md_1874 md_1872 md_1871 md_1870 md_1869 md_1868 md_1867 md_1866 md_1747 md_1660 md_1659 md_1658 md_1657 md_1656 md_1655 md_1654 md_1653 md_1652 md_1651 md_1650 md_1649 md_1648 md_1647 md_1646 md_1637 md_1636 md_1635 md_1634 md_1621 md_1384 md_1382 md_1381 md_1380 md_1379 md_1378 md_1377 md_1364 md_1347 md_1345 md_1344 md_1343 md_1342 md_1341 md_1340 md_1339 md_1338 md_1337 md_1336 md_1335 md_1334 md_1333 md_1332 md_1331 md_1330 md_1329 md_1328 md_1327 md_1326 md_1324 md_1322 md_1320 md_1316 md_1315 md_1314 md_1313 md_1312 md_1274 md_1264 md_1248 md_1246 md_1245 md_1244 md_1243 md_1242 md_1241 md_1240 md_1239 md_1238 md_1237 md_1236 md_1223 md_1201 md_1198 md_1195 md_1191 md_1190 md_1185 md_1184 md_1181 md_1176 md_1175 md_1174 md_1172 md_1171 md_1000 md_0006 md_0004 md_0003 md_0002 md_0001 m_1438 m_1339 m_1338 m_1337 m_1336 m_1335 m_1155 m_1153 m_1152 m_1151 m_1150 m_1149 m_1148 m_1147 m_1146 m_1145 m_1114 m_1113 m_1112 m_1111 m_1110 m_1109 m_1108 m_1107 m_1106 m_1105 m_1099 m_1098 m_1097 m_1096 m_1095 m_1094 m_1093 m_1092 m_1091 m_1090 m_1089 m_1088 m_1087 m_1086 m_1085 m_1084 m_1083 m_1082 m_1081 m_1080 m_1079 m_1077 m_1076 m_1075 m_1074 m_1073 m_1072 m_1071 m_1070 m_1068 m_1066 m_1065 m_1064 m_1063 m_1062 m_1061 m_1060 m_1059 m_1058 m_1057 m_1056 m_1055 m_1054 m_1053 m_1052 m_1051 m_1050 m_1049 m_1048 m_1047 m_1046 m_1045 m_1044 m_1043 m_1042 m_1041 m_1040 m_1039 m_1038 m_1037 m_1036 m_1035 m_1034 m_1033 m_1032 m_1031 m_1030 m_1029 m_1028 m_1027 m_1026 m_1025 m_1024 m_1023 m_1022 m_1021 m_1020 m_1019 m_1018 m_1017 m_1015 m_1014 m_1013 m_1012 m_1011 m_1010 m_1009 m_1008 m_1006 m_1005 m_1004 m_1002 m_1001 m_1000 {
recode `v' (.a=.)
}

rename xmd_1275 md_1275 
rename xmd_1748 md_1748
rename xmd_2830 md_2830
rename xmd_2860 md_2860

*Should we really make (0=.) changes? If machine learning, it doesn't matter and may actually contain information

*recode m_1000 (0=.)
*recode m_1001 (0=.)
*recode m_1002 (0=.)
recode m_1004 (0=2) if m_1005==4|m_1005==19
recode m_1004 (.=2) if m_1005==4|m_1005==19
*recode m_1005 (0=.)
encode md_1275, gen(xmd_1275)
recode xmd_1275 (1=.)
drop md_1275
rename xmd md_1275
*recode m_1006 (0=.)
*foreach v of varlist m_1062-m_1066 {
*recode `v' (0=.)
*}
replace m_1062 =10 if m_1063>=1 & m_1063<99 & m_1062==99 
replace m_1062 =10 if m_1064>=1 & m_1064<99 & m_1062==. 
replace m_1062 =10 if m_1066!=. & m_1062==. 
replace m_1062 =10 if m_1066==1 & m_1062==99 

label def m_1062 10 "has car, but dont know", add

*recode md_0006 (0=.)
recode md_1174 (0=98) if md_1172==1
recode md_1174 (.=98) if md_1172==1
*recode md_1198 (0=.)
*recode md_1223 (0=.)
*recode md_1274 (0=.)
replace md_1275 =. if md_1274<97 
replace md_1275 =. if md_1274==99
*recode md_1327 (0=.)
*recode md_1364 (0=.)
*recode md_1634 (0=.) if md_1635==0
recode md_1634 (0=97) if md_1635!=0 & md_1635!=.
recode md_1635 (0=.) if md_1634==.
replace md_1635 =97 if md_1635==0
replace md_1635 =98 if md_1634==98
replace md_1634 =98 if md_1635==98
*recode md_1636 (0=.)
*recode md_1637 (0=.)
*recode md_1660 (0=.)


encode md_1748, gen(xmd_1748)
recode xmd_1748 (1=.)
drop md_1748
rename xmd md_1748
*recode md_1747 (0=.)
replace md_1748 =. if md_1747<97 
replace md_1747 =97 if md_1748!=. & md_1747==98 
replace md_1747 =97 if md_1748!=. & md_1747==99 

*recode md_2173 (0=.) if md_2172==98
*recode md_2173 (0=.)
*recode md_2174 (0=.) if md_2172==98
*recode md_2174 (0=.)
*recode md_2175 (0=.) if md_2172==98
*recode md_2175 (0=.)
*recode md_2176 (0=.) if md_2172==98
*recode md_2176 (0=.)
*recode md_2186 (0=.)
*recode md_2357 (0=.)
*recode md_2785 (0=.)
replace md_2786 =. if md_2785==98 & md_2786==99

encode md_2830, gen(xmd_2830)
recode xmd_2830 (1=.)
drop md_2830
rename xmd md_2830
*recode md_2824 (0=.)
replace md_2830 =. if md_2824<97 
replace md_2824 =97 if md_2830!=. & md_2824==99 
replace md_1747 =97 if md_1748!=. & md_1747==99 

encode md_2860, gen(xmd_2860)
recode xmd_2860 (1=.)
drop md_2860
rename xmd md_2860
*recode md_2859 (0=.)
replace md_2860 =. if md_2859<97 
replace md_2859 =97 if md_2860!=. & md_2859==98 
*recode md_2862 (0=.)
*recode md_2863 (0=.)
*recode md_2864 (0=.)
*recode md_2865 (0=.)
*recode md_9006 (0=.)
*recode md_1000 (0=.)
*recode md_1190 (0=.)
*recode md_1191 (0=.)

