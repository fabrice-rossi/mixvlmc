# print works as expected

    VLMC context tree on 0, 1, 2 
     cutoff: 3.768 (quantile: 0.0231)
     Number of contexts: 3 
     Maximum context length: 1 
     Selected by BIC (967.7161) with likelihood function "truncated" (-473.2203)

---

    VLMC context tree on 0, 1, 2 
     cutoff: 3.768 (quantile: 0.0231)
     Number of contexts: 3 
     Maximum context length: 1 
     Selected by AIC (937.348) with likelihood function "truncated" (-473.2203)

# summary works as expected

    VLMC tune results
    
    Best VLMC selected by BIC (967.7161) with likelihood function "truncated" (-473.2203)
    VLMC context tree on 0, 1, 2 
     cutoff: 3.768 (quantile: 0.0231)
     Number of contexts: 3 
     Maximum context length: 1 
    
    Pruning results
        cutoff        alpha depth nb_contexts loglikelihood       AIC       BIC
      1.553652 2.114743e-01     8          48     -413.7037 1019.4073 1422.4613
      1.566520 2.087705e-01     8          44     -416.7154 1009.4308 1378.8969
      1.601899 2.015135e-01     8          42     -418.8484 1005.6968 1358.3691
      1.663405 1.894927e-01     8          42     -420.4757 1008.9514 1361.6236
      1.731122 1.770856e-01     8          41     -422.3991 1008.7982 1353.0735
      1.771785 1.700292e-01     8          39     -424.3660 1004.7319 1332.2132
      1.806683 1.641979e-01     8          38     -426.1471 1004.2942 1323.3786
      1.840646 1.587148e-01     8          36     -428.0099 1000.0199 1302.3104
      1.864315 1.550024e-01     5          28     -433.4319  978.8638 1213.9786
      1.910771 1.479662e-01     5          25     -435.8113  971.6227 1181.5466
      1.955919 1.414344e-01     5          23     -437.8881  967.7762 1160.9062
      2.023933 1.321348e-01     5          21     -440.5870  965.1740 1141.5101
      2.198586 1.109600e-01     5          19     -444.1560  964.3120 1123.8542
      2.431783 8.788004e-02     4          15     -447.8067  955.6135 1081.5678
      2.632452 7.190195e-02     4          13     -451.1809  954.3618 1063.5223
      2.794372 6.115324e-02     4          11     -454.1745  952.3490 1044.7156
      2.951746 5.224842e-02     4           8     -458.9591  949.9182 1017.0939
      3.141665 4.321078e-02     3           5     -461.6390  943.2780  985.2628
      3.767760 2.310376e-02     1           3     -465.2626  942.5252  967.7161
      9.203328 1.007037e-04     1           3     -469.1176  950.2351  975.4260
     20.859681 8.724800e-10     1           2     -489.2067  986.4135 1003.2074
     24.056504 3.567739e-11     0           1     -511.2550 1026.5100 1034.9070

---

    VLMC tune results
    
    Best VLMC selected by AIC (960.4407) with likelihood function "specific" (-473.2203)
    VLMC context tree on 0, 1, 2 
     cutoff: 3.768 (quantile: 0.0231)
     Number of contexts: 3 
     Maximum context length: 1 
    
    Pruning results
        cutoff        alpha depth nb_contexts loglikelihood       AIC       BIC
      1.000000 3.678794e-01    10         118     -349.3750 1190.7499 2227.5435
      1.015516 3.622156e-01    10         117     -350.3844 1188.7688 2217.1331
      1.027283 3.579784e-01    10         111     -355.6258 1175.2516 2153.0407
      1.035378 3.550922e-01    10         108     -357.4687 1166.9375 2119.4389
      1.039644 3.535805e-01    10         107     -358.5066 1165.0131 2109.0853
      1.059577 3.466024e-01    10         106     -359.5480 1163.0961 2098.7391
      1.092080 3.355178e-01    10         105     -360.6260 1161.2520 2088.4658
      1.112773 3.286463e-01    10         104     -361.7324 1159.4647 2078.2493
      1.123115 3.252650e-01    10         103     -362.8516 1157.7032 2068.0586
      1.133206 3.219993e-01    10         101     -364.2791 1152.5583 2046.0552
      1.151443 3.161802e-01    10          94     -369.1617 1134.3235 1968.8159
      1.187675 3.049293e-01    10          94     -370.3253 1136.6506 1971.1430
      1.214332 2.969084e-01    10          93     -371.5376 1135.0752 1961.1384
      1.231640 2.918137e-01    10          88     -376.6371 1125.2742 1909.1913
      1.300578 2.723743e-01    10          87     -377.8842 1123.7684 1899.2563
      1.356487 2.575640e-01    10          85     -379.9250 1119.8501 1878.4795
      1.358784 2.569731e-01    10          84     -381.2816 1118.5633 1868.7635
      1.373575 2.532001e-01    10          83     -382.6426 1117.2852 1859.0562
      1.396566 2.474452e-01     8          66     -394.6718 1069.3437 1659.3888
      1.422575 2.410924e-01     8          65     -396.9073 1069.8146 1651.4306
      1.445170 2.357060e-01     8          60     -400.1991 1056.3983 1595.8681
      1.455263 2.333389e-01     8          59     -401.7269 1055.4538 1586.4944
      1.460912 2.320246e-01     8          57     -403.9964 1051.9927 1566.1749
      1.465595 2.309405e-01     8          54     -405.8933 1043.7867 1532.6812
      1.485898 2.262990e-01     8          52     -407.5304 1039.0607 1511.0968
      1.518211 2.191036e-01     8          50     -409.6354 1035.2708 1490.4485
      1.538883 2.146207e-01     8          49     -412.1583 1036.3167 1483.0651
      1.550745 2.120899e-01     8          48     -413.7037 1035.4073 1473.7266
      1.566520 2.087705e-01     8          44     -416.7154 1025.4308 1430.0332
      1.601899 2.015135e-01     8          42     -418.8484 1021.6968 1409.4408
      1.663405 1.894927e-01     8          42     -420.4757 1024.9514 1412.6953
      1.731122 1.770856e-01     8          41     -422.3991 1024.7982 1404.1130
      1.771785 1.700292e-01     8          39     -424.3660 1020.7319 1383.1882
      1.806683 1.641979e-01     8          38     -426.1471 1020.2942 1374.3213
      1.840646 1.587148e-01     8          36     -428.0099 1016.0199 1353.1885
      1.864315 1.550024e-01     5          28     -435.5135  993.0270 1250.1181
      1.910771 1.479662e-01     5          25     -438.3894  986.7787 1218.5822
      1.955919 1.414344e-01     5          23     -440.4661  982.9322 1197.8772
      2.023933 1.321348e-01     5          21     -443.1651  980.3301 1178.4167
      2.198586 1.109600e-01     5          19     -446.7340  979.4681 1160.6962
      2.431783 8.788004e-02     4          15     -452.0986  972.1971 1115.4938
      2.632452 7.190195e-02     4          13     -455.4727  970.9455 1097.3837
      2.794372 6.115324e-02     4          11     -458.4663  968.9327 1078.5125
      2.951746 5.224842e-02     4           8     -463.2509  966.5019 1050.7940
      3.141665 4.321078e-02     3           5     -468.6039  963.2077 1017.9976
      3.767760 2.310376e-02     1           3     -473.2203  960.4407  989.9429
      9.203328 1.007037e-04     1           3     -477.5920  969.1840  998.6863
     20.859681 8.724800e-10     1           2     -496.9670 1003.9341 1025.0071
     24.056504 3.567739e-11     0           1     -520.4248 1044.8496 1053.2788

# tune_vlmc verbosity is adequate

    Fitting a vlmc with max_depth= 2 and cutoff= 1.553652 
    Max depth reached, increasing it to 4 
    Max depth reached, increasing it to 8 
    Max depth reached, increasing it to 16 
    Initial criterion = Inf 
    Improving criterion = 1422.461 likelihood = -413.7037 df = 96 nobs =  492 
    Pruning vlmc with cutoff = 1.56652 
    Improving criterion = 1378.897 likelihood = -416.7154 df = 88 nobs =  492 
    Pruning vlmc with cutoff = 1.601899 
    Improving criterion = 1358.369 likelihood = -418.8484 df = 84 nobs =  492 
    Pruning vlmc with cutoff = 1.663405 
    Pruning vlmc with cutoff = 1.731122 
    Improving criterion = 1353.073 likelihood = -422.3991 df = 82 nobs =  492 
    Pruning vlmc with cutoff = 1.771785 
    Improving criterion = 1332.213 likelihood = -424.366 df = 78 nobs =  492 
    Pruning vlmc with cutoff = 1.806683 
    Improving criterion = 1323.379 likelihood = -426.1471 df = 76 nobs =  492 
    Pruning vlmc with cutoff = 1.840646 
    Improving criterion = 1302.31 likelihood = -428.0099 df = 72 nobs =  492 
    Pruning vlmc with cutoff = 1.864315 
    Improving criterion = 1213.979 likelihood = -433.4319 df = 56 nobs =  492 
    Pruning vlmc with cutoff = 1.910771 
    Improving criterion = 1181.547 likelihood = -435.8113 df = 50 nobs =  492 
    Pruning vlmc with cutoff = 1.955919 
    Improving criterion = 1160.906 likelihood = -437.8881 df = 46 nobs =  492 
    Pruning vlmc with cutoff = 2.023933 
    Improving criterion = 1141.51 likelihood = -440.587 df = 42 nobs =  492 
    Pruning vlmc with cutoff = 2.198586 
    Improving criterion = 1123.854 likelihood = -444.156 df = 38 nobs =  492 
    Pruning vlmc with cutoff = 2.431783 
    Improving criterion = 1081.568 likelihood = -447.8067 df = 30 nobs =  492 
    Pruning vlmc with cutoff = 2.632452 
    Improving criterion = 1063.522 likelihood = -451.1809 df = 26 nobs =  492 
    Pruning vlmc with cutoff = 2.794372 
    Improving criterion = 1044.716 likelihood = -454.1745 df = 22 nobs =  492 
    Pruning vlmc with cutoff = 2.951746 
    Improving criterion = 1017.094 likelihood = -458.9591 df = 16 nobs =  492 
    Pruning vlmc with cutoff = 3.141665 
    Improving criterion = 985.2628 likelihood = -461.639 df = 10 nobs =  492 
    Pruning vlmc with cutoff = 3.76776 
    Improving criterion = 967.7161 likelihood = -465.2626 df = 6 nobs =  492 
    Pruning vlmc with cutoff = 9.203328 
    Pruning vlmc with cutoff = 20.85968 
    Pruning vlmc with cutoff = 24.0565 
    VLMC context tree on 0, 1, 2 
     cutoff: 3.768 (quantile: 0.0231)
     Number of contexts: 3 
     Maximum context length: 1 
     Selected by BIC (967.7161) with likelihood function "truncated" (-473.2203)

