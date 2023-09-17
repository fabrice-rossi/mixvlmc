# print works as expected

    VLMC context tree on 0, 1, 2 
     cutoff: 8.715 (quantile: 0.000164)
     Number of contexts: 3 
     Maximum context length: 1 
     Selected by BIC (990.3778) with likelihood function "extended" (-476.5451)

---

    VLMC context tree on 0, 1, 2 
     cutoff: 3.379 (quantile: 0.03409)
     Number of contexts: 3 
     Maximum context length: 1 
     Selected by AIC (954.5098) with likelihood function "truncated" (-472.6002)

# summary works as expected

    VLMC tune results
    
    Best VLMC selected by BIC (990.3778) with likelihood function "extended" (-476.5451)
    VLMC context tree on 0, 1, 2 
     cutoff: 8.715 (quantile: 0.000164)
     Number of contexts: 3 
     Maximum context length: 1 
    
    Pruning results
        cutoff        alpha depth nb_contexts loglikelihood       AIC       BIC
      1.553652 2.114743e-01     8          56     -412.1827 1072.3654 1594.9768
      1.566520 2.087705e-01     8          52     -415.2330 1062.4660 1551.3605
      1.595110 2.028862e-01     8          50     -417.3660 1054.7320 1518.3389
      1.617948 1.983051e-01     8          49     -418.9795 1053.9591 1509.1367
      1.629846 1.959597e-01     8          49     -420.6019 1053.2038 1499.9523
      1.638429 1.942851e-01     8          48     -422.2393 1052.4785 1490.7978
      1.659070 1.903160e-01     8          46     -424.5283 1049.0567 1470.5175
      1.685882 1.852809e-01     8          44     -426.7619 1045.5238 1450.1261
      1.727358 1.777533e-01     8          42     -429.7192 1039.4385 1418.7532
      1.771785 1.700292e-01     8          39     -432.1259 1032.2518 1386.2789
      1.806683 1.641979e-01     8          38     -433.9070 1031.8141 1377.4119
      1.833370 1.598738e-01     8          36     -435.8403 1027.6806 1356.4200
      1.841439 1.585891e-01     8          35     -437.6745 1027.3489 1347.6591
      1.906938 1.485344e-01     7          27     -443.0233 1006.0466 1258.9231
      1.968433 1.396755e-01     5          23     -445.9947  991.9895 1202.7199
      2.079931 1.249389e-01     5          21     -448.6321  989.2642 1183.1362
      2.212651 1.094102e-01     5          19     -451.5385  983.0770 1151.6613
      2.256204 1.047474e-01     5          17     -453.9017  979.8034 1131.5293
      2.303889 9.986969e-02     5          14     -457.7522  975.5045 1101.9427
      2.552296 7.790260e-02     4          10     -461.3788  966.7576 1059.4790
      2.834259 5.876203e-02     4           8     -464.4293  964.8586 1040.7216
      2.880310 5.611737e-02     3           5     -469.2545  962.5089 1013.0842
      3.378896 3.408506e-02     1           3     -472.6002  961.2005  994.9173
      8.715379 1.640434e-04     1           3     -476.5451  965.0901  990.3778
     21.194476 6.242464e-10     1           2     -495.8001  999.6002 1016.4586
     25.464063 8.731682e-12     0           1     -519.1294 1042.2587 1050.6880

---

    VLMC tune results
    
    Best VLMC selected by AIC (957.1793) with likelihood function "specific" (-472.6002)
    VLMC context tree on 0, 1, 2 
     cutoff: 3.379 (quantile: 0.03409)
     Number of contexts: 3 
     Maximum context length: 1 
    
    Pruning results
        cutoff        alpha depth nb_contexts loglikelihood       AIC       BIC
      1.000000 3.678794e-01    10         112     -352.0230 1172.0461 2158.2644
      1.015516 3.622156e-01    10         111     -353.0325 1170.0649 2147.8540
      1.027283 3.579784e-01    10         107     -356.9642 1161.9283 2106.0005
      1.035378 3.550922e-01    10         104     -358.8625 1153.7251 2072.5096
      1.039644 3.535805e-01    10         103     -359.9003 1151.8007 2062.1560
      1.072543 3.421375e-01    10         102     -360.9418 1149.8836 2051.8098
      1.115718 3.276801e-01    10         102     -362.0464 1152.0927 2054.0188
      1.133206 3.219993e-01    10         100     -363.4503 1146.9007 2031.9684
      1.175289 3.087298e-01    10          96     -367.3507 1138.7014 1990.0522
      1.214332 2.969084e-01    10          95     -368.5630 1137.1259 1980.0475
      1.227189 2.931154e-01    10          89     -374.3681 1124.7362 1917.0826
      1.247246 2.872949e-01    10          87     -376.2179 1120.4357 1895.9236
      1.319793 2.671906e-01    10          86     -377.4743 1118.9487 1886.0074
      1.392476 2.484592e-01     8          66     -392.2825 1064.5651 1654.6102
      1.402794 2.459090e-01     8          66     -393.6812 1067.3624 1657.4076
      1.421783 2.412833e-01     8          65     -395.6243 1067.2486 1648.8646
      1.437610 2.374947e-01     8          64     -397.0611 1066.1223 1639.3090
      1.445170 2.357060e-01     8          60     -398.9878 1053.9756 1593.4454
      1.477267 2.282607e-01     8          59     -400.5155 1053.0310 1584.0717
      1.517674 2.192213e-01     8          58     -402.0185 1052.0371 1574.6485
      1.538883 2.146207e-01     8          57     -404.7330 1053.4659 1567.6481
      1.550745 2.120899e-01     8          56     -406.2783 1052.5565 1558.3095
      1.566520 2.087705e-01     8          52     -409.1532 1042.3064 1514.3425
      1.595110 2.028862e-01     8          50     -411.2862 1038.5724 1493.7501
      1.617948 1.983051e-01     8          49     -412.8997 1037.7994 1484.5479
      1.629846 1.959597e-01     8          49     -414.5221 1041.0442 1487.7927
      1.638429 1.942851e-01     8          48     -416.1594 1040.3189 1478.6381
      1.659070 1.903160e-01     8          46     -418.4485 1036.8971 1458.3579
      1.685882 1.852809e-01     8          44     -420.6821 1033.3642 1437.9665
      1.727358 1.777533e-01     8          42     -423.6394 1031.2789 1419.0228
      1.771785 1.700292e-01     8          39     -426.0461 1024.0922 1386.5485
      1.806683 1.641979e-01     8          38     -427.8272 1023.6545 1377.6815
      1.833370 1.598738e-01     8          36     -429.7605 1019.5210 1356.6896
      1.841439 1.585891e-01     8          35     -431.5947 1019.1893 1347.9287
      1.906938 1.485344e-01     7          27     -438.0408  998.0815 1255.1726
      1.968433 1.396755e-01     5          23     -442.2061  986.4122 1201.3573
      2.079931 1.249389e-01     5          21     -444.8435  983.6870 1181.7736
      2.212651 1.094102e-01     5          19     -447.7499  981.4997 1162.7279
      2.256204 1.047474e-01     5          17     -450.1131  978.2261 1142.5958
      2.303889 9.986969e-02     5          14     -454.0615  974.1231 1113.2051
      2.552296 7.790260e-02     4          10     -458.8848  965.7695 1066.9201
      2.834259 5.876203e-02     4           8     -461.9353  963.8706 1048.1627
      2.880310 5.611737e-02     3           5     -467.2549  960.5098 1015.2997
      3.378896 3.408506e-02     1           3     -471.5896  957.1793  986.6815
      8.715379 1.640434e-04     1           3     -475.5345  965.0689  994.5712
     21.194476 6.242464e-10     1           2     -494.7895  999.5790 1020.6520
     25.464063 8.731682e-12     0           1     -519.1294 1042.2587 1050.6880

# tune_vlmc verbosity is adequate

    Fitting a vlmc with max_depth= 2 and cutoff= 1.553652 
    Max depth reached, increasing it to 4 
    Max depth reached, increasing it to 8 
    Max depth reached, increasing it to 16 
    Initial criterion= Inf 
    Improving criterion= 1594.977 
    Pruning vlmc with cutoff= 1.56652 
    Improving criterion= 1551.361 
    Pruning vlmc with cutoff= 1.59511 
    Improving criterion= 1518.339 
    Pruning vlmc with cutoff= 1.617948 
    Improving criterion= 1509.137 
    Pruning vlmc with cutoff= 1.629846 
    Improving criterion= 1499.952 
    Pruning vlmc with cutoff= 1.638429 
    Improving criterion= 1490.798 
    Pruning vlmc with cutoff= 1.65907 
    Improving criterion= 1470.517 
    Pruning vlmc with cutoff= 1.685882 
    Improving criterion= 1450.126 
    Pruning vlmc with cutoff= 1.727358 
    Improving criterion= 1418.753 
    Pruning vlmc with cutoff= 1.771785 
    Improving criterion= 1386.279 
    Pruning vlmc with cutoff= 1.806683 
    Improving criterion= 1377.412 
    Pruning vlmc with cutoff= 1.83337 
    Improving criterion= 1356.42 
    Pruning vlmc with cutoff= 1.841439 
    Improving criterion= 1347.659 
    Pruning vlmc with cutoff= 1.906938 
    Improving criterion= 1258.923 
    Pruning vlmc with cutoff= 1.968433 
    Improving criterion= 1202.72 
    Pruning vlmc with cutoff= 2.079931 
    Improving criterion= 1183.136 
    Pruning vlmc with cutoff= 2.212651 
    Improving criterion= 1151.661 
    Pruning vlmc with cutoff= 2.256204 
    Improving criterion= 1131.529 
    Pruning vlmc with cutoff= 2.303889 
    Improving criterion= 1101.943 
    Pruning vlmc with cutoff= 2.552296 
    Improving criterion= 1059.479 
    Pruning vlmc with cutoff= 2.834259 
    Improving criterion= 1040.722 
    Pruning vlmc with cutoff= 2.88031 
    Improving criterion= 1013.084 
    Pruning vlmc with cutoff= 3.378896 
    Improving criterion= 994.9173 
    Pruning vlmc with cutoff= 8.715379 
    Improving criterion= 990.3778 
    Pruning vlmc with cutoff= 21.19448 
    Pruning vlmc with cutoff= 25.46406 
    VLMC context tree on 0, 1, 2 
     cutoff: 8.715 (quantile: 0.000164)
     Number of contexts: 3 
     Maximum context length: 1 
     Selected by BIC (990.3778) with likelihood function "extended" (-476.5451)

