# print works as expected

    VLMC context tree on 0, 1, 2 
     cutoff: 3.379 (quantile: 0.03409)
     Number of contexts: 3 
     Maximum context length: 1 
     Selected by BIC (980.4549) with likelihood function "truncated" (-471.5896)

---

    VLMC context tree on 0, 1, 2 
     cutoff: 2.88 (quantile: 0.05612)
     Number of contexts: 5 
     Maximum context length: 3 
     Selected by AIC (961.2005) with likelihood function "extended" (-467.2549)

# summary works as expected

    VLMC tune results
    
    Best VLMC selected by BIC (980.4549) with likelihood function "truncated" (-471.5896)
    VLMC context tree on 0, 1, 2 
     cutoff: 3.379 (quantile: 0.03409)
     Number of contexts: 3 
     Maximum context length: 1 
    
    Pruning results
        cutoff        alpha depth nb_contexts loglikelihood       AIC       BIC
      1.553652 2.114743e-01     8          56     -406.2783 1036.5565 1506.7862
      1.566520 2.087705e-01     8          52     -409.1532 1026.3064 1462.9481
      1.595110 2.028862e-01     8          50     -411.2862 1022.5724 1442.4203
      1.617948 1.983051e-01     8          49     -412.8997 1021.7994 1433.2504
      1.629846 1.959597e-01     8          49     -414.5221 1025.0442 1436.4951
      1.638429 1.942851e-01     8          48     -416.1594 1024.3189 1427.3729
      1.659070 1.903160e-01     8          46     -418.4485 1020.8971 1407.1571
      1.685882 1.852809e-01     8          44     -420.6821 1017.3642 1386.8303
      1.727358 1.777533e-01     8          42     -423.6394 1015.2789 1367.9511
      1.771785 1.700292e-01     8          39     -426.0461 1008.0922 1335.5735
      1.806683 1.641979e-01     8          38     -427.8272 1007.6545 1326.7389
      1.833370 1.598738e-01     8          36     -429.7605 1003.5210 1305.8115
      1.841439 1.585891e-01     8          35     -431.5947 1003.1893 1297.0828
      1.906938 1.485344e-01     7          27     -438.0408  984.0815 1210.9090
      1.968433 1.396755e-01     5          23     -442.2061  976.4122 1169.8219
      2.079931 1.249389e-01     5          21     -444.8435  973.6870 1150.2784
      2.212651 1.094102e-01     5          19     -447.7499  971.4997 1131.2729
      2.256204 1.047474e-01     5          17     -450.1131  968.2261 1111.1811
      2.303889 9.986969e-02     5          14     -454.0615  964.1231 1081.8507
      2.552296 7.790260e-02     4          10     -458.8848  957.7695 1041.9010
      2.834259 5.876203e-02     4           8     -461.9353  955.8706 1023.1758
      2.880310 5.611737e-02     3           5     -467.2549  954.5098  996.5957
      3.378896 3.408506e-02     1           3     -471.5896  955.1793  980.4549
      8.715379 1.640434e-04     1           3     -475.5345  963.0689  988.3446
     21.194476 6.242464e-10     1           2     -494.7895  997.5790 1014.4294
     25.464063 8.731682e-12     0           1     -519.1294 1042.2587 1050.6880

---

    VLMC tune results
    
    Best VLMC selected by AIC (961.2005) with likelihood function "extended" (-467.2549)
    VLMC context tree on 0, 1, 2 
     cutoff: 2.88 (quantile: 0.05612)
     Number of contexts: 5 
     Maximum context length: 3 
    
    Pruning results
        cutoff        alpha depth nb_contexts loglikelihood       AIC       BIC
      1.000000 3.678794e-01    10         112     -359.2180 1226.4360 2296.9464
      1.015516 3.622156e-01    10         111     -360.2274 1224.4548 2286.5361
      1.027283 3.579784e-01    10         107     -364.1591 1212.3182 2232.2534
      1.035378 3.550922e-01    10         104     -366.0575 1204.1150 2198.7625
      1.039644 3.535805e-01    10         103     -367.0953 1202.1906 2188.4089
      1.072543 3.421375e-01    10         102     -368.1368 1200.2735 2178.0626
      1.115718 3.276801e-01    10         102     -369.2413 1198.4826 2167.8425
      1.133206 3.219993e-01    10         100     -370.6453 1189.2906 2133.3628
      1.175289 3.087298e-01    10          96     -374.5456 1177.0913 2079.0174
      1.214332 2.969084e-01    10          95     -375.7579 1175.5158 2069.0127
      1.227189 2.931154e-01    10          89     -381.9685 1163.9371 2006.8587
      1.247246 2.872949e-01    10          87     -383.8183 1159.6366 1985.6997
      1.319793 2.671906e-01    10          86     -385.0748 1158.1495 1975.7835
      1.392476 2.484592e-01     8          66     -397.9734 1099.9468 1740.5673
      1.402794 2.459090e-01     8          66     -399.3721 1098.7442 1730.9354
      1.421783 2.412833e-01     8          65     -401.5288 1095.0575 1710.3903
      1.437610 2.374947e-01     8          64     -402.9656 1093.9312 1700.8347
      1.445170 2.357060e-01     8          60     -404.8922 1081.7845 1654.9712
      1.477267 2.282607e-01     8          59     -406.4200 1076.8399 1633.1682
      1.517674 2.192213e-01     8          58     -407.9230 1075.8460 1623.7450
      1.538883 2.146207e-01     8          57     -410.6374 1073.2748 1604.3154
      1.550745 2.120899e-01     8          56     -412.1827 1072.3654 1594.9768
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

# tune_vlmc verbosity is adequate

    Fitting a vlmc with max_depth= 2 and cutoff= 1.553652 
    Max depth reached, increasing it to 4 
    Max depth reached, increasing it to 8 
    Max depth reached, increasing it to 16 
    Initial criterion= Inf 
    Improving criterion= 1506.786 
    Pruning vlmc with cutoff= 1.56652 
    Improving criterion= 1462.948 
    Pruning vlmc with cutoff= 1.59511 
    Improving criterion= 1442.42 
    Pruning vlmc with cutoff= 1.617948 
    Improving criterion= 1433.25 
    Pruning vlmc with cutoff= 1.629846 
    Pruning vlmc with cutoff= 1.638429 
    Improving criterion= 1427.373 
    Pruning vlmc with cutoff= 1.65907 
    Improving criterion= 1407.157 
    Pruning vlmc with cutoff= 1.685882 
    Improving criterion= 1386.83 
    Pruning vlmc with cutoff= 1.727358 
    Improving criterion= 1367.951 
    Pruning vlmc with cutoff= 1.771785 
    Improving criterion= 1335.574 
    Pruning vlmc with cutoff= 1.806683 
    Improving criterion= 1326.739 
    Pruning vlmc with cutoff= 1.83337 
    Improving criterion= 1305.811 
    Pruning vlmc with cutoff= 1.841439 
    Improving criterion= 1297.083 
    Pruning vlmc with cutoff= 1.906938 
    Improving criterion= 1210.909 
    Pruning vlmc with cutoff= 1.968433 
    Improving criterion= 1169.822 
    Pruning vlmc with cutoff= 2.079931 
    Improving criterion= 1150.278 
    Pruning vlmc with cutoff= 2.212651 
    Improving criterion= 1131.273 
    Pruning vlmc with cutoff= 2.256204 
    Improving criterion= 1111.181 
    Pruning vlmc with cutoff= 2.303889 
    Improving criterion= 1081.851 
    Pruning vlmc with cutoff= 2.552296 
    Improving criterion= 1041.901 
    Pruning vlmc with cutoff= 2.834259 
    Improving criterion= 1023.176 
    Pruning vlmc with cutoff= 2.88031 
    Improving criterion= 996.5957 
    Pruning vlmc with cutoff= 3.378896 
    Improving criterion= 980.4549 
    Pruning vlmc with cutoff= 8.715379 
    Pruning vlmc with cutoff= 21.19448 
    Pruning vlmc with cutoff= 25.46406 
    VLMC context tree on 0, 1, 2 
     cutoff: 3.379 (quantile: 0.03409)
     Number of contexts: 3 
     Maximum context length: 1 
     Selected by BIC (980.4549) with likelihood function "truncated" (-471.5896)

