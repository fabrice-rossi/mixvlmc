# metrics.vlmc objects print as expected

    Code
      print(m_metrics)
    Output
      VLMC context tree on (0,0.458], (0.458,1.34], (1.34,2.13], (2.13,7.54] 
       cutoff: 3.126 (quantile: 0.1)
       Number of contexts: 15 
       Maximum context length: 5 
       Confusion matrix: 
                     (0,0.458] (0.458,1.34] (1.34,2.13] (2.13,7.54] 
        (0,0.458]    161       220          218         95          
        (0.458,1.34] 70        16           12          52          
        (1.34,2.13]  10        9            11          52          
        (2.13,7.54]  11        6            11          53          
       Accurary: 0.2393 
       AUC: 0.5029 

