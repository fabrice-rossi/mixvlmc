# metrics.vlmc objects print as expecte

    Code
      print(m_metrics)
    Output
      VLMC context tree on (0,0.458], (0.458,1.34], (1.34,2.13], (2.13,7.54] 
       cutoff: 3.126 (quantile: 0.1)
       Number of contexts: 15 
       Maximum context length: 5 
       Confusion matrix: 
                               (0,0.458] (0.458,1.34] (1.34,2.13] (2.13,7.54] 
        predicted (0,0.458]    218       60           8           4           
        predicted (0.458,1.34] 32        156          21          4           
        predicted (1.34,2.13]  1         32           187         39          
        predicted (2.13,7.54]  1         3            36          205         
       Accurary: 0.7607 
       AUC: 0.5029 

