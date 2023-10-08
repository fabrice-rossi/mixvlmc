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
        (0,0.458]    200       49           2           1           
        (0.458,1.34] 50        168          27          7           
        (1.34,2.13]  1         32           187         39          
        (2.13,7.54]  1         3            36          205         
       Accuracy: 0.754 
       AUC: 0.9068 

