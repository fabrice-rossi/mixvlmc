# metrics.covlmc objects print as expected without AUC

    Code
      print(m_metrics)
    Output
      VLMC with covariate context tree on low, typical, high 
       cutoff in quantile scale: 0.5
       Number of contexts: 5 
       Maximum context length: 2 
       Confusion matrix: 
                low typical high 
        low     129 39      0    
        typical 38  468     46   
        high    1   45      240  
       Accurary: 0.832 
       AUC: 0.9 

# metrics.covlmc objects print as expected

    Code
      print(m_metrics)
    Output
      VLMC with covariate context tree on low, typical, high 
       cutoff in quantile scale: 0.5
       Number of contexts: 5 
       Maximum context length: 2 
       Confusion matrix: 
                low typical high 
        low     129 39      0    
        typical 38  468     46   
        high    1   45      240  
       Accurary: 0.832 
       AUC: 0.9285 

