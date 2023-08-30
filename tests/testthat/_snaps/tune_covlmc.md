# print works as expected

    Code
      print(bt_covlmc)
    Output
      VLMC with covariate context tree on A, B, C 
       cutoff in quantile scale: 7.738e-07
       Number of contexts: 3 
       Maximum context length: 1 
       Selected by BIC (1044.993) with likelihood function "truncated" (-466.5832)

---

    Code
      print(at_covlmc)
    Output
      VLMC with covariate context tree on A, B, C 
       cutoff in quantile scale: 0.5
       Number of contexts: 3 
       Maximum context length: 1 
       Selected by AIC (947.2463) with likelihood function "truncated" (-449.6231)

# summary works as expected

    Code
      print(summary(bt_covlmc))
    Output
      VLMC with covariate tune results
      
      Best VLMC with covariate selected by BIC (1044.993) with likelihood function "truncated" (-466.5832)
      VLMC with covariate context tree on A, B, C 
       cutoff in quantile scale: 7.738e-07
       Number of contexts: 3 
       Maximum context length: 1 
      
      Pruning results
              alpha depth nb_contexts loglikelihood       AIC      BIC
       5.000000e-01     1           3     -449.6231  947.2463 1048.349
       7.738076e-07     1           3     -466.5832  969.1665 1044.993
       1.454609e-13     1           3     -499.9600 1015.9199 1049.621
       1.950257e-17     0           1     -546.7455 1097.4909 1105.920

---

    Code
      print(summary(at_covlmc))
    Output
      VLMC with covariate tune results
      
      Best VLMC with covariate selected by AIC (947.2463) with likelihood function "truncated" (-449.6231)
      VLMC with covariate context tree on A, B, C 
       cutoff in quantile scale: 0.5
       Number of contexts: 3 
       Maximum context length: 1 
      
      Pruning results
              alpha depth nb_contexts loglikelihood       AIC      BIC
       5.000000e-01     1           3     -449.6231  947.2463 1048.349
       7.738076e-07     1           3     -466.5832  969.1665 1044.993
       1.454609e-13     1           3     -499.9600 1015.9199 1049.621
       1.950257e-17     0           1     -546.7455 1097.4909 1105.920

