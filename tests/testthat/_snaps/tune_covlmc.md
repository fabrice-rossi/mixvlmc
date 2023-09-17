# print works as expected

    Code
      print(bt_covlmc)
    Output
      VLMC with covariate context tree on A, B, C 
       cutoff in quantile scale: 7.738e-07
       Number of contexts: 3 
       Maximum context length: 1 
       Selected by BIC (1059.721) with likelihood function "extended" (-467.7144)

---

    Code
      print(at_covlmc)
    Output
      VLMC with covariate context tree on A, B, C 
       cutoff in quantile scale: 0.5
       Number of contexts: 3 
       Maximum context length: 1 
       Selected by AIC (953.5087) with likelihood function "extended" (-450.7543)

# summary works as expected

    Code
      print(summary(bt_covlmc))
    Output
      VLMC with covariate tune results
      
      Best VLMC with covariate selected by BIC (1059.721) with likelihood function "extended" (-467.7144)
      VLMC with covariate context tree on A, B, C 
       cutoff in quantile scale: 7.738e-07
       Number of contexts: 3 
       Maximum context length: 1 
      
      Pruning results
              alpha depth nb_contexts loglikelihood cov_depth       AIC      BIC
       5.000000e-01     1           3     -450.7543         1  953.5087 1063.088
       7.738076e-07     1           3     -467.7144         1  975.4289 1059.721
       1.454609e-13     1           3     -501.0912         1 1026.1823 1076.758
       1.950257e-17     0           1     -546.7455         0 1097.4909 1105.920

---

    Code
      print(summary(at_covlmc))
    Output
      VLMC with covariate tune results
      
      Best VLMC with covariate selected by AIC (953.5087) with likelihood function "extended" (-450.7543)
      VLMC with covariate context tree on A, B, C 
       cutoff in quantile scale: 0.5
       Number of contexts: 3 
       Maximum context length: 1 
      
      Pruning results
              alpha depth nb_contexts loglikelihood cov_depth       AIC      BIC
       5.000000e-01     1           3     -450.7543         1  953.5087 1063.088
       7.738076e-07     1           3     -467.7144         1  975.4289 1059.721
       1.454609e-13     1           3     -501.0912         1 1026.1823 1076.758
       1.950257e-17     0           1     -546.7455         0 1097.4909 1105.920

# tune_vlmc verbosity is adequate

    Fitting a covlmc with max_depth= 100 and alpha= 0.5 
    Initial criterion= Inf 
    Improving criterion= 1087.44 
    Pruning covlmc with alpha= 1.323035e-09 
    VLMC with covariate context tree on A, B, C 
     cutoff in quantile scale: 0.5
     Number of contexts: 3 
     Maximum context length: 1 
     Selected by BIC (1087.44) with likelihood function "extended" (-462.9299)

