---
title: "Multiple Linear Regression Model Selection"
output:
  html_document:
    df_print: paged
---

# Goal

We want to model how the ratio of intra-state gun traces to total gun traces in each state is affected by gun laws in those states.

The ratio of intra-state gun traces to total gun traces is being used as a measure of the availabilty of guns in that state. If guns are not very available to residents, they will be more likely to get guns from other states, sometimes through illegal means.

# Models

## Null Model

```r
m_null <- lm(data = df_means, `Rate of Home Recoveries`~1)
summary(m_null)
```

```
## 
## Call:
## lm(formula = `Rate of Home Recoveries` ~ 1, data = df_means)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.46732 -0.03272  0.03157  0.08636  0.15680 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.6791     0.0185   36.72   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1308 on 49 degrees of freedom
```


## Full Model

```r
m <- lm(data = df_means, `Rate of Home Recoveries`~.-Abbr-State)
summary(m)
```

```
## 
## Call:
## lm(formula = `Rate of Home Recoveries` ~ . - Abbr - State, data = df_means)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.099988 -0.027843 -0.000187  0.026328  0.108760 
## 
## Coefficients: (1 not defined because of singularities)
##                                                      Estimate Std. Error
## (Intercept)                                         6.674e-01  1.582e-02
## `Initial Permit Cost`                              -7.716e-02  4.542e-02
## `Training or Testing Requirement for Carry Permit`  7.768e-03  1.481e-02
## `Initial Permit Term`                              -1.613e-02  1.515e-02
## `Open Carry Index`                                 -9.733e-03  1.775e-02
## `Concelaed Carry Index`                             7.343e-02  4.402e-02
## `Assault Weapons Ban`                               1.647e-01  1.283e-01
## `Design Safety Standards`                           2.999e-02  4.150e-02
## `Large Capacity Magazine Ban`                      -8.224e-02  1.065e-01
## `50 Caliber Ban`                                   -7.180e-02  1.941e-02
## `Local Gun Ban`                                    -6.455e-03  5.497e-02
## `Stricter Minimum Age`                             -2.693e-02  1.274e-02
## `Waiting Period`                                    1.908e-02  2.217e-02
## `Restrictions on multiple purchases`                7.867e-03  1.973e-02
## `Licensing or Regulation of Dealers`               -1.838e-02  1.981e-02
## `Background Checks for Private Sales`               1.992e-02  2.643e-02
## `Firearms Licensing Index`                         -4.803e-02  6.509e-02
## `Registration of Firearms`                          5.533e-02  3.278e-02
## `Built in Locking Devices Required`                 2.913e-02  2.788e-02
## Microstamping                                      -3.722e-02  5.226e-02
## `Stand Your Ground Law`                             2.073e-02  1.502e-02
## `Machine Guns Prohibited`                           9.215e-03  2.761e-02
## `Silencers Prohibited`                              2.260e-03  2.744e-02
## `Short Barreled Rifle Prohibited`                  -6.968e-05  4.251e-02
## `Short Barreled Shotgun Prohibited`                 4.284e-03  3.445e-02
## `Retention of Sales Records`                       -3.247e-02  2.564e-02
## `Gun Rights`                                               NA         NA
##                                                    t value Pr(>|t|)    
## (Intercept)                                         42.178  < 2e-16 ***
## `Initial Permit Cost`                               -1.699  0.10229    
## `Training or Testing Requirement for Carry Permit`   0.525  0.60474    
## `Initial Permit Term`                               -1.065  0.29748    
## `Open Carry Index`                                  -0.548  0.58863    
## `Concelaed Carry Index`                              1.668  0.10831    
## `Assault Weapons Ban`                                1.283  0.21157    
## `Design Safety Standards`                            0.723  0.47678    
## `Large Capacity Magazine Ban`                       -0.772  0.44746    
## `50 Caliber Ban`                                    -3.700  0.00112 ** 
## `Local Gun Ban`                                     -0.117  0.90749    
## `Stricter Minimum Age`                              -2.113  0.04516 *  
## `Waiting Period`                                     0.860  0.39805    
## `Restrictions on multiple purchases`                 0.399  0.69356    
## `Licensing or Regulation of Dealers`                -0.928  0.36275    
## `Background Checks for Private Sales`                0.754  0.45834    
## `Firearms Licensing Index`                          -0.738  0.46766    
## `Registration of Firearms`                           1.688  0.10441    
## `Built in Locking Devices Required`                  1.045  0.30639    
## Microstamping                                       -0.712  0.48316    
## `Stand Your Ground Law`                              1.380  0.18032    
## `Machine Guns Prohibited`                            0.334  0.74144    
## `Silencers Prohibited`                               0.082  0.93504    
## `Short Barreled Rifle Prohibited`                   -0.002  0.99871    
## `Short Barreled Shotgun Prohibited`                  0.124  0.90206    
## `Retention of Sales Records`                        -1.266  0.21751    
## `Gun Rights`                                            NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.06545 on 24 degrees of freedom
## Multiple R-squared:  0.8773,	Adjusted R-squared:  0.7495 
## F-statistic: 6.865 on 25 and 24 DF,  p-value: 5.642e-06
```

# Now use Forward Model Selection to choose only the Variables that help

```r
M_step <- step(object=m_null, scope = list(lower=m_null, upper = m), direction = "forward")
```

```
## Start:  AIC=-202.43
## `Rate of Home Recoveries` ~ 1
## 
##                                                      Df Sum of Sq     RSS
## + `Assault Weapons Ban`                               1   0.45216 0.38591
## + `Large Capacity Magazine Ban`                       1   0.44984 0.38822
## + `Concelaed Carry Index`                             1   0.40927 0.42880
## + `Gun Rights`                                        1   0.38515 0.45292
## + `Firearms Licensing Index`                          1   0.37603 0.46203
## + `Silencers Prohibited`                              1   0.32558 0.51248
## + `Background Checks for Private Sales`               1   0.31799 0.52008
## + `Built in Locking Devices Required`                 1   0.28575 0.55232
## + `Short Barreled Rifle Prohibited`                   1   0.27323 0.56484
## + `Registration of Firearms`                          1   0.27247 0.56560
## + `Open Carry Index`                                  1   0.21345 0.62462
## + `Short Barreled Shotgun Prohibited`                 1   0.20391 0.63416
## + `Machine Guns Prohibited`                           1   0.18627 0.65180
## + `Retention of Sales Records`                        1   0.12579 0.71228
## + `Waiting Period`                                    1   0.11521 0.72286
## + `Design Safety Standards`                           1   0.11349 0.72458
## + `Restrictions on multiple purchases`                1   0.09409 0.74397
## + `Initial Permit Cost`                               1   0.07382 0.76425
## + `Licensing or Regulation of Dealers`                1   0.07062 0.76744
## + `Stand Your Ground Law`                             1   0.06827 0.76979
## + `Training or Testing Requirement for Carry Permit`  1   0.06278 0.77529
## + Microstamping                                       1   0.03449 0.80357
## <none>                                                            0.83807
## + `Local Gun Ban`                                     1   0.03013 0.80793
## + `Initial Permit Term`                               1   0.01956 0.81851
## + `Stricter Minimum Age`                              1   0.01109 0.82698
## + `50 Caliber Ban`                                    1   0.00408 0.83399
##                                                          AIC
## + `Assault Weapons Ban`                              -239.21
## + `Large Capacity Magazine Ban`                      -238.91
## + `Concelaed Carry Index`                            -233.94
## + `Gun Rights`                                       -231.20
## + `Firearms Licensing Index`                         -230.21
## + `Silencers Prohibited`                             -225.03
## + `Background Checks for Private Sales`              -224.29
## + `Built in Locking Devices Required`                -221.28
## + `Short Barreled Rifle Prohibited`                  -220.16
## + `Registration of Firearms`                         -220.09
## + `Open Carry Index`                                 -215.13
## + `Short Barreled Shotgun Prohibited`                -214.37
## + `Machine Guns Prohibited`                          -213.00
## + `Retention of Sales Records`                       -208.57
## + `Waiting Period`                                   -207.83
## + `Design Safety Standards`                          -207.71
## + `Restrictions on multiple purchases`               -206.39
## + `Initial Permit Cost`                              -205.04
## + `Licensing or Regulation of Dealers`               -204.84
## + `Stand Your Ground Law`                            -204.68
## + `Training or Testing Requirement for Carry Permit` -204.33
## + Microstamping                                      -202.53
## <none>                                               -202.43
## + `Local Gun Ban`                                    -202.26
## + `Initial Permit Term`                              -201.62
## + `Stricter Minimum Age`                             -201.10
## + `50 Caliber Ban`                                   -200.68
## 
## Step:  AIC=-239.21
## `Rate of Home Recoveries` ~ `Assault Weapons Ban`
## 
##                                                      Df Sum of Sq     RSS
## + `50 Caliber Ban`                                    1  0.146994 0.23891
## + `Silencers Prohibited`                              1  0.074211 0.31170
## + Microstamping                                       1  0.056727 0.32918
## + `Short Barreled Rifle Prohibited`                   1  0.039169 0.34674
## + `Short Barreled Shotgun Prohibited`                 1  0.033857 0.35205
## + `Concelaed Carry Index`                             1  0.027857 0.35805
## + `Firearms Licensing Index`                          1  0.017278 0.36863
## + `Gun Rights`                                        1  0.016561 0.36935
## + `Machine Guns Prohibited`                           1  0.015321 0.37059
## <none>                                                            0.38591
## + `Restrictions on multiple purchases`                1  0.011617 0.37429
## + `Background Checks for Private Sales`               1  0.010762 0.37514
## + `Design Safety Standards`                           1  0.008398 0.37751
## + `Training or Testing Requirement for Carry Permit`  1  0.007942 0.37796
## + `Built in Locking Devices Required`                 1  0.006915 0.37899
## + `Local Gun Ban`                                     1  0.006686 0.37922
## + `Stricter Minimum Age`                              1  0.005795 0.38011
## + `Stand Your Ground Law`                             1  0.004005 0.38190
## + `Licensing or Regulation of Dealers`                1  0.003900 0.38201
## + `Large Capacity Magazine Ban`                       1  0.003817 0.38209
## + `Retention of Sales Records`                        1  0.002156 0.38375
## + `Initial Permit Term`                               1  0.002128 0.38378
## + `Open Carry Index`                                  1  0.001511 0.38440
## + `Registration of Firearms`                          1  0.001044 0.38486
## + `Waiting Period`                                    1  0.000075 0.38583
## + `Initial Permit Cost`                               1  0.000020 0.38589
##                                                          AIC
## + `50 Caliber Ban`                                   -261.18
## + `Silencers Prohibited`                             -247.89
## + Microstamping                                      -245.16
## + `Short Barreled Rifle Prohibited`                  -242.56
## + `Short Barreled Shotgun Prohibited`                -241.80
## + `Concelaed Carry Index`                            -240.96
## + `Firearms Licensing Index`                         -239.50
## + `Gun Rights`                                       -239.40
## + `Machine Guns Prohibited`                          -239.24
## <none>                                               -239.21
## + `Restrictions on multiple purchases`               -238.74
## + `Background Checks for Private Sales`              -238.62
## + `Design Safety Standards`                          -238.31
## + `Training or Testing Requirement for Carry Permit` -238.25
## + `Built in Locking Devices Required`                -238.11
## + `Local Gun Ban`                                    -238.08
## + `Stricter Minimum Age`                             -237.97
## + `Stand Your Ground Law`                            -237.73
## + `Licensing or Regulation of Dealers`               -237.72
## + `Large Capacity Magazine Ban`                      -237.71
## + `Retention of Sales Records`                       -237.49
## + `Initial Permit Term`                              -237.49
## + `Open Carry Index`                                 -237.41
## + `Registration of Firearms`                         -237.34
## + `Waiting Period`                                   -237.22
## + `Initial Permit Cost`                              -237.21
## 
## Step:  AIC=-261.18
## `Rate of Home Recoveries` ~ `Assault Weapons Ban` + `50 Caliber Ban`
## 
##                                                      Df Sum of Sq     RSS
## + `Silencers Prohibited`                              1  0.041678 0.19723
## + `Concelaed Carry Index`                             1  0.021564 0.21735
## + `Short Barreled Rifle Prohibited`                   1  0.020355 0.21856
## + `Short Barreled Shotgun Prohibited`                 1  0.018885 0.22003
## + `Machine Guns Prohibited`                           1  0.017972 0.22094
## + `Restrictions on multiple purchases`                1  0.011426 0.22749
## + `Gun Rights`                                        1  0.011109 0.22780
## <none>                                                            0.23891
## + `Built in Locking Devices Required`                 1  0.007190 0.23172
## + `Background Checks for Private Sales`               1  0.006848 0.23206
## + `Waiting Period`                                    1  0.006279 0.23263
## + `Initial Permit Cost`                               1  0.005318 0.23359
## + `Training or Testing Requirement for Carry Permit`  1  0.004922 0.23399
## + `Stand Your Ground Law`                             1  0.004760 0.23415
## + `Stricter Minimum Age`                              1  0.004414 0.23450
## + `Firearms Licensing Index`                          1  0.004072 0.23484
## + `Registration of Firearms`                          1  0.002470 0.23644
## + `Licensing or Regulation of Dealers`                1  0.002198 0.23671
## + `Large Capacity Magazine Ban`                       1  0.002169 0.23674
## + `Initial Permit Term`                               1  0.002074 0.23684
## + `Open Carry Index`                                  1  0.001047 0.23787
## + `Local Gun Ban`                                     1  0.000876 0.23804
## + Microstamping                                       1  0.000782 0.23813
## + `Design Safety Standards`                           1  0.000722 0.23819
## + `Retention of Sales Records`                        1  0.000025 0.23889
##                                                          AIC
## + `Silencers Prohibited`                             -268.77
## + `Concelaed Carry Index`                            -263.91
## + `Short Barreled Rifle Prohibited`                  -263.64
## + `Short Barreled Shotgun Prohibited`                -263.30
## + `Machine Guns Prohibited`                          -263.09
## + `Restrictions on multiple purchases`               -261.63
## + `Gun Rights`                                       -261.56
## <none>                                               -261.18
## + `Built in Locking Devices Required`                -260.71
## + `Background Checks for Private Sales`              -260.64
## + `Waiting Period`                                   -260.52
## + `Initial Permit Cost`                              -260.31
## + `Training or Testing Requirement for Carry Permit` -260.23
## + `Stand Your Ground Law`                            -260.19
## + `Stricter Minimum Age`                             -260.12
## + `Firearms Licensing Index`                         -260.04
## + `Registration of Firearms`                         -259.70
## + `Licensing or Regulation of Dealers`               -259.65
## + `Large Capacity Magazine Ban`                      -259.64
## + `Initial Permit Term`                              -259.62
## + `Open Carry Index`                                 -259.40
## + `Local Gun Ban`                                    -259.37
## + Microstamping                                      -259.35
## + `Design Safety Standards`                          -259.33
## + `Retention of Sales Records`                       -259.19
## 
## Step:  AIC=-268.77
## `Rate of Home Recoveries` ~ `Assault Weapons Ban` + `50 Caliber Ban` + 
##     `Silencers Prohibited`
## 
##                                                      Df Sum of Sq     RSS
## + `Initial Permit Cost`                               1 0.0112251 0.18601
## + `Design Safety Standards`                           1 0.0104793 0.18676
## + `Restrictions on multiple purchases`                1 0.0098129 0.18742
## <none>                                                            0.19723
## + `Stricter Minimum Age`                              1 0.0075546 0.18968
## + `Large Capacity Magazine Ban`                       1 0.0046430 0.19259
## + `Licensing or Regulation of Dealers`                1 0.0042849 0.19295
## + `Concelaed Carry Index`                             1 0.0041190 0.19312
## + `Initial Permit Term`                               1 0.0031808 0.19405
## + `Stand Your Ground Law`                             1 0.0025706 0.19466
## + `Built in Locking Devices Required`                 1 0.0018419 0.19539
## + Microstamping                                       1 0.0011983 0.19604
## + `Training or Testing Requirement for Carry Permit`  1 0.0010875 0.19615
## + `Short Barreled Shotgun Prohibited`                 1 0.0007283 0.19651
## + `Open Carry Index`                                  1 0.0006972 0.19654
## + `Registration of Firearms`                          1 0.0006960 0.19654
## + `Firearms Licensing Index`                          1 0.0005593 0.19667
## + `Waiting Period`                                    1 0.0002252 0.19701
## + `Local Gun Ban`                                     1 0.0002132 0.19702
## + `Background Checks for Private Sales`               1 0.0001068 0.19713
## + `Machine Guns Prohibited`                           1 0.0000768 0.19716
## + `Gun Rights`                                        1 0.0000453 0.19719
## + `Short Barreled Rifle Prohibited`                   1 0.0000154 0.19722
## + `Retention of Sales Records`                        1 0.0000000 0.19723
##                                                          AIC
## + `Initial Permit Cost`                              -269.70
## + `Design Safety Standards`                          -269.50
## + `Restrictions on multiple purchases`               -269.32
## <none>                                               -268.77
## + `Stricter Minimum Age`                             -268.72
## + `Large Capacity Magazine Ban`                      -267.96
## + `Licensing or Regulation of Dealers`               -267.87
## + `Concelaed Carry Index`                            -267.82
## + `Initial Permit Term`                              -267.58
## + `Stand Your Ground Law`                            -267.43
## + `Built in Locking Devices Required`                -267.24
## + Microstamping                                      -267.07
## + `Training or Testing Requirement for Carry Permit` -267.05
## + `Short Barreled Shotgun Prohibited`                -266.95
## + `Open Carry Index`                                 -266.95
## + `Registration of Firearms`                         -266.95
## + `Firearms Licensing Index`                         -266.91
## + `Waiting Period`                                   -266.83
## + `Local Gun Ban`                                    -266.82
## + `Background Checks for Private Sales`              -266.80
## + `Machine Guns Prohibited`                          -266.79
## + `Gun Rights`                                       -266.78
## + `Short Barreled Rifle Prohibited`                  -266.77
## + `Retention of Sales Records`                       -266.77
## 
## Step:  AIC=-269.7
## `Rate of Home Recoveries` ~ `Assault Weapons Ban` + `50 Caliber Ban` + 
##     `Silencers Prohibited` + `Initial Permit Cost`
## 
##                                                      Df Sum of Sq     RSS
## + `Registration of Firearms`                          1 0.0106796 0.17533
## <none>                                                            0.18601
## + `Stricter Minimum Age`                              1 0.0067288 0.17928
## + `Concelaed Carry Index`                             1 0.0052697 0.18074
## + `Restrictions on multiple purchases`                1 0.0052125 0.18080
## + `Licensing or Regulation of Dealers`                1 0.0047544 0.18125
## + `Large Capacity Magazine Ban`                       1 0.0043854 0.18162
## + `Design Safety Standards`                           1 0.0037165 0.18229
## + `Gun Rights`                                        1 0.0036712 0.18234
## + `Training or Testing Requirement for Carry Permit`  1 0.0032638 0.18275
## + `Waiting Period`                                    1 0.0027956 0.18321
## + `Stand Your Ground Law`                             1 0.0023215 0.18369
## + `Retention of Sales Records`                        1 0.0016816 0.18433
## + `Initial Permit Term`                               1 0.0011000 0.18491
## + `Local Gun Ban`                                     1 0.0004349 0.18557
## + `Short Barreled Rifle Prohibited`                   1 0.0003208 0.18569
## + `Machine Guns Prohibited`                           1 0.0002969 0.18571
## + `Firearms Licensing Index`                          1 0.0002532 0.18576
## + Microstamping                                       1 0.0002359 0.18577
## + `Short Barreled Shotgun Prohibited`                 1 0.0002262 0.18578
## + `Open Carry Index`                                  1 0.0001429 0.18587
## + `Background Checks for Private Sales`               1 0.0001035 0.18591
## + `Built in Locking Devices Required`                 1 0.0000019 0.18601
##                                                          AIC
## + `Registration of Firearms`                         -270.65
## <none>                                               -269.70
## + `Stricter Minimum Age`                             -269.54
## + `Concelaed Carry Index`                            -269.14
## + `Restrictions on multiple purchases`               -269.12
## + `Licensing or Regulation of Dealers`               -268.99
## + `Large Capacity Magazine Ban`                      -268.89
## + `Design Safety Standards`                          -268.71
## + `Gun Rights`                                       -268.70
## + `Training or Testing Requirement for Carry Permit` -268.58
## + `Waiting Period`                                   -268.46
## + `Stand Your Ground Law`                            -268.33
## + `Retention of Sales Records`                       -268.15
## + `Initial Permit Term`                              -268.00
## + `Local Gun Ban`                                    -267.82
## + `Short Barreled Rifle Prohibited`                  -267.79
## + `Machine Guns Prohibited`                          -267.78
## + `Firearms Licensing Index`                         -267.77
## + Microstamping                                      -267.76
## + `Short Barreled Shotgun Prohibited`                -267.76
## + `Open Carry Index`                                 -267.74
## + `Background Checks for Private Sales`              -267.73
## + `Built in Locking Devices Required`                -267.70
## 
## Step:  AIC=-270.66
## `Rate of Home Recoveries` ~ `Assault Weapons Ban` + `50 Caliber Ban` + 
##     `Silencers Prohibited` + `Initial Permit Cost` + `Registration of Firearms`
## 
##                                                      Df Sum of Sq     RSS
## + `Stricter Minimum Age`                              1 0.0089681 0.16636
## <none>                                                            0.17533
## + `Licensing or Regulation of Dealers`                1 0.0067436 0.16859
## + `Concelaed Carry Index`                             1 0.0049204 0.17041
## + `Training or Testing Requirement for Carry Permit`  1 0.0046184 0.17071
## + `Stand Your Ground Law`                             1 0.0044561 0.17087
## + `Gun Rights`                                        1 0.0039655 0.17136
## + `Retention of Sales Records`                        1 0.0036367 0.17169
## + `Large Capacity Magazine Ban`                       1 0.0026845 0.17264
## + `Short Barreled Shotgun Prohibited`                 1 0.0026549 0.17267
## + `Restrictions on multiple purchases`                1 0.0023973 0.17293
## + `Waiting Period`                                    1 0.0022480 0.17308
## + `Local Gun Ban`                                     1 0.0022081 0.17312
## + `Firearms Licensing Index`                          1 0.0009670 0.17436
## + `Initial Permit Term`                               1 0.0009348 0.17439
## + `Design Safety Standards`                           1 0.0008533 0.17448
## + `Short Barreled Rifle Prohibited`                   1 0.0006003 0.17473
## + `Machine Guns Prohibited`                           1 0.0005015 0.17483
## + `Background Checks for Private Sales`               1 0.0004972 0.17483
## + `Built in Locking Devices Required`                 1 0.0000042 0.17533
## + `Open Carry Index`                                  1 0.0000016 0.17533
## + Microstamping                                       1 0.0000008 0.17533
##                                                          AIC
## + `Stricter Minimum Age`                             -271.28
## <none>                                               -270.65
## + `Licensing or Regulation of Dealers`               -270.62
## + `Concelaed Carry Index`                            -270.08
## + `Training or Testing Requirement for Carry Permit` -269.99
## + `Stand Your Ground Law`                            -269.94
## + `Gun Rights`                                       -269.80
## + `Retention of Sales Records`                       -269.70
## + `Large Capacity Magazine Ban`                      -269.43
## + `Short Barreled Shotgun Prohibited`                -269.42
## + `Restrictions on multiple purchases`               -269.34
## + `Waiting Period`                                   -269.30
## + `Local Gun Ban`                                    -269.29
## + `Firearms Licensing Index`                         -268.93
## + `Initial Permit Term`                              -268.92
## + `Design Safety Standards`                          -268.90
## + `Short Barreled Rifle Prohibited`                  -268.83
## + `Machine Guns Prohibited`                          -268.80
## + `Background Checks for Private Sales`              -268.80
## + `Built in Locking Devices Required`                -268.66
## + `Open Carry Index`                                 -268.66
## + Microstamping                                      -268.66
## 
## Step:  AIC=-271.28
## `Rate of Home Recoveries` ~ `Assault Weapons Ban` + `50 Caliber Ban` + 
##     `Silencers Prohibited` + `Initial Permit Cost` + `Registration of Firearms` + 
##     `Stricter Minimum Age`
## 
##                                                      Df Sum of Sq     RSS
## + `Training or Testing Requirement for Carry Permit`  1 0.0081080 0.15825
## + `Gun Rights`                                        1 0.0078369 0.15852
## + `Concelaed Carry Index`                             1 0.0067335 0.15963
## <none>                                                            0.16636
## + `Stand Your Ground Law`                             1 0.0053144 0.16105
## + `Waiting Period`                                    1 0.0051350 0.16123
## + `Licensing or Regulation of Dealers`                1 0.0049868 0.16137
## + `Large Capacity Magazine Ban`                       1 0.0043902 0.16197
## + `Retention of Sales Records`                        1 0.0030681 0.16329
## + `Local Gun Ban`                                     1 0.0029004 0.16346
## + `Machine Guns Prohibited`                           1 0.0021578 0.16420
## + `Restrictions on multiple purchases`                1 0.0019590 0.16440
## + `Firearms Licensing Index`                          1 0.0016746 0.16469
## + `Background Checks for Private Sales`               1 0.0007627 0.16560
## + `Built in Locking Devices Required`                 1 0.0007309 0.16563
## + `Short Barreled Shotgun Prohibited`                 1 0.0005706 0.16579
## + `Open Carry Index`                                  1 0.0004443 0.16592
## + `Initial Permit Term`                               1 0.0004112 0.16595
## + `Design Safety Standards`                           1 0.0001977 0.16616
## + `Short Barreled Rifle Prohibited`                   1 0.0001789 0.16618
## + Microstamping                                       1 0.0000026 0.16636
##                                                          AIC
## + `Training or Testing Requirement for Carry Permit` -271.78
## + `Gun Rights`                                       -271.69
## + `Concelaed Carry Index`                            -271.35
## <none>                                               -271.28
## + `Stand Your Ground Law`                            -270.90
## + `Waiting Period`                                   -270.85
## + `Licensing or Regulation of Dealers`               -270.80
## + `Large Capacity Magazine Ban`                      -270.62
## + `Retention of Sales Records`                       -270.21
## + `Local Gun Ban`                                    -270.16
## + `Machine Guns Prohibited`                          -269.93
## + `Restrictions on multiple purchases`               -269.87
## + `Firearms Licensing Index`                         -269.79
## + `Background Checks for Private Sales`              -269.51
## + `Built in Locking Devices Required`                -269.50
## + `Short Barreled Shotgun Prohibited`                -269.45
## + `Open Carry Index`                                 -269.41
## + `Initial Permit Term`                              -269.40
## + `Design Safety Standards`                          -269.34
## + `Short Barreled Rifle Prohibited`                  -269.33
## + Microstamping                                      -269.28
## 
## Step:  AIC=-271.78
## `Rate of Home Recoveries` ~ `Assault Weapons Ban` + `50 Caliber Ban` + 
##     `Silencers Prohibited` + `Initial Permit Cost` + `Registration of Firearms` + 
##     `Stricter Minimum Age` + `Training or Testing Requirement for Carry Permit`
## 
##                                         Df Sum of Sq     RSS     AIC
## + `Waiting Period`                       1 0.0067908 0.15146 -271.97
## <none>                                               0.15825 -271.78
## + `Stand Your Ground Law`                1 0.0051104 0.15314 -271.42
## + `Large Capacity Magazine Ban`          1 0.0044401 0.15381 -271.20
## + `Concelaed Carry Index`                1 0.0041079 0.15415 -271.09
## + `Gun Rights`                           1 0.0040509 0.15420 -271.08
## + `Local Gun Ban`                        1 0.0039960 0.15426 -271.06
## + `Initial Permit Term`                  1 0.0039618 0.15429 -271.05
## + `Licensing or Regulation of Dealers`   1 0.0026195 0.15563 -270.61
## + `Retention of Sales Records`           1 0.0016674 0.15659 -270.31
## + `Restrictions on multiple purchases`   1 0.0015461 0.15671 -270.27
## + `Firearms Licensing Index`             1 0.0014710 0.15678 -270.25
## + `Machine Guns Prohibited`              1 0.0012830 0.15697 -270.19
## + `Built in Locking Devices Required`    1 0.0007549 0.15750 -270.02
## + `Short Barreled Rifle Prohibited`      1 0.0004489 0.15781 -269.92
## + `Short Barreled Shotgun Prohibited`    1 0.0002634 0.15799 -269.86
## + `Background Checks for Private Sales`  1 0.0001073 0.15815 -269.81
## + Microstamping                          1 0.0000405 0.15821 -269.79
## + `Design Safety Standards`              1 0.0000074 0.15825 -269.78
## + `Open Carry Index`                     1 0.0000001 0.15825 -269.78
## 
## Step:  AIC=-271.97
## `Rate of Home Recoveries` ~ `Assault Weapons Ban` + `50 Caliber Ban` + 
##     `Silencers Prohibited` + `Initial Permit Cost` + `Registration of Firearms` + 
##     `Stricter Minimum Age` + `Training or Testing Requirement for Carry Permit` + 
##     `Waiting Period`
## 
##                                         Df Sum of Sq     RSS     AIC
## <none>                                               0.15146 -271.97
## + `Licensing or Regulation of Dealers`   1 0.0059196 0.14554 -271.96
## + `Large Capacity Magazine Ban`          1 0.0053608 0.14610 -271.77
## + `Initial Permit Term`                  1 0.0049727 0.14649 -271.64
## + `Stand Your Ground Law`                1 0.0043206 0.14714 -271.42
## + `Retention of Sales Records`           1 0.0025751 0.14889 -270.83
## + `Concelaed Carry Index`                1 0.0024170 0.14905 -270.78
## + `Short Barreled Rifle Prohibited`      1 0.0022456 0.14922 -270.72
## + `Local Gun Ban`                        1 0.0016213 0.14984 -270.51
## + `Gun Rights`                           1 0.0011823 0.15028 -270.36
## + `Short Barreled Shotgun Prohibited`    1 0.0008022 0.15066 -270.24
## + `Open Carry Index`                     1 0.0006833 0.15078 -270.20
## + `Machine Guns Prohibited`              1 0.0006362 0.15083 -270.18
## + `Firearms Licensing Index`             1 0.0004886 0.15097 -270.13
## + `Restrictions on multiple purchases`   1 0.0001590 0.15130 -270.02
## + `Design Safety Standards`              1 0.0001245 0.15134 -270.01
## + `Built in Locking Devices Required`    1 0.0001163 0.15135 -270.01
## + Microstamping                          1 0.0000080 0.15146 -269.98
## + `Background Checks for Private Sales`  1 0.0000003 0.15146 -269.97
```
The resulting model is given below

```r
summary(M_step)
```

```
## 
## Call:
## lm(formula = `Rate of Home Recoveries` ~ `Assault Weapons Ban` + 
##     `50 Caliber Ban` + `Silencers Prohibited` + `Initial Permit Cost` + 
##     `Registration of Firearms` + `Stricter Minimum Age` + `Training or Testing Requirement for Carry Permit` + 
##     `Waiting Period`, data = df_means)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.142142 -0.032878 -0.007898  0.041740  0.100415 
## 
## Coefficients:
##                                                     Estimate Std. Error
## (Intercept)                                         0.681584   0.009210
## `Assault Weapons Ban`                               0.103710   0.015964
## `50 Caliber Ban`                                   -0.067857   0.011225
## `Silencers Prohibited`                              0.030751   0.012680
## `Initial Permit Cost`                              -0.067726   0.022284
## `Registration of Firearms`                          0.036390   0.017948
## `Stricter Minimum Age`                             -0.020145   0.009503
## `Training or Testing Requirement for Carry Permit`  0.015809   0.009724
## `Waiting Period`                                    0.020236   0.014925
##                                                    t value Pr(>|t|)    
## (Intercept)                                         74.003  < 2e-16 ***
## `Assault Weapons Ban`                                6.497 8.51e-08 ***
## `50 Caliber Ban`                                    -6.045 3.73e-07 ***
## `Silencers Prohibited`                               2.425  0.01979 *  
## `Initial Permit Cost`                               -3.039  0.00412 ** 
## `Registration of Firearms`                           2.028  0.04914 *  
## `Stricter Minimum Age`                              -2.120  0.04011 *  
## `Training or Testing Requirement for Carry Permit`   1.626  0.11167    
## `Waiting Period`                                     1.356  0.18258    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.06078 on 41 degrees of freedom
## Multiple R-squared:  0.8193,	Adjusted R-squared:  0.784 
## F-statistic: 23.23 on 8 and 41 DF,  p-value: 6.314e-13
```


