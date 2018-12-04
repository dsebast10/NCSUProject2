---
title: "Multi Linear Regression Model Results"
output: html_notebook
---

# Final Model

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

The model has an adjusted R^2 of 0.784 which is fairly good. That is a large portion of the variance in intra-state recovery ratios being explained by the legal environment.

The specific variables that the model chose do not necessarily make the most sense when thinking about trafficking. We might expect laws about Background Checks, and regulation of Dealers to have the greatest impact. It is possible that some of these variables are highly co-linear with some of the variables that were included in the model already and therefore were not chosen. 

The most consequential variable turned out to be the Assault Weapons ban. Again, this doesn't make too much sense. The vast majority of guns traced are handguns. (Unfortunately, while the ATF does release data on the types of guns recovered, that data is released in aggregate so we can't pin down if the guns coming into states are more likely to be assault weapons) 

It is important to remember that all of these indicies from Cato are very made up, with little thought to them, other than to chide states with stronger gun laws.

