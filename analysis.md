Motorcycle claims - Wasa insurance 1994 to 1998
================
Christian Duffau-Rasmussen
2018-10-20

``` r
library(ggplot2)
library(insuranceData)
library(statmod)
library(TDboost)
```

    ## Loading required package: lattice

    ## Loaded TDboost 1.2

``` r
data(dataOhlsson)
```

## The data set

The data comes from the former Swedish insurance company Wasa, and
concerns partial casco insurance, for motorcycles. It contains
aggregated data on all insurance policies and claims during
    1994-1998.

``` r
head(dataOhlsson)
```

    ##   agarald kon zon mcklass fordald bonuskl duration antskad skadkost
    ## 1       0   M   1       4      12       1 0.175342       0        0
    ## 2       4   M   3       6       9       1 0.000000       0        0
    ## 3       5   K   3       3      18       1 0.454795       0        0
    ## 4       5   K   4       1      25       1 0.172603       0        0
    ## 5       6   K   2       1      26       1 0.180822       0        0
    ## 6       9   K   3       3       8       1 0.542466       0        0

The variables are:

  - `agarald` The owners age, between 0 and 99, a numeric vector
  - `kon` The owners age, between 0 and 99, a factor with levels K M
  - `zon` Geographic zone numbered from 1 to 7, in a standard
    classification of all Swedish parishes, a numeric vector
  - `mcklass` MC class, a classification by the so called EV ratio,
    defined as (Engine power in kW x 100) / (Vehicle weight in kg + 75),
    rounded to the nearest lower integer. The 75 kg represent the
    average driver weight. The EV ratios are divided into seven classes,
    a numeric vector
  - `fordald` Vehicle age, between 0 and 99, a numeric vector
  - `bonuskl` Bonus class, taking values from 1 to 7. A new driver
    starts with bonus class 1; for each claim-free year the bonus class
    is increased by 1. After the first claim the bonus is decreased by
    2; the driver can not return to class 7 with less than 6 consecutive
    claim free years, a numeric vector
  - `duration` the number of policy years, a numeric vector
  - `antskad` the number of claims, a numeric vector
  - `skadkost` the claim cost, a numeric
vector

## Linear regression

``` r
lm_model <- lm(skadkost ~ agarald + fordald + kon + factor(zon) + factor(mcklass) + factor(bonuskl) + duration, data=dataOhlsson)
summary(lm_model)
```

    ## 
    ## Call:
    ## lm(formula = skadkost ~ agarald + fordald + kon + factor(zon) + 
    ##     factor(mcklass) + factor(bonuskl) + duration, data = dataOhlsson)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -3211   -488   -249    -23 365087 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      1174.719    112.301  10.460  < 2e-16 ***
    ## agarald           -12.241      1.472  -8.316  < 2e-16 ***
    ## fordald           -19.126      2.035  -9.399  < 2e-16 ***
    ## konM              207.153     52.328   3.959 7.54e-05 ***
    ## factor(zon)2     -226.172     66.634  -3.394 0.000689 ***
    ## factor(zon)3     -435.807     65.723  -6.631 3.36e-11 ***
    ## factor(zon)4     -473.739     60.048  -7.889 3.08e-15 ***
    ## factor(zon)5     -599.650    108.763  -5.513 3.53e-08 ***
    ## factor(zon)6     -506.497     91.079  -5.561 2.69e-08 ***
    ## factor(zon)7     -597.933    247.949  -2.412 0.015889 *  
    ## factor(mcklass)2 -136.701     87.289  -1.566 0.117335    
    ## factor(mcklass)3 -108.596     69.651  -1.559 0.118966    
    ## factor(mcklass)4 -212.867     73.022  -2.915 0.003557 ** 
    ## factor(mcklass)5 -119.727     74.002  -1.618 0.105692    
    ## factor(mcklass)6   56.865     80.108   0.710 0.477793    
    ## factor(mcklass)7 -287.811    177.192  -1.624 0.104318    
    ## factor(bonuskl)2  -19.177     63.185  -0.303 0.761510    
    ## factor(bonuskl)3   48.941     69.498   0.704 0.481308    
    ## factor(bonuskl)4  100.415     72.555   1.384 0.166369    
    ## factor(bonuskl)5   34.428     76.991   0.447 0.654750    
    ## factor(bonuskl)6   28.812     75.778   0.380 0.703785    
    ## factor(bonuskl)7  106.194     54.648   1.943 0.051991 .  
    ## duration           92.519     15.281   6.054 1.42e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4683 on 64525 degrees of freedom
    ## Multiple R-squared:  0.005496,   Adjusted R-squared:  0.005157 
    ## F-statistic: 16.21 on 22 and 64525 DF,  p-value: < 2.2e-16

## Tweedie model of number of claims

``` r
tweedie_model <- glm(skadkost ~ agarald + fordald + kon + factor(zon) + factor(mcklass) + factor(bonuskl) + duration, data=dataOhlsson, family=statmod::tweedie(var.power = 1.5))
summary(tweedie_model)
```

    ## 
    ## Call:
    ## glm(formula = skadkost ~ agarald + fordald + kon + factor(zon) + 
    ##     factor(mcklass) + factor(bonuskl) + duration, family = statmod::tweedie(var.power = 1.5), 
    ##     data = dataOhlsson)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -61.072   -7.633   -6.413   -5.589  269.840  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       0.0149393  0.0129162   1.157  0.24743    
    ## agarald           0.0010275  0.0001619   6.346 2.22e-10 ***
    ## fordald           0.0040733  0.0004068  10.013  < 2e-16 ***
    ## konM             -0.0227028  0.0091994  -2.468  0.01360 *  
    ## factor(zon)2      0.0007045  0.0030892   0.228  0.81961    
    ## factor(zon)3      0.0150400  0.0052989   2.838  0.00454 ** 
    ## factor(zon)4      0.0224123  0.0054299   4.128 3.67e-05 ***
    ## factor(zon)5      0.0853930  0.0395024   2.162  0.03064 *  
    ## factor(zon)6      0.0469927  0.0203925   2.304  0.02120 *  
    ## factor(zon)7      0.6737595  1.1574227   0.582  0.56049    
    ## factor(mcklass)2  0.0026279  0.0105400   0.249  0.80311    
    ## factor(mcklass)3 -0.0041753  0.0075888  -0.550  0.58219    
    ## factor(mcklass)4  0.0038380  0.0086744   0.442  0.65816    
    ## factor(mcklass)5 -0.0059285  0.0077424  -0.766  0.44385    
    ## factor(mcklass)6 -0.0112937  0.0074832  -1.509  0.13125    
    ## factor(mcklass)7  0.0180097  0.0228752   0.787  0.43111    
    ## factor(bonuskl)2  0.0021177  0.0061775   0.343  0.73175    
    ## factor(bonuskl)3 -0.0048579  0.0049982  -0.972  0.33110    
    ## factor(bonuskl)4 -0.0023327  0.0046995  -0.496  0.61963    
    ## factor(bonuskl)5 -0.0023051  0.0065253  -0.353  0.72390    
    ## factor(bonuskl)6 -0.0025356  0.0064261  -0.395  0.69315    
    ## factor(bonuskl)7 -0.0058165  0.0043790  -1.328  0.18409    
    ## duration         -0.0017317  0.0002249  -7.700 1.38e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Tweedie family taken to be 4621.546)
    ## 
    ##     Null deviance: 7708506  on 64547  degrees of freedom
    ## Residual deviance: 5917313  on 64525  degrees of freedom
    ## AIC: NA
    ## 
    ## Number of Fisher Scoring iterations: 16

## TDBoost

``` r
tdboost_model <- TDboost(skadkost ~ agarald + fordald + kon + factor(zon) + factor(mcklass) + factor(bonuskl) + duration, 
                         data=dataOhlsson,
                         distribution= list(name="EDM", alpha=1.5))
```

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1       64.9933            -nan     0.0010   -0.0001
    ##      2       64.9837            -nan     0.0010    0.0107
    ##      3       64.9694            -nan     0.0010    0.0137
    ##      4       64.9551            -nan     0.0010    0.0149
    ##      5       64.9540            -nan     0.0010    0.0007
    ##      6       64.9434            -nan     0.0010    0.0074
    ##      7       64.9333            -nan     0.0010    0.0081
    ##      8       64.9226            -nan     0.0010    0.0106
    ##      9       64.9109            -nan     0.0010    0.0109
    ##     10       64.9007            -nan     0.0010    0.0104
    ##    100       64.3742            -nan     0.0010    0.0097

``` r
summary(tdboost_model, plotit=FALSE)
```

    ##               var    rel.inf
    ## 1        duration 65.4774938
    ## 2         fordald 33.5939375
    ## 3         agarald  0.9285687
    ## 4             kon  0.0000000
    ## 5     factor(zon)  0.0000000
    ## 6 factor(mcklass)  0.0000000
    ## 7 factor(bonuskl)  0.0000000

## References
