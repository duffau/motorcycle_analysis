Motorcycle claims - Wasa insurance 1994 to 1998
================
Christian Duffau-Rasmussen
2018-09-29

``` r
library(ggplot2)
library(insuranceData)
data(dataOhlsson)
```

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
  - `skadkost` the claim cost, a numeric vector

<!-- end list -->

``` r
dataOhlsson <- dataOhlsson[dataOhlsson$skadkost>0, ]
```

Subsetting the dataset to only include policies with positive claims,
reduces the dataset from 64548 rows to only 670 rows.

``` r
ggplot(dataOhlsson, aes(skadkost, color=kon, fill=kon)) +
  geom_density(alpha=0.2)
```

![](analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> Comparing
the distribution of the realized claims, woman have more small claims
than men and the male claims has heavier
tails.

``` r
ggplot(dataOhlsson, aes(skadkost, color=as.factor(mcklass), fill=as.factor(mcklass))) +
  geom_density(position='stack')
```

![](analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
