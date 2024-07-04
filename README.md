
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wmwm

<!-- badges: start -->
<!-- badges: end -->

This package includes one function `wmwm.test()`, which performs the
two-sample hypothesis test method proposed in (Zeng et al., 2024) for
univariate data when data are not fully observed. This method is a
theoretical extension of Wilcoxon-Mann-Whitney test in the presence of
missing data, which controls the Type I error regardless of values of
missing data.

Bounds of the Wilcoxon-Mann-Whitne test statistic and its p-value will
be computed in the presence of missing data. The p-value of the test
method proposed in (Zeng et al., 2024) is then returned as the maximum
possible p-value of the Wilcoxon-Mann-Whitney test.

## Installation

You can install the development version of wmwm from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Yijin-Zeng/Wilcoxon-Mann-Whitney-Test-with-Missing-data")
```

## Example

This is a basic example which shows you how to perform the test with
missing data:

``` r
library(wmwm)

#### Assume all samples are distinct.
X <- c(6.2, 3.5, NA, 7.6, 9.2)
Y <- c(0.2, 1.3, -0.5, -1.7)
## By default, when the sample sizes of both X and Y are smaller than 50,
## exact distribution will be used.
wmwm.test(X, Y, ties = FALSE, alternative = 'two.sided')
#> $p.value
#> [1] 0.1904762
#> 
#> $bounds.statistic
#> [1] 16 20
#> 
#> $bounds.pvalue
#> [1] 0.01587302 0.19047619
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> $ties.method
#> [1] FALSE
#> 
#> $description.bounds
#> [1] "bounds.pvalue is the bounds of the exact p-value"
#> 
#> $data.name
#> [1] "X and Y"
```

``` r

## using normality approximation with continuity correction:
wmwm.test(X, Y, ties = FALSE, alternative = 'two.sided', exact = FALSE, correct = TRUE)
#> $p.value
#> [1] 0.1779096
#> 
#> $bounds.statistic
#> [1] 16 20
#> 
#> $bounds.pvalue
#> [1] 0.01996445 0.17790959
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> $ties.method
#> [1] FALSE
#> 
#> $description.bounds
#> [1] "bounds.pvalue is the bounds of the p-value obtained using normal approximation with continuity correction"
#> 
#> $data.name
#> [1] "X and Y"
```

``` r

#### Assume samples can be tied.
X <- c(6, 9, NA, 7, 9)
Y <- c(0, 1, 0, -1)
## When the samples can be tied, normality approximation will be used.
## By default, lower.boundary = -Inf, upper.boundary = Inf.
wmwm.test(X, Y, ties = TRUE, alternative = 'two.sided')
#> Warning in boundsPValueWithTies(X, Y, alternative = alternative, lower.boundary
#> = lower.boundary, : cannot bound exact p-value with ties
#> $p.value
#> [1] 0.174277
#> 
#> $bounds.statistic
#> [1] 16 20
#> 
#> $bounds.pvalue
#> [1] 0.01745104 0.17427702
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> $ties.method
#> [1] TRUE
#> 
#> $description.bounds
#> [1] "bounds.pvalue is the bounds of the p-value obtained using normal approximation with continuity correction"
#> 
#> $data.name
#> [1] "X and Y"
```

``` r

## specifying lower.boundary and upper.boundary:
wmwm.test(X, Y, ties = TRUE, alternative = 'two.sided', lower.boundary = -1, upper.boundary = 9)
#> Warning in boundsPValueWithTies(X, Y, alternative = alternative, lower.boundary
#> = lower.boundary, : cannot bound exact p-value with ties
#> $p.value
#> [1] 0.1383146
#> 
#> $bounds.statistic
#> [1] 16.5 20.0
#> 
#> $bounds.pvalue
#> [1] 0.01745104 0.13831461
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> $ties.method
#> [1] TRUE
#> 
#> $description.bounds
#> [1] "bounds.pvalue is the bounds of the p-value obtained using normal approximation with continuity correction"
#> 
#> $data.name
#> [1] "X and Y"
```

## See Also

The R function `stats::wilcox.test()` executes Wilcoxon-Mann-Whitney
two-sample when all samples are observed.

## References

Zeng Y, Adams NM, Bodenham DA. On two-sample testing for data with
arbitrarily missing values. arXiv preprint arXiv:2403.15327. 2024 Mar
22.

Mann, Henry B., and Donald R. Whitney. “On a test of whether one of two
random variables is stochastically larger than the other.” The annals of
mathematical statistics (1947): 50-60.
