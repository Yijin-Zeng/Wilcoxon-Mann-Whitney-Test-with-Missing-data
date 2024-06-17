
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wmwm

<!-- badges: start -->
<!-- badges: end -->

This package includes one function `wmwm.test()`, which performs the
two-sample hypothesis test method proposed in (Zeng et al., 2024) for
univariate data when data are not fully observed. Its method is a
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
   set.seed(0)
   X <- rnorm(100,0,1)
   X[1:5] <- NA
   Y <- rnorm(50,1,1)
   Y[1:5] <- NA
   wmwm.test(X,Y)
#> $p.value
#> [1] 0.01078726
#> 
#> $bounds.statistic
#> [1] 1135 1860
#> 
#> $bounds.pvalue
#> [1] 5.331134e-08 1.078726e-02
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

## References

Zeng Y, Adams NM, Bodenham DA. On two-sample testing for data with
arbitrarily missing values. arXiv preprint arXiv:2403.15327. 2024 Mar
22.

Mann, Henry B., and Donald R. Whitney. “On a test of whether one of two
random variables is stochastically larger than the other.” The annals of
mathematical statistics (1947): 50-60.
