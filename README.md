
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wmwm

<!-- badges: start -->
<!-- badges: end -->

This package wmwm is to perform the two-sample Wilcoxon-Mann-Whitney
test in the presence of missing data. The Wilcoxon-Mann-Whitney test,
also known as Wilcoxon rank sum test or Mann-Whitney U test, is a
non-parametric two-sample hypothesis testing method. Suppose `X` and `Y`
are sets of univariate, distinct, real-valued samples assumed randomly
selected from two populations. The null hypothesis of the test is that
the two populations are the same. If both `X` and `Y` are fully
observed, stats::wilcox.test() in R is for the Wilcoxon-Mann-Whitney
test.

While only subsets of `X` and `Y` are observed, denote observed samples
in `X` and `Y` as `X'` and `Y'`, respectively. The Wilcoxon-Mann-Whitney
test can not be carried out directly. The default method in
stats::wilcox.test() is to ignore all unobserved samples and perform the
test between `X'` and `Y'`. Unfortunately, this practice of ignoring
unobserved samples is rarely valid and can potentially leads to a higher
type I error than a pre-specified significance level.

This package provides a method for conducting two-sample
Wilcoxon-Mann-Whitney test in the presence of missing data by computing
the bounds of the Wilcoxon-Mann-Whitney test statistic and p-values.
Then, the null hypothesis should be rejected if all test statistics are
significant, alternatively, if all p-values are smaller than a
pre-specified significance level. This method controls the type I error
under a pre-specified significance level regardless of the values of
missing data. This package also allows the test if `X` and `Y` are
multisets consisting of real-valued, potentially tied samples.

## Installation

You can install the development version of wmwm from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Yijin-Zeng/Wilcoxon-Mann-Whitney-Test-with-Missing-data")
```

## Example

As a very simple example, the following shows how we can perform the
Wilcoxon-Mann-Whitney test in the presence of missing data by computing
the bounds of the statistic and the p-value. To start, let us randomly
generate our samples.

``` r
   library(wmwm)
   set.seed(1) 
   ## tested samples
   X <- stats::rnorm(100,0,1)
   X[1:5] <- NA      
   Y <- stats::rnorm(60,1,1)
   Y[1:5] <- NA
   X[1:6]
#> [1]         NA         NA         NA         NA         NA -0.8204684
```

``` r
   Y[1:6]
#> [1]       NA       NA       NA       NA       NA 2.767287
```

In `X` and `Y`, each unobserved sample is shown as NA. Suppose a
two-sided Wilcoxon-Mann-Whitney test between `X` and `Y` with a
significance level $\alpha = 0.05$ is of interest. The testing can be
done by runing:

``` r
   wmwm.test(X,Y)
#> $p.value
#> [1] 0.01494568
#> 
#> $bounds.statistic
#> [1] 1534 2309
#> 
#> $bounds.pvalue
#> [1] 2.401707e-07 1.494568e-02
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> $ties.method
#> [1] FALSE
#> 
#> $description.bounds
#> [1] "bounds.pvalue is the bounds of the p-value obtained using normal approximation with continuity correction when data are not missing."
#> 
#> $data.name
#> [1] "X and Y"
```

Subsequently, since `bounds.pvalue` shows the p-value is bounded lower
than 0.0149, which is smaller than the pre-specified significance level
$\alpha = 0.05$, the null hypothesis should then be rejected.

Additionally, the `bounds.statistic` gives the bounds of the Wilcoxon
test statistic; `alternative` specifies the alternative hypothesis;
`description.bounds` describes the assumptions underlying the bounds.

## Optional choices

This function provides other options of the bounds. For example, instead
of bounding the p-value using normal approximation, we may want to bound
the exact p-value. This can be done by running:

``` r
   wmwm.test(X,Y, exact = TRUE)
#> $p.value
#> [1] 0.01464088
#> 
#> $bounds.statistic
#> [1] 1534 2309
#> 
#> $bounds.pvalue
#> [1] 1.125866e-07 1.464088e-02
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> $ties.method
#> [1] FALSE
#> 
#> $description.bounds
#> [1] "bounds.pvalue is the bounds of the exact p-value when data are not missing"
#> 
#> $data.name
#> [1] "X and Y"
```

In some cases, we need to deal with tied samples, i.e. two or more
observations may be equal. This function deal with ties using mid-rank
practice automatically if observed samples containing ties observations.

``` r
   X <- stats::rpois(100,1)
   X[1:5] <- NA      
   Y <- stats::rpois(60,2)
   Y[1:5] <- NA
   X[1:6]
#> [1] NA NA NA NA NA  3
```

``` r
   Y[1:6]
#> [1] NA NA NA NA NA  1
```

``` r
   wmwm.test(X,Y)
#> $p.value
#> [1] 0.1351375
#> 
#> $bounds.statistic
#> [1] 1811.5 2586.5
#> 
#> $bounds.pvalue
#> [1] 1.434931e-05 1.351375e-01
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> $ties.method
#> [1] TRUE
#> 
#> $description.bounds
#> [1] "bounds.pvalue is the bounds of the p-value obtained using normal approximation with continuity correction when data are not missing."
#> 
#> $data.name
#> [1] "X and Y"
```

If we know that the unobserved samples can not be smaller than the
minimum of observed samples, (in this case, can not smaller than 0), we
can specify `lower.boundary = 'equal'` to make the bounds potentially
tighter.

``` r
   min(c(X[!is.na(X)], Y[!is.na(Y)]))
#> [1] 0
```

``` r
   wmwm.test(X,Y,lower.boundary = 'equal')
#> Warning in boundsSumRank(X, Y, ties, lower.boundary, upper.boundary):
#> lower.boundary must be smaller or equal than the minimum of all observed data.
#> lower.boundary is set to -Inf
#> $p.value
#> [1] 0.1351375
#> 
#> $bounds.statistic
#> [1] 1811.5 2586.5
#> 
#> $bounds.pvalue
#> [1] 1.434931e-05 1.351375e-01
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> $ties.method
#> [1] TRUE
#> 
#> $description.bounds
#> [1] "bounds.pvalue is the bounds of the p-value obtained using normal approximation with continuity correction when data are not missing."
#> 
#> $data.name
#> [1] "X and Y"
```

## References

Mann, Henry B., and Donald R. Whitney. “On a test of whether one of two
random variables is stochastically larger than the other.” The annals of
mathematical statistics (1947): 50-60.

Lehmann, Erich Leo, and Howard J. D’Abrera. Nonparametrics: statistical
methods based on ranks. Holden-day, 1975.

Schafer, Joseph L., and John W. Graham. “Missing data: our view of the
state of the art.” Psychological methods 7.2 (2002): 147.
