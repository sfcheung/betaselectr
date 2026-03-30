# The 'vcov' Method for 'lm_betaselect' and `glm_betaselect` Objects

Compute the variance-covariance matrix of estimates in the output of
[`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
or
[`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md).

## Usage

``` r
# S3 method for class 'lm_betaselect'
vcov(
  object,
  method = c("boot", "bootstrap", "ls", "default"),
  type = c("beta", "standardized", "raw", "unstandardized"),
  warn = TRUE,
  ...
)

# S3 method for class 'glm_betaselect'
vcov(
  object,
  method = c("boot", "bootstrap", "ls", "default"),
  type = c("beta", "standardized", "raw", "unstandardized"),
  warn = TRUE,
  ...
)
```

## Arguments

- object:

  The output of
  [`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
  or an `lm_betaselect`-class object, or the output of
  [`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
  or a `glm_betaselect`-class object.

- method:

  The method used to compute the variance-covariance matrix. If
  bootstrapping was requested when calling
  [`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
  or
  [`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
  and this argument is set to `"bootstrap"` or `"boot"`, the bootstrap
  variance-covariance matrix is returned. If bootstrapping was not
  requested or if this argument is set to `"ls"` or `"default"`, then
  the usual `lm` or `glm` variance-covariance matrix is returned, with a
  warning raised unless `type` is `"raw"` or `"unstandardized".` Default
  is `"boot"`.

- type:

  String. If `"unstandardized"` or `"raw"`, the variance-covariance
  matrix of the coefficients *before* standardization are returned. If
  `"beta"` or `"standardized"`, then the variance-covariance matrix of
  the coefficients *after* selected variables standardized are returned.
  Default is `"beta"`.

- warn:

  Logical. WHether a warning will be raised is OLS (or WLS)
  variance-covariance matrix is requested for the model with some
  variables standardized (i.e., `type` is `"beta"` or `"standardized"`).
  Default is `TRUE`.

- ...:

  Other arguments to be passed to
  [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html).

## Value

A matrix of the variances and covariances of the parameter estimates.

## Details

The type of variance-covariance matrix depends on the object. If
bootstrapping was requested, by default it returns the bootstrap
variance-covariance matrix. Otherwise, it returns the default
variance-covariance matrix and raises a warning.

Support for other type of variance-covariance matrix will be added.

## See also

[`lm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)
and
[`glm_betaselect()`](https://sfcheung.github.io/betaselectr/reference/lm_betaselect.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
data(data_test_mod_cat)

# bootstrap should be set to 2000 or 5000 in real studies
lm_beta_x <- lm_betaselect(dv ~ iv*mod + cov1 + cat1,
                           data = data_test_mod_cat,
                           to_standardize = "iv",
                           do_boot = TRUE,
                           bootstrap = 100,
                           iseed = 1234)
vcov(lm_beta_x)
#>             (Intercept)          iv          mod        cov1      cat1gp2
#> (Intercept) 341529.1966 -30895.9870 -3464.959234  719.311869   6119.71357
#> iv          -30895.9870 390982.0777   418.709645 -934.754965 -11173.61930
#> mod          -3464.9592    418.7096    36.371280  -15.826380    -98.89705
#> cov1           719.3119   -934.7550   -15.826380   89.431977    110.16302
#> cat1gp2       6119.7136 -11173.6193   -98.897047  110.163020   5596.41208
#> cat1gp3        168.9982  -6374.1237   -27.706276  -11.015161   2379.59442
#> iv:mod         317.3652  -3869.0948    -4.181951    8.995417    104.91294
#>                 cat1gp3       iv:mod
#> (Intercept)   168.99821   317.365230
#> iv          -6374.12372 -3869.094815
#> mod           -27.70628    -4.181951
#> cov1          -11.01516     8.995417
#> cat1gp2      2379.59442   104.912942
#> cat1gp3      5405.70924    61.328872
#> iv:mod         61.32887    38.479356
# A warning is expected for the following call
vcov(lm_beta_x, method = "ls")
#> Warning: With standardization, the variance-covariance matrix from 'lm()' or 'glm()' should be used with caution.
#>               (Intercept)           iv           mod          cov1     cat1gp2
#> (Intercept) 384454.213030    855.64316 -3723.4966214 -1097.5954358  -632.86557
#> iv             855.643164 369545.84716    36.6391492  -270.3857355 -2377.70021
#> mod          -3723.496621     36.63915    37.3035605     0.8903597   -20.15980
#> cov1         -1097.595436   -270.38574     0.8903597   103.4952425   -33.43135
#> cat1gp2       -632.865575  -2377.70021   -20.1598049   -33.4313529  5783.03424
#> cat1gp3        474.235748  -1276.79274   -30.9821620   -35.2964697  2994.51496
#> iv:mod          -1.815315  -3681.99659    -0.4181725     2.6484043    22.01237
#>                 cat1gp3        iv:mod
#> (Intercept)   474.23575    -1.8153150
#> iv          -1276.79274 -3681.9965904
#> mod           -30.98216    -0.4181725
#> cov1          -35.29647     2.6484043
#> cat1gp2      2994.51496    22.0123669
#> cat1gp3      5643.94687    11.5915722
#> iv:mod         11.59157    36.7790296
vcov(lm_beta_x, type = "raw")
#>              (Intercept)            iv           mod        cov1     cat1gp2
#> (Intercept) 22377927.302 -1455331.0752 -224020.61098 7757.749242 89893.97909
#> iv          -1455331.075    96108.5731   14544.46771 -468.507751 -5592.14211
#> mod          -224020.611    14544.4677    2249.82334  -85.068231  -901.81123
#> cov1            7757.749     -468.5078     -85.06823   89.431977   110.16302
#> cat1gp2        89893.979    -5592.1421    -901.81123  110.163020  5596.41208
#> cat1gp3        49297.662    -3270.8129    -513.93637  -11.015161  2379.59442
#> iv:mod         14484.706     -955.2700    -145.18126    4.575244    53.41171
#>                 cat1gp3       iv:mod
#> (Intercept) 49297.66173 14484.705612
#> iv          -3270.81287  -955.269975
#> mod          -513.93637  -145.181261
#> cov1          -11.01516     4.575244
#> cat1gp2      2379.59442    53.411713
#> cat1gp3      5405.70924    32.167824
#> iv:mod         32.16782     9.524374



data_test_mod_cat$p <- scale(data_test_mod_cat$dv)[, 1]
data_test_mod_cat$p <- ifelse(data_test_mod_cat$p > 0,
                              yes = 1,
                              no = 0)
# bootstrap should be set to 2000 or 5000 in real studies
logistic_beta_x <- glm_betaselect(p ~ iv*mod + cov1 + cat1,
                                  data = data_test_mod_cat,
                                  family = binomial,
                                  to_standardize = "iv",
                                  do_boot = TRUE,
                                  bootstrap = 100,
                                  iseed = 1234)
vcov(logistic_beta_x)
#>              (Intercept)           iv           mod          cov1       cat1gp2
#> (Intercept)  8.797812807  1.062150602 -8.713790e-02 -0.0018071544  0.0526904870
#> iv           1.062150602 16.529230960 -8.734895e-03 -0.0352498781 -0.0484103318
#> mod         -0.087137902 -0.008734895  8.837247e-04 -0.0001470441 -0.0010337441
#> cov1        -0.001807154 -0.035249878 -1.470441e-04  0.0017832978  0.0004442933
#> cat1gp2      0.052690487 -0.048410332 -1.033744e-03  0.0004442933  0.0842637108
#> cat1gp3     -0.008942157 -0.128293611 -9.921562e-05 -0.0017746214  0.0289331357
#> iv:mod      -0.011914407 -0.166098036  1.004560e-04  0.0003511092  0.0004519368
#>                   cat1gp3        iv:mod
#> (Intercept) -8.942157e-03 -0.0119144070
#> iv          -1.282936e-01 -0.1660980361
#> mod         -9.921562e-05  0.0001004560
#> cov1        -1.774621e-03  0.0003511092
#> cat1gp2      2.893314e-02  0.0004519368
#> cat1gp3      7.354171e-02  0.0012905037
#> iv:mod       1.290504e-03  0.0016728656
# A warning is expected for the following call
vcov(logistic_beta_x, method = "default")
#> Warning: With standardization, the variance-covariance matrix from 'lm()' or 'glm()' should be used with caution.
#>              (Intercept)           iv           mod          cov1       cat1gp2
#> (Intercept)  7.012135227  0.411643579 -6.900526e-02 -1.333292e-02  0.0059333119
#> iv           0.411643579 10.675506213 -4.381053e-03 -5.439836e-03 -0.0331613460
#> mod         -0.069005263 -0.004381053  6.977811e-04 -1.373419e-05 -0.0004708791
#> cov1        -0.013332920 -0.005439836 -1.373419e-05  1.508938e-03 -0.0003525212
#> cat1gp2      0.005933312 -0.033161346 -4.708791e-04 -3.525212e-04  0.0864200443
#> cat1gp3      0.029765007 -0.040425854 -6.849103e-04 -5.964737e-04  0.0450139761
#> iv:mod      -0.005485693 -0.107183568  5.816087e-05  5.175472e-05  0.0003017852
#>                   cat1gp3        iv:mod
#> (Intercept)  0.0297650073 -5.485693e-03
#> iv          -0.0404258542 -1.071836e-01
#> mod         -0.0006849103  5.816087e-05
#> cov1        -0.0005964737  5.175472e-05
#> cat1gp2      0.0450139761  3.017852e-04
#> cat1gp3      0.0857890925  3.923683e-04
#> iv:mod       0.0003923683  1.079343e-03
vcov(logistic_beta_x, type = "raw")
#>             (Intercept)           iv          mod          cov1       cat1gp2
#> (Intercept) 911.3586621 -60.75224666 -9.167686512  0.2650278872  0.4042298256
#> iv          -60.7522467   4.08971442  0.611749464 -0.0177351424 -0.0237022676
#> mod          -9.1676865   0.61174946  0.092432484 -0.0028141215 -0.0042640760
#> cov1          0.2650279  -0.01773514 -0.002814122  0.0017832978  0.0004442933
#> cat1gp2       0.4042298  -0.02370227 -0.004264076  0.0004442933  0.0842637108
#> cat1gp3       0.9981170  -0.06700826 -0.010255653 -0.0017746214  0.0289331357
#> iv:mod        0.6098632  -0.04109360 -0.006153992  0.0001768950  0.0002190784
#>                  cat1gp3        iv:mod
#> (Intercept)  0.998116981  0.6098631875
#> iv          -0.067008258 -0.0410935984
#> mod         -0.010255653 -0.0061539924
#> cov1        -0.001774621  0.0001768950
#> cat1gp2      0.028933136  0.0002190784
#> cat1gp3      0.073541707  0.0006774900
#> iv:mod       0.000677490  0.0004137834
```
