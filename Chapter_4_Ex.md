Chapter 4 Excercises
================

Code and solutions for Chapter 4 of the [Statistical Rethinking 2
Ed.](https://xcelab.net/rm/statistical-rethinking/) textbook by R.
McElreath.

``` r
library(rethinking)
```

    ## Loading required package: rstan

    ## Loading required package: StanHeaders

    ## Loading required package: ggplot2

    ## rstan (Version 2.21.2, GitRev: 2e1f913d3ca3)

    ## For execution on a local, multicore CPU with excess RAM we recommend calling
    ## options(mc.cores = parallel::detectCores()).
    ## To avoid recompilation of unchanged Stan programs, we recommend calling
    ## rstan_options(auto_write = TRUE)

    ## Loading required package: parallel

    ## rethinking (Version 2.13)

    ## 
    ## Attaching package: 'rethinking'

    ## The following object is masked from 'package:stats':
    ## 
    ##     rstudent

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ tibble  3.1.3     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1
    ## ✓ purrr   0.3.4

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x tidyr::extract() masks rstan::extract()
    ## x dplyr::filter()  masks stats::filter()
    ## x dplyr::lag()     masks stats::lag()
    ## x purrr::map()     masks rethinking::map()

``` r
library(ggthemes)
```

Set the palette and the running theme for ggplot2.

``` r
theme_set(theme_bw())
theme_update(axis.text.x = element_text(
angle = -45,
hjust = 0,
vjust = 0.5
))
```

## Easy

### 4E1

Line 1

### 4E2

2

### 4E3

$$ Pr(mu, sigma\\\|h) = Product-i(Normal(h-i\\\|mu, sigma)\\\*Normal(mu\\\| 0, 10)\\\*Exponential(sigma\\\| 1)) \\\\ Integral Integral Product-i(Normal(h-i\\\|mu, sigma)\\\*Normal(mu\\\| 0, 10)\\\*Exponential(sigma\\\| 1)) dmu dgamma $$

### 4E4

Line 2

### 4E5

3

## Medium

### 4M1

``` r
set.seed(42)

n_samples <- 1e4

ex4M1 <- tibble(
  mu = rnorm(n_samples, 0, 10),
  sigma = rexp(n_samples, 1)
) %>%
  mutate(y = rnorm(n_samples, mu, sigma))

glimpse(ex4M1)
```

    ## Rows: 10,000
    ## Columns: 3
    ## $ mu    <dbl> 13.7095845, -5.6469817, 3.6312841, 6.3286260, 4.0426832, -1.0612…
    ## $ sigma <dbl> 0.29428439, 0.26763543, 1.07316088, 1.93306396, 0.52182603, 0.72…
    ## $ y     <dbl> 13.6839959, -5.6557772, 2.2124824, 6.5303356, 5.2047443, -2.6241…

``` r
ex4M1 %>%
  ggplot(aes(x = y)) +
  geom_density()
```

![](Chapter_4_Ex_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### 4M2

``` r
ex4M2_formula <- alist(
  y ~ dnorm(mu, sigma),
  mu ~ dnorm(0, 10),
  sigma ~ dexp(1)
)
```

### 4M3

y-i \~ Normal(mu-i, sigma)

mu-i = a + b \* w-i

a \~ Normal(0, 10)

b \~ Uniform(0, 1)

sigma \~ Exponential(1)

### 4M4

height-i \~ Normal(mu-i, sigma)

mu-i = a + b \* year-i

a = Normal(178, 20)

b = Log-Normal(0, 1)

sigma = Uniform(0, 50)

### 4M5

Same as 4M4

### 4M6

height-i \~ Normal(mu-i, sigma)

mu-i = a + b \* year-i

a = Normal(178, 20)

b = Log-Normal(0, 1)

sigma = Uniform(0, 10)

### 4M7

``` r
data(Howell1)
d <- tibble(Howell1)

d2 <- d %>%
  filter(age >= 18) %>%
  mutate(weight_centered = weight - mean(weight))

m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b* weight_centered,
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0,50)), 
  data = d2)

m4.3bis <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b* weight,
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0,50)),
  data = d2)
```

``` r
precis(m4.3)
```

    ##              mean         sd        5.5%       94.5%
    ## a     154.6018820 0.27032596 154.1698489 155.0339151
    ## b       0.9032777 0.04192646   0.8362711   0.9702843
    ## sigma   5.0722235 0.19118707   4.7666696   5.3777773

``` r
round(vcov(m4.3), 3)
```

    ##           a     b sigma
    ## a     0.073 0.000 0.000
    ## b     0.000 0.002 0.000
    ## sigma 0.000 0.000 0.037

``` r
precis(m4.3bis)
```

    ##              mean         sd        5.5%       94.5%
    ## a     114.5341918 1.89775429 111.5012139 117.5671697
    ## b       0.8907349 0.04175814   0.8239973   0.9574724
    ## sigma   5.0727384 0.19125078   4.7670827   5.3783941

``` r
round(vcov(m4.3bis), 3)
```

    ##            a      b sigma
    ## a      3.601 -0.078 0.009
    ## b     -0.078  0.002 0.000
    ## sigma  0.009  0.000 0.037

``` r
n_samples <- 1e4
n_grid <- 100

samples_4_3 <- tibble(extract.samples(m4.3, n_samples)) 
samples_4_3b <- tibble(extract.samples(m4.3bis, n_samples))

grid_4_3 <- seq(from = min(d2$weight_centered), 
                to = max(d2$weight_centered), 
                length.out = n_grid)

grid_4_3b <- seq(from = min(d2$weight), 
                 to = max(d2$weight), 
                 length.out = n_grid)

params_mean_4_3 <- samples_4_3 %>%
  summarise(across(everything(), mean))

params_mean_4_3b <- samples_4_3b %>%
  summarise(across(everything(), mean))
```

``` r
d2 %>%
  ggplot(aes(x = weight_centered, y = height)) +
  geom_point(shape = 21,
             color = "navyblue") +
  geom_abline(data = params_mean_4_3,
              aes(slope = b,
                  intercept = a))
```

![](Chapter_4_Ex_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
d2 %>%
  ggplot(aes(x = weight, y = height)) +
  geom_point(shape = 21,
             color = "navyblue") +
  geom_abline(data = params_mean_4_3b,
              aes(slope = b,
                  intercept = a)) +
  coord_cartesian(xlim = c(-1, 70), ylim = c(-1, 185))
```

![](Chapter_4_Ex_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
mu_4_3 <- samples_4_3 %>%
  mutate(mu = map2(a, b, ~ .x + .y * grid_4_3),
         weight_centered = list(grid_4_3)) %>%
  unnest(c("mu", "weight_centered")) %>%
  group_by(weight_centered) %>%
  summarise(mu_mean = mean(mu),
            mu_HPDI_89 = list(HPDI(mu))) %>%
  unnest_wider(mu_HPDI_89)

mu_4_3
```

    ## # A tibble: 100 × 4
    ##    weight_centered mu_mean `|0.89` `0.89|`
    ##              <dbl>   <dbl>   <dbl>   <dbl>
    ##  1           -13.9    142.    141.    143.
    ##  2           -13.6    142.    141.    143.
    ##  3           -13.3    143.    142.    144.
    ##  4           -13.0    143.    142.    144.
    ##  5           -12.6    143.    142.    144.
    ##  6           -12.3    143.    143.    144.
    ##  7           -12.0    144.    143.    145.
    ##  8           -11.7    144.    143.    145.
    ##  9           -11.3    144.    143.    145.
    ## 10           -11.0    145.    144.    145.
    ## # … with 90 more rows

``` r
mu_4_3 %>%
  ggplot(aes(x = weight_centered,
             y = mu_mean)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = `|0.89`,
                  ymax = `0.89|`),
              fill = "grey70",
              alpha = 0.3) +
  geom_point(data = d2,
             aes(x = weight_centered,
                 y = height),
             color = "navyblue",
             shape = 21) +
  labs(x = "Weight - Mean Weight",
       y = "Height")
```

![](Chapter_4_Ex_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
mu_4_3b <- samples_4_3b %>%
  mutate(mu = map2(a, b, ~ .x + .y * grid_4_3b),
         weight = list(grid_4_3b)) %>%
  unnest(c("mu", "weight")) %>%
  group_by(weight) %>%
  summarise(mu_mean = mean(mu),
            mu_HPDI_89 = list(HPDI(mu))) %>%
  unnest_wider(mu_HPDI_89)

mu_4_3b
```

    ## # A tibble: 100 × 4
    ##    weight mu_mean `|0.89` `0.89|`
    ##     <dbl>   <dbl>   <dbl>   <dbl>
    ##  1   31.1    142.    141.    143.
    ##  2   31.4    143.    141.    143.
    ##  3   31.7    143.    142.    144.
    ##  4   32.0    143.    142.    144.
    ##  5   32.4    143.    142.    144.
    ##  6   32.7    144.    143.    145.
    ##  7   33.0    144.    143.    145.
    ##  8   33.3    144.    143.    145.
    ##  9   33.7    145.    144.    145.
    ## 10   34.0    145.    144.    146.
    ## # … with 90 more rows

``` r
mu_4_3b %>%
  ggplot(aes(x = weight,
             y = mu_mean)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = `|0.89`,
                  ymax = `0.89|`),
              fill = "grey70",
              alpha = 0.3) +
  geom_point(data = d2,
             aes(x = weight,
                y = height),
             color = "navyblue",
             shape = 21) +
  labs(x = "Weight",
       y = "Height")
```

![](Chapter_4_Ex_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

### 4M8

``` r
data("cherry_blossoms")
d <- cherry_blossoms
precis(d)
```

    ##                   mean          sd      5.5%      94.5%       histogram
    ## year       1408.000000 350.8845964 867.77000 1948.23000   ▇▇▇▇▇▇▇▇▇▇▇▇▁
    ## doy         104.540508   6.4070362  94.43000  115.00000        ▁▂▅▇▇▃▁▁
    ## temp          6.141886   0.6636479   5.15000    7.29470        ▁▃▅▇▃▂▁▁
    ## temp_upper    7.185151   0.9929206   5.89765    8.90235 ▁▂▅▇▇▅▂▂▁▁▁▁▁▁▁
    ## temp_lower    5.098941   0.8503496   3.78765    6.37000 ▁▁▁▁▁▁▁▃▅▇▃▂▁▁▁

``` r
d2 <- d[complete.cases(d$doy),]
num_knots <- 15
knot_list <- quantile(d2$year, 
                      probs = seq(0, 1, length.out = num_knots))
```

``` r
library(splines)
B <- bs(d2$year,
        knots = knot_list[-c(1, num_knots)],
        degree = 3,
        intercept = TRUE)

str(B)
```

    ##  'bs' num [1:827, 1:17] 1 0.96 0.767 0.563 0.545 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : NULL
    ##   ..$ : chr [1:17] "1" "2" "3" "4" ...
    ##  - attr(*, "degree")= int 3
    ##  - attr(*, "knots")= Named num [1:13] 1036 1174 1269 1377 1454 ...
    ##   ..- attr(*, "names")= chr [1:13] "7.142857%" "14.28571%" "21.42857%" "28.57143%" ...
    ##  - attr(*, "Boundary.knots")= int [1:2] 812 2015
    ##  - attr(*, "intercept")= logi TRUE

``` r
m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(100, 10),
    w ~ dnorm(0,2),
    sigma ~ dexp(1)
  ), 
  data = list(D = d2$doy, B = B),
  start = list(w = rep(0, ncol(B)))
)
```

``` r
post <- extract.samples(m4.7)
precis(post, depth = 2)
```

    ##              mean        sd        5.5%       94.5%      histogram
    ## a     103.8596658 0.5433430 102.9897199 104.7346295     ▁▁▁▃▇▅▂▁▁▁
    ## sigma   5.9397246 0.1476746   5.7029595   6.1783664   ▁▁▁▃▇▇▅▂▁▁▁▁
    ## w[1]   -1.3154016 1.6786415  -3.9899335   1.3379156 ▁▁▁▁▃▅▇▇▃▂▁▁▁▁
    ## w[2]   -1.4661625 1.5694006  -3.9724322   1.0352149  ▁▁▁▁▃▇▇▇▃▁▁▁▁
    ## w[3]   -0.2032329 1.5071438  -2.6173761   2.2029279   ▁▁▁▂▅▇▇▃▂▁▁▁
    ## w[4]    2.5414788 1.1855263   0.6393004   4.4560277      ▁▁▂▅▇▅▂▁▁
    ## w[5]    0.4828156 1.2347087  -1.4772198   2.4744853    ▁▁▁▂▅▇▅▂▁▁▁
    ## w[6]    1.0152023 1.2868580  -1.0478919   3.0729762     ▁▁▁▃▇▇▃▁▁▁
    ## w[7]   -2.5862195 1.2214097  -4.5342532  -0.6316053     ▁▁▁▂▅▇▅▂▁▁
    ## w[8]    4.0704904 1.2060234   2.1322285   5.9849424   ▁▁▁▁▃▇▇▃▁▁▁▁
    ## w[9]    0.3403781 1.2706102  -1.6893277   2.3651984    ▁▁▁▂▇▇▅▂▁▁▁
    ## w[10]   1.7654467 1.2796850  -0.2960436   3.7863244     ▁▁▂▅▇▇▃▁▁▁
    ## w[11]   3.0170059 1.2487065   1.0386183   5.0358787     ▁▁▁▃▇▇▃▁▁▁
    ## w[12]   0.5578369 1.2501121  -1.4427761   2.5283057    ▁▁▁▂▅▇▅▂▁▁▁
    ## w[13]   3.2658955 1.2652715   1.2786743   5.3027765     ▁▁▁▃▇▇▅▂▁▁
    ## w[14]   0.9324866 1.2818136  -1.1098966   2.9648635   ▁▁▁▁▃▇▇▃▁▁▁▁
    ## w[15]  -2.0558482 1.4080485  -4.3337337   0.2017741   ▁▁▁▂▅▇▇▃▂▁▁▁
    ## w[16]  -5.1410941 1.4743632  -7.4882046  -2.7685824   ▁▁▁▂▅▇▇▃▂▁▁▁
    ## w[17]  -4.9134680 1.5006038  -7.3449007  -2.5271444  ▁▁▁▂▅▇▇▅▂▁▁▁▁

``` r
str(post)
```

    ## List of 3
    ##  $ a    : num [1:10000] 104 104 104 104 104 ...
    ##  $ sigma: num [1:10000] 5.72 5.63 5.99 5.9 6.02 ...
    ##  $ w    : num [1:10000, 1:17] -1.452 -2.387 -0.79 -0.578 -2.658 ...
    ##  - attr(*, "source")= chr "quap posterior: 10000 samples from m4.7"

``` r
w <- apply(post$w, 2, mean)
plot(NULL, xlim = range(d2$year), ylim = c(-6,6),
     xlab = "year", ylab = "basis * weight")
for(i in 1:ncol(B)){
  lines(d2$year, w[i]*B[,i])
}
```

![](Chapter_4_Ex_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
mu <- link(m4.7)
mu_PI <- apply(mu, 2, PI, 0.97)

plot(d2$year, 
     d2$doy, 
     col = col.alpha(rangi2, 0.3),
     pch = 16)
shade(mu_PI, 
      d2$year,
      col = col.alpha("black", 0.5))
```

![](Chapter_4_Ex_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

## Hard

Document the information about the analysis session

``` r
sessionInfo()
```

    ## R version 4.1.1 (2021-08-10)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Big Sur 10.16
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] splines   parallel  stats     graphics  grDevices utils     datasets 
    ## [8] methods   base     
    ## 
    ## other attached packages:
    ##  [1] ggthemes_4.2.4       forcats_0.5.1        stringr_1.4.0       
    ##  [4] dplyr_1.0.7          purrr_0.3.4          readr_2.0.1         
    ##  [7] tidyr_1.1.3          tibble_3.1.3         tidyverse_1.3.1     
    ## [10] rethinking_2.13      rstan_2.21.2         ggplot2_3.3.5       
    ## [13] StanHeaders_2.21.0-7
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] httr_1.4.2         jsonlite_1.7.2     modelr_0.1.8       RcppParallel_5.1.4
    ##  [5] assertthat_0.2.1   highr_0.9          stats4_4.1.1       cellranger_1.1.0  
    ##  [9] yaml_2.2.1         pillar_1.6.2       backports_1.2.1    lattice_0.20-44   
    ## [13] glue_1.4.2         digest_0.6.27      rvest_1.0.1        colorspace_2.0-2  
    ## [17] htmltools_0.5.1.1  pkgconfig_2.0.3    broom_0.7.9        haven_2.4.3       
    ## [21] mvtnorm_1.1-2      scales_1.1.1       processx_3.5.2     tzdb_0.1.2        
    ## [25] farver_2.1.0       generics_0.1.0     ellipsis_0.3.2     withr_2.4.2       
    ## [29] cli_3.0.1          magrittr_2.0.1     crayon_1.4.1       readxl_1.3.1      
    ## [33] evaluate_0.14      ps_1.6.0           fs_1.5.0           fansi_0.5.0       
    ## [37] MASS_7.3-54        xml2_1.3.2         pkgbuild_1.2.0     tools_4.1.1       
    ## [41] loo_2.4.1          prettyunits_1.1.1  hms_1.1.0          lifecycle_1.0.0   
    ## [45] matrixStats_0.60.0 V8_3.4.2           munsell_0.5.0      reprex_2.0.1      
    ## [49] callr_3.7.0        compiler_4.1.1     rlang_0.4.11       grid_4.1.1        
    ## [53] rstudioapi_0.13    labeling_0.4.2     rmarkdown_2.10     gtable_0.3.0      
    ## [57] codetools_0.2-18   inline_0.3.19      DBI_1.1.1          curl_4.3.2        
    ## [61] R6_2.5.0           gridExtra_2.3      lubridate_1.7.10   knitr_1.33        
    ## [65] utf8_1.2.2         shape_1.4.6        stringi_1.7.3      Rcpp_1.0.7        
    ## [69] vctrs_0.3.8        dbplyr_2.1.1       tidyselect_1.1.1   xfun_0.25         
    ## [73] coda_0.19-4