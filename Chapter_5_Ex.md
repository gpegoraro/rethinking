Chapter 5 Excercises
================

Code and solutions for Chapter 5 of the [Statistical Rethinking 2
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

    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
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

### 5E1

4.  mu\[i\] = A + BX \* x\[i\] + BZ \* z\[i\]

### 5E2

L\[i\] \~ Normal(mu\[i\], sigma)

mu\[i\] = A + AD \* animal\_diversity\[i\] + PD \* plant\_diversity\[i\]

## Medium

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
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] ggthemes_4.2.4       forcats_0.5.1        stringr_1.4.0       
    ##  [4] dplyr_1.0.7          purrr_0.3.4          readr_2.0.1         
    ##  [7] tidyr_1.1.3          tibble_3.1.4         tidyverse_1.3.1     
    ## [10] rethinking_2.13      rstan_2.21.2         ggplot2_3.3.5       
    ## [13] StanHeaders_2.21.0-7
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] httr_1.4.2         jsonlite_1.7.2     modelr_0.1.8       RcppParallel_5.1.4
    ##  [5] assertthat_0.2.1   stats4_4.1.1       cellranger_1.1.0   yaml_2.2.1        
    ##  [9] pillar_1.6.2       backports_1.2.1    lattice_0.20-44    glue_1.4.2        
    ## [13] digest_0.6.27      rvest_1.0.1        colorspace_2.0-2   htmltools_0.5.2   
    ## [17] pkgconfig_2.0.3    broom_0.7.9        haven_2.4.3        mvtnorm_1.1-2     
    ## [21] scales_1.1.1       processx_3.5.2     tzdb_0.1.2         generics_0.1.0    
    ## [25] ellipsis_0.3.2     withr_2.4.2        cli_3.0.1          magrittr_2.0.1    
    ## [29] crayon_1.4.1       readxl_1.3.1       evaluate_0.14      ps_1.6.0          
    ## [33] fs_1.5.0           fansi_0.5.0        MASS_7.3-54        xml2_1.3.2        
    ## [37] pkgbuild_1.2.0     tools_4.1.1        loo_2.4.1          prettyunits_1.1.1 
    ## [41] hms_1.1.0          lifecycle_1.0.0    matrixStats_0.60.1 V8_3.4.2          
    ## [45] munsell_0.5.0      reprex_2.0.1       callr_3.7.0        compiler_4.1.1    
    ## [49] rlang_0.4.11       grid_4.1.1         rstudioapi_0.13    rmarkdown_2.10    
    ## [53] gtable_0.3.0       codetools_0.2-18   inline_0.3.19      DBI_1.1.1         
    ## [57] curl_4.3.2         R6_2.5.1           gridExtra_2.3      lubridate_1.7.10  
    ## [61] knitr_1.34         fastmap_1.1.0      utf8_1.2.2         shape_1.4.6       
    ## [65] stringi_1.7.4      Rcpp_1.0.7         vctrs_0.3.8        dbplyr_2.1.1      
    ## [69] tidyselect_1.1.1   xfun_0.26          coda_0.19-4
