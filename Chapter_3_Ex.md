Chapter 3 Excercises
================

Code and solutions for Chapter 3 of the [Statistical Rethinking 2
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
library(ggplot2)
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
    ## [1] ggthemes_4.2.4       rethinking_2.13      rstan_2.21.2        
    ## [4] ggplot2_3.3.5        StanHeaders_2.21.0-7
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] shape_1.4.6        tidyselect_1.1.1   xfun_0.25          purrr_0.3.4       
    ##  [5] lattice_0.20-44    V8_3.4.2           colorspace_2.0-2   vctrs_0.3.8       
    ##  [9] generics_0.1.0     htmltools_0.5.1.1  stats4_4.1.1       loo_2.4.1         
    ## [13] yaml_2.2.1         utf8_1.2.2         rlang_0.4.11       pkgbuild_1.2.0    
    ## [17] pillar_1.6.2       glue_1.4.2         withr_2.4.2        DBI_1.1.1         
    ## [21] matrixStats_0.60.0 lifecycle_1.0.0    stringr_1.4.0      munsell_0.5.0     
    ## [25] gtable_0.3.0       mvtnorm_1.1-2      coda_0.19-4        codetools_0.2-18  
    ## [29] evaluate_0.14      inline_0.3.19      knitr_1.33         callr_3.7.0       
    ## [33] ps_1.6.0           curl_4.3.2         fansi_0.5.0        Rcpp_1.0.7        
    ## [37] scales_1.1.1       RcppParallel_5.1.4 jsonlite_1.7.2     gridExtra_2.3     
    ## [41] digest_0.6.27      stringi_1.7.3      processx_3.5.2     dplyr_1.0.7       
    ## [45] grid_4.1.1         cli_3.0.1          tools_4.1.1        magrittr_2.0.1    
    ## [49] tibble_3.1.3       crayon_1.4.1       pkgconfig_2.0.3    MASS_7.3-54       
    ## [53] ellipsis_0.3.2     prettyunits_1.1.1  assertthat_0.2.1   rmarkdown_2.10    
    ## [57] rstudioapi_0.13    R6_2.5.0           compiler_4.1.1
