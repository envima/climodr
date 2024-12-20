
<!-- README.md is generated from README.Rmd. Please edit that file -->

# climodr

<!-- badges: start -->
<!-- badges: end -->

The goal of climodr is to use point data from climate stations, spectral
imagery and elevation models to automatically create ready-to-use
climate maps providing an easy to use method for creating high quality
climate maps.

## Installation

You can install the development version of climodr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("envima/climodr")
```

## Example

There is an example workflow provided in the vignette of climodr, which
you cann acces via following command:

``` r
library(climodr)
#> Lade nötiges Paket: tidyverse
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
vignette("climodr")
#> Warning: Vignette 'climodr' nicht gefunden
```

For more information about the package, visit [our
website](https://envima.github.io/climodr/).
