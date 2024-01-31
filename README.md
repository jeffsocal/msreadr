# msreadr <img src="man/figures/msreadr_logo.png" align="right" width="120"/>

A simplified mass-spec data reader that creates a standardized data
object for use among the tidyproteomics packages.

## Installation

To install, open R and type:

``` r
install.packages("devtools")
devtools::install_github("jeffsocal/msreadr")
```

## Get Started

Its simple to get started, just point the `read_spectra()` function at a
downloaded MGF or mzML file and save as an object.

``` r
library(msreadr)
data <- path_to_example() |> read_spectra()
```

Examine the contents

``` r
data
```

    ## 

    ## ── R MS SPECTRA data object ──

    ## 

    ## Memory          35.58 kB 
    ## Scans           8 
    ## Precursor        
    ##   Intensity     6.5 - 8.3 (log10) 
    ##   LC time       0.29 - 0.92 (sec) 
    ##   M/Z           378.1929 - 506.757 
    ##   Z             2 
    ## 

## Extending msreader

Check out the related packages
[mspredictr](https://github.com/jeffsocal/mspredictr)
