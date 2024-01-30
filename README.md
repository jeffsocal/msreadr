# msfastar <img src="man/figures/msreadr_logo.png" align="right" width="120"/>

A simplified mass-spec data reader that creates a standardized data object 
    for use among the tidyproteomics packages.

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
```

