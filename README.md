<!-- badges: start -->

[![R-CMD-check](https://github.com/agistaterre/telraamStats/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/agistaterre/telraamStats/actions/workflows/R-CMD-check.yaml) <!-- badges: end -->

# telraamStats

The aim of this package is to grant the user tools for data visualisation and data analysis of mobility data for Telraam sensors. It reuses and expands on functionnalities from this [`application`](https://agistaterre.shinyapps.io/mov-around/).

[`Link to the Github`](https://github.com/agistaterre/mov-around)

# Licence

[![CC BY-SA 4.0](https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by-sa/4.0/)

This work is licensed under a [Creative Commons Attribution-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-sa/4.0/).

[![CC BY-SA 4.0](https://licensebuttons.net/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)

# Install Package

If you want to install this package from Github, you can use the devtools package:

``` r
if(!require(devtools)){
    install.packages("devtools")
}
devtools::install_github("https://github.com/KetsiaGuichard/telraamStats",
                              dependencies = TRUE, 
                              build_vignettes = TRUE)
```

Vignette (and the entire package) is currently in a development version. If you wish to review the vignette, the `build_vignettes` argument is mandatory. Once the package is installed, you can run the following command to view the first vignette :

``` r
vignette("data-details")
```

Or directly read this page : `vignette("data-details")`.
