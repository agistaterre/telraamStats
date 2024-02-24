<!-- badges: start -->

[![R-CMD-check](https://github.com/agistaterre/telraamStats/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/agistaterre/telraamStats/actions/workflows/R-CMD-check.yaml) <!-- badges: end -->

# telraamStats

The aim of this package is to grant the user tools for data visualisation and data analysis of mobility data for Telraam sensors. It reuses and expands on functionnalities from this [`application`](https://agistaterre.shinyapps.io/mov-around/).

[`Link to forked Github repository`](https://github.com/KetsiaGuichard/telraamStats) [`Link to original Github repository`](https://github.com/agistaterre/mov-around)

This package is currently in a development version, feel free to contact us for more information !

# Specific Terms

Telraam data includes specific terms that are essential for further use of this package :

-   ***segment*** is the abbreviated term for a road segment. A road segment is a part of a road, defined by a set of geographical coordinate pairs and a Telraam id. These segments are defined typically between two corners of a street, so a longer street will most likely consist of multiple road segments ;
-   a ***sensor*** is the device that measures traffic in a specific segment. Multiple sensors can be associated with the same road segment, but a unique sensor is not linked to multiple segments. Sensors are also defined by their configuration, primarily their version (either V1 or S2, with S2 being the most advanced version of Telraam devices) ;
-   the ***uptime***: Telraam V1 sensors don't count trafic 100% of the time. A portion of their time is used for calculations and preprocessing. The uptime is the percentage of time during which the counter is actively counting. Data provided through the Telraam API is already corrected for this uptime but Telraam recommends keeping an eye on the uptime values. A high uptime, typically between 0.7-0.8, indicates very good data. The first and last daylight hour of the day will consistently have lower uptimes due to the aforementioned reasons, but if uptimes during the day are below 0.5, it usually indicates a potential issue with the instance. Uptime for Telraam S2 units is almost always 1.

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

Vignette (and the entire package) is currently in a development version. If you wish to review the vignette, the `build_vignettes` argument is mandatory.

# Vignettes

Once the package is installed, you can run the following command to view the first vignette :

``` r
vignette("data-details")
```

Or directly read this page : `vignette("data-details")`.

# Future Developments

Future developments will focus on data quality topics: descriptive statistics and visualizations of data quality, as well as imputation methods for data with low update frequencies (indicating low quality).
