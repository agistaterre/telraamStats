<!-- badges: start -->

[![R-CMD-check](https://github.com/agistaterre/telraamStats/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/agistaterre/telraamStats/actions/workflows/R-CMD-check.yaml) <!-- badges: end -->

# telraamStats

The aim of this package is to grant the user tools for data visualisation and data analysis of mobility data for Telraam sensors.

An example of using this package can be seen in this [`application`](https://agistaterre.shinyapps.io/mov-around/), although it also utilizes more advanced representations.

[`Link to Github repository`](https://github.com/agistaterre/telraamStats) 

# Telraam Sensors

[Telraam](https://telraam.net/en/what-is-telraam) is a Belgian company that offers a citizen-powered solution for gathering diverse traffic data using an affordable device. The sensor continuously monitors street activity from a citizen's window, capturing data on various transportation modes like cars, heavy vehicles, cyclists, and pedestrians. This data is crucial for informing traffic planning and engaging local communities in dialogue with authorities.

The sensors employs advanced AI and proprietary algorithms to detect, classify and count road users, providing anonymous, aggregate data with a 15-minute resolution. The device operates autonomously once installed in an upper-floor window with an unobstructed view of the street. Data collected by Telraam devices can be shared as Open Data.

For more information or to view the map of currently available sensors, you can visit the [Telraam website](https://telraam.net).

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

``` r
install.packages('telraamStats')
library(telraamStats)
```


# Vignettes

Two vignettes are currently available: 

-   one explaining the retrieval of Telraam data and describing the data included in the package (`vignette("data-details")`)
-   the second detailing the available graphical representations (`vignette("data-visualization")`).

# Future Developments

Future developments will focus on data quality topics: descriptive statistics and visualizations of data quality, as well as imputation methods for data with low `update` frequencies (indicating low quality).
