
# waterCALendar

<!-- badges: start -->
<!-- badges: end -->

The California water year starts on October 1 and ends on September 30. 
Water years are often used in hydrology and water resource management in California. 
The waterCALendar package provides functions to convert between calendar dates and 
water calendar dates and water year types, as well as other related functionalities.

## Installation

You can install the development version of waterCALendar from [GitHub](https://github.com/danielrodonnell/waterCALendar) with:

``` r
# install.packages("remotes")
remotes::install_github("danielrodonnell/waterCALendar")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(waterCALendar)

## basic example code
year_to_wy("1997")
date_to_dowy("1997-10-01")
date_to_wyt("1997-10-01","Sacramento")
```

