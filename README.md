
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MCERA5

<!-- badges: start -->
<!-- badges: end -->

‘MCERA5’ is designed to simplify the download and analysis of the
European Centre for Medium-range Weather Forecasts ReAnalysis 5 (ERA5)
family. It simplifies the downloading of ERA5 datasets by summarizing

It uses the European Centre for Medium-Range Weather Forecasts [`ecmwfr`
package](https://github.com/bluegreen-labs/ecmwfr) in `R` to interface
the ECMWFR API web service and access the ERA5 data from the [Copernicus
Climate Data Store
(CDS)](https://cds.climate.copernicus.eu/cdsapp#!/home).

## Installation

‘MCERA5’ is not yet on CRAN, so it needs to be installed via GitHub. You
can install the latest development version of ‘MCERA5’ like this:

``` r
remotes::install_github("marlenebauer/MCERA5")
#> Skipping install of 'MCERA5' from a github remote, the SHA1 (a2ff2d29) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

Users require a user ID and API key to access the Copernicus Store Data
(CDS) API which can be found at their [CDS
profile](https://cds.climate.copernicus.eu/user/login).

Additionally, you need to download the ‘rgeoboundaries’ package from
GitHub to exploit the entire capability of ‘MCERA5’:

``` r
remotes::install_github("wmgeolab/rgeoboundaries")
#> Skipping install of 'rgeoboundaries' from a github remote, the SHA1 (ecb0269e) has not changed since last install.
#>   Use `force = TRUE` to force installation
library(rgeoboundaries)
```

## Get started

After successfully installed, you can load the package as below and work
with the provided functions.

``` r
user <- "Your user ID"
key <- " Your personal API key"
dataset_short_name <- "reanalysis-era5-land-monthly-means"
product_type <- "monthly_averaged_reanalysis"
variable <- "2m_temperature"
StartDate <- "2009-12-01"
EndDate <- "2020-02-29"
season <- "DJF"
time = sprintf("%02d:00", 4:5) # c("04:00", "05:00")
country_name <- "Germany" # provided by the 'rgeoboundaries' package
# call the get_era5 function
# optional parameters need to be called as shown below
data <- get_era5(user, key, dataset_short_name, variable, StartDate, EndDate, time, season=season, country_name=country_name, product_type=product_type)
```

An overview of the ERA5 family datasets with all available
dataset_short_names and product_types is provided
[here](https://confluence.ecmwf.int/display/CKB/Climate+Data+Store+%28CDS%29+API+Keywords#ClimateDataStore(CDS)APIKeywords-ERA5familydatasets).
A list of all available variables for ERA5-Land can be found
[here](https://confluence.ecmwf.int/display/CKB/ERA5-Land%3A+data+documentation)
and for ERA5
[here](https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation).
