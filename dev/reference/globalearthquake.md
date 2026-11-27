# Significant Earthquake Dataset

A data set containing the earthquakes of magnitude at least 5 that
occurred during the period 1900-2022, with their GPS coordinates and
magnitudes.

## Usage

``` r
globalearthquake
```

## Format

A data frame with 98785 rows and 12 variables:

- date_time:

  date and time in POSIXct format

- latitude:

  latitude of the earthquake, from -90° to 90°

- longitude:

  longitude of the earthquake, from -180° to 180°

- mag:

  the magnitude of the earthquake, indicating its strength

- Date:

  date when the earthquake occurred

- nbweeks:

  number of complete weeks elapsed since 1900/01/01

- year:

  year

- month:

  month of the year

- month_day:

  day of the month

- week:

  week of the year, from 1 to 53 (week 1 consists of the first seven
  days of the year, week 2 of the next seven days, etc.)

- week_day:

  day of the week from 1 = Sunday to 7 = Saturday

- year_day:

  day of the year from 1 to 366

## Source

Earthquake Catalog, U.S. Geological Survey, Department of the Interior.
<https://www.usgs.gov/programs/earthquake-hazards>

## Details

This is a compiled version of the full data set available on [U.S.
Geological Survey Earthquake
Events](https://www.usgs.gov/programs/earthquake-hazards) (USGS) which
is in the [public
domain](https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits).

The data set contains only the earthquakes between 1900 and 2022 with a
magnitude of at least 5, sorted by date and time.

## Examples

``` r
summary(globalearthquake$mag)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   5.000   5.100   5.300   5.456   5.700   9.500 
## earthquakes in a rectangle around Japan since 1980
japan <- subset(
  globalearthquake,
  latitude > 30 & latitude < 46 & longitude > 129 & longitude < 146 &
    year >= 1980
)
## binary time series: was there at least one earthquake during the week?
weeks <- seq(min(japan$nbweeks), max(globalearthquake$nbweeks))
japan_weeks <- weeks %in% japan$nbweeks
table(japan_weeks)
#> japan_weeks
#> FALSE  TRUE 
#>   827  1417 
japan_model <- tune_vlmc(japan_weeks)
draw(as_vlmc(japan_model))
#> ▪ (0.3685, 0.6315)
#> └─ TRUE (0.3503, 0.6497)
#>    ├─ FALSE (0.4153, 0.5847)
#>    └─ TRUE (0.3156, 0.6844)
#>       └─ FALSE (0.4034, 0.5966)
```
