
``` r
knitr::opts_chunk$set(message=FALSE)
```

``` r
library(neonstore)
library(tidyverse)
library(tsibble)
library(fable)
library(ISOweek)
library(neon4cast) # remotes::install_github("eco4cast/neon4cast", dep=TRUE)

source("R/flat_format.R")
source("R/iso_week.R")
```

# A trivial ARIMA forecast

``` r
team <- "cb_f2"
## uuid::UUIDgenerate()
iteration_id <- "63bb1970-3664-48f6-829d-0e9f766bf7a3"
```

## Access Target Data

``` r
targets <-
  "https://data.ecoforecast.org/targets/beetles/beetles-targets.csv.gz" %>% 
  read_csv(col_types = "cDdd") %>% 
  as_tsibble(index = time, key = siteID)
```

``` r
past <-  targets
forecast_date <- Sys.Date() 

forecast_date <- as.Date("2021-07-01")
past <-  targets %>% filter(time < forecast_date)
```

## Compute a forecast

``` r
gap_filled <- tsibble::fill_gaps(past)
arima_richness <- gap_filled  %>% 
  model(arima = ARIMA(richness)) %>%
  forecast(h = "1 year") %>%
  efi_format()

arima_abundance <- gap_filled  %>%
  model(arima = ARIMA(abundance)) %>%
  forecast(h = "1 year") %>%
  efi_format()

## Combine richness and abundance forecasts. drop the 'model' column
arima_forecast <- inner_join(arima_richness, arima_abundance) %>% select(!.model)
```

## EFI Formatting

``` r
## Combine richness and abundance forecasts. drop the 'model' column
forecast <- inner_join(arima_richness, arima_abundance) %>% select(!.model)
```

Dates must be expressed as the Monday of the week the sampling would be
performed.  
This way don’t need to predict the precise day that the trap is
collected, all traps collected in the same week will count as an
observation made for that week.

``` r
forecast <- forecast %>% mutate(time = iso_week(time) )
```

``` r
forecast_file <- glue::glue("{theme}-{date}-{team}.csv.gz",
                            theme = "beetles", 
                            date=forecast_date,
                            team = team)
write_csv(forecast, forecast_file)
```

``` r
neon4cast::forecast_output_validator(forecast_file)
```

    ## [1] TRUE

``` r
yaml_meta <- neon4cast::create_model_metadata(forecast_file)
eml_file <- neon4cast::write_metadata_eml(forecast_file, 
                                          yaml_meta, 
                                          forecast_issue_time = forecast_date, 
                                          forecast_iteration_id = iteration_id)
```

    ## Warning in readChar(path, nchar): truncating string with embedded nuls

``` r
submit(forecast_file, metadata = eml_file)
```

    ## [1] TRUE
