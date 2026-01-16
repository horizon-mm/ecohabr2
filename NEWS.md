# ecohabr2 2.0.0

## 2.0.1 (2026-01-15
* Use lubridate::ymd_hms() instead of lubridate::fast_strptime() to read timestamp. The former is more efficient.
* Add a timezone parameter in the initialize() function so that users can match the place where the Eco-HAB data are acquired.
* Store the start and end time in timeline_bin as POSIXct.

## 2.0.0 (2026-01-12)
* Initial github submission.
* New data structure design and algorithms from [ecohabr](https://github.com/horizon-mm/ecohabr).Functions are similar but usages are incompatible with ecohabr.
* Use data.table to store and process data from Eco-HAB experiments.
* Use R6 class to organize data and functions.

Known issues: 
* Timezone ambiguity at winter DST has not been addressed.
* Timezone has not been adjusted if the data are analyzed at a different timezone from acquisition. Currently all time stamps are treated as local time.
