# ecohabr2 2.2.0

## 2.2.0 (2026-02-26)
* Store time as one `POSIXct` (time) instead of one `POSIXct` (time) and one `integer` (delay) in all `data.table` class members.
* Use the base R pipeline operator `|>` and require R version 4.1.0 and later ones.
* Optimize display in shinyApp for `edit_events()`.

Known issues: 
* Currently `calc_follow()` only checks if the follower enters the tube later than the followee. Whether the follower exits the tube later than the followee needs to be addressed.
* In `calc_events()`, the initial location in idlist file has not been used to populate the first event (the one before the first RFID reading).

## 2.1.1 (2026-02-13)
* Timezone ambiguity is addressed by guessing the transition point from recordings. Not that it won't be accurate if too few data points are recorded.
* Add a "local" seed in the `dummy()` function to generate reproducible results.
* Add a `reset_results()` function to delete current results in an EcoHAB object.

Known issues: 
* Currently `calc_follow()` only checks if the follower enters the tube later than the followee. Whether the follower exits the tube later than the followee needs to be addressed.
* In `calc_events()`, the initial location in idlist file has not been used to populate the first event (the one before the first RFID reading).

## 2.1.0 (2026-02-05)
* Add a `dummy()` function to generate dummy data for test.
* Add a vignette to show how to use the package.
* Add an extremely simplified, dummy data set for test.
* Read all input files using `data.table::fread()` in character mode.

Known issues: 
* Timezone ambiguity at winter DST has not been addressed.
* Currently `calc_follow()` only checks if the follower enters the tube later than the followee. Whether the follower exits the tube later than the followee needs to be addressed.
* In `calc_events()`, the initial location in idlist file has not been used to populate the first event (the one before the first RFID reading).

## 2.0.1 (2026-01-17)
* Use `lubridate::ymd_hms()` instead of `lubridate::fast_strptime()` to read timestamp. The former is more efficient.
* Add a timezone parameter in the `initialize()` function so that users can match the place where the Eco-HAB data are acquired.
* Store the start and end time in timeline_bin as `POSIXct`.
* Add a pop-up window to show concurrent events in the events editor shinyApp.
* Throw warning instead of error messages when parameters are missing.
* Return self in all setters to allow method chaining.

Known issues: 
* Timezone ambiguity at winter DST has not been addressed.
* Currently `calc_follow()` only checks if the follower enters the tube later than the followee. Whether the follower exits the tube later than the followee needs to be addressed.

## 2.0.0 (2026-01-12)
* Initial github submission.
* New data structure design and algorithms from [ecohabr](https://github.com/horizon-mm/ecohabr).Functions are similar but usages are incompatible with ecohabr.
* Use data.table to store and process data from Eco-HAB experiments.
* Use R6 class to organize data and functions.

Known issues: 
* Timezone ambiguity at winter DST has not been addressed.
* Timezone has not been adjusted if the data are analyzed at a different timezone from acquisition. Currently all time stamps are treated as local time.
