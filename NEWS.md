# ecohabr2 2.0.0

## 2.0.0 (2026-01-12)
* Initial CRAN submission.
* New data structure design and algorithms from ecohabr.Functions are similar but usages are incompatible with ecohabr.
* Use data.table to store and process data from Eco-HAB experiments.
* Use R6 class to organize data and functions.

Known issues: 
* Timezone ambiguity at winter DST has not been addressed.
* Timezone has not been adjusted if the data are analyzed at a different timezone from acquisition. Currently all time stamps are treated as local time.