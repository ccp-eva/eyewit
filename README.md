# Tobii Eye-Tracking Utilities
This repo provides some bootstrapping and generic helper functions for common eye-tracking data wrangling tasks using *Tobii’s I-VT Fixation Filter*.

## ToDo
- merge AOI functions into one
- merge get_fixation functions into one
- use tidyverse?
- write unit tests
  - https://towardsdatascience.com/unit-testing-in-r-68ab9cc8d211

## Functions
### get_AOIs
- supports multipel coordinate systems (by now: MCS and ADCS)
- set a evaluation range to speed up process

### getLooks
- get looking times
- get first look information
