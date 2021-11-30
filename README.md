<img align="right" width="150" src="assets/logo.svg">

# Tobii Eye-Tracking Utilities / `EyeWit`

> Helper functions for common eye-tracking data wrangling tasks and eye-tracking analysis around _Tobii’s I-VT Fixation Filter_

## Development

### Road to Package `eyewit`

As of now this repo is a collection of R functions. Release 1.0.0 aims to put all these into a libarary (`eyewit::`).

### Road to Unit Tests

Modification in delicate functions (e.g., calculating looking time) may result in wrong calculations. Thus, an automatic unit test must be performed to grant correct calculations. This will be the next milestone after 1.0.0.

### Road to Shiny

When this package reached a mature state, I will transfer functionality in a web UI.

## Functions

Listed in rough execution order

| **Objects**               | **Description**                                                                                                                                                                                                                                                |
| ------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| interface                 | Contains user defined parameters for the experiment: raw files directory, x, y column names, AOI information, etc.                                                                                                                                             |
| preflight()               | Takes a `df` and the `interface` as input and performs several checks (check for required columns, fix column names, check that rownames are in an incremental order (i.e., no missing rows or prior pre-processing), check if single AOIs are not overlapping |
| get_startend_pos()        | Returns a list of start and end positions for a given `df` and a `regex` pattern. Those position pairs define usually a trial                                                                                                                                  |
| get_trial_count()         | Based on a single or multiple provided trialscopes, the function calculates the trial count                                                                                                                                                                    |
| allocate_trials()         | Creates an enumerated "Trial" column in the provided df for a single or multiple trialscopes                                                                                                                                                                   |
| get_aois()                | To be documented                                                                                                                                                                                                                                               |
| get_fixationindex_pairs() | To be documented                                                                                                                                                                                                                                               |
| get_gazeshift_latency()   | To be documented                                                                                                                                                                                                                                               |
| value_parser_by_key()     | To be documented                                                                                                                                                                                                                                               |
| get_looks                 | To be documented & To be refactored                                                                                                                                                                                                                            |
| get_first_free_fi()       | Utility function for get_looks() (specifically for `omit_first_overflow_fi` parameter)                                                                                                                                                                         |
| get_lookaway_scope_end()  | Utility function for get_looks() (specifically for `lookaway_stop` parameter)                                                                                                                                                                                  |
| get_non_fixation_data()   | To be documented                                                                                                                                                                                                                                               |
| get_objects()             | To be documented                                                                                                                                                                                                                                               |
| get_preflook_pos()        | To be documented, (legacy, tailor-made function)                                                                                                                                                                                                               |
| is_aoilist_overlapping()  | Part of the preflight() to check overlapping aois                                                                                                                                                                                                              |
| is_hitname_in_range()     | Returns TRUE/FALSE for a given vec, target hitname, fixation start index, and fixation stop index                                                                                                                                                              |
| merge_startend_chunks()   | Utility function for get_trial_count() and allocate_trials()                                                                                                                                                                                                   |

## Contribution

Feel free to open PRs or discuss enhancements and code reviews
