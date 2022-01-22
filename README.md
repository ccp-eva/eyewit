<img align="right" width="170" src="assets/logo.svg">

# eyewit
### Tobii Eyetracking Utility Functions

> An R package for pre-processing raw eye-tracking data around *Tobii's I-VT Fixation Filter*

----

## Installation

`devtools::install_github(ccp-eva/eyewit)`

## What’s Happening

### eyewit Roadmap
#### Release 0.5
Progress is tracked here: https://github.com/ccp-eva/eyewit/projects/4
Now that eyewit is an actual R package, I need to address the following points:
- [ ] Allmost all functions need proper documentation
- [ ] Many functions need unit tests
- [ ] Refactor functions (espacially `get_looks`)

#### Release 0.6
Progress is tracked here: https://github.com/ccp-eva/eyewit/projects/8

### January 2022
- [x] Successfully converted to an R package (`devtools::install_github(ccp-eva/eyewit)`)


## Functions

As of January 2022, functions are being refactored and documented in their corresponding helper files. This table will is out of date and will be removed!

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
