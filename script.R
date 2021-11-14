rm(list = ls(all.names = TRUE)) # Clear workspace
graphics.off() # close all open graphics

# import user interface
source("interface.R")

# install packages
lapply(interface$dependency_list, require, character.only = TRUE)


# import utility functions
list.files("util", "*.R$", full.names = TRUE, ignore.case = TRUE) %>% sapply(source)


# read raw data filenames
participants <- list.files(interface$raw_dir)

# incomplete subjects (i.e., not having 2 pretest & 12 test trials)
incomplete_subjets <- c()

# Loop over all participants
for (subject in participants) {

  # remove later
  subject <- participants[1]

  # read tsv files
  df_raw <- read_tsv(file.path(interface$raw_dir, subject))

  # run preflight checks & diagnostics, returns a lean df
  df <- preflight(df_raw, interface)

  # get start and end index pairs for inter_trial chunks
  startend_pretest_action <- get_start_end_pos(df, interface$inter_trial_chunk_patterns[1])
  startend_pretest_outcome <- get_start_end_pos(df, interface$inter_trial_chunk_patterns[2])
  startend_test_action <- get_start_end_pos(df, interface$inter_trial_chunk_patterns[3])
  startend_test_outcome <- get_start_end_pos(df, interface$inter_trial_chunk_patterns[4])

  # test if subject has consistent start/end indexes and get check trial count to match 2 & 12
  if (
    get_trial_count(c(startend_pretest_action, startend_pretest_outcome)) != 2 &&
      get_trial_count(c(startend_test_action, startend_test_outcome)) != 12
  ) {
    incomplete_subjets <- c(incomplete_subjets, subject)
    stop("Bad Trial count")
  }

  # track current trials
  current_pretest_trials <- get_trial_count(c(startend_pretest_action, startend_pretest_outcome))
  current_test_trials <- get_trial_count(c(startend_test_action, startend_test_outcome))


  # Allocate Trials and Fillup StudioEventData Label
  df <- allocate_trials(df, c(startend_pretest_action, startend_pretest_outcome), 2)
  df <- allocate_trials(df, c(startend_test_action, startend_test_outcome), 2, reset_to_1 = TRUE)

  # track video names
  names_test_action <- df$StudioEventData[startend_test_action$start] %>%
    unique() %>%
    as.character()

  names_test_outcome <- df$StudioEventData[startend_test_outcome$start] %>%
    unique() %>%
    as.character()

  # Insert AOI Columns
  df <- add_column(df, "{interface$aoisets$actionphasebody$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$actionphasebody, startend_test_action), .before = 1)

  df <- add_column(df, "{interface$aoisets$actionphaseface$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$actionphaseface, startend_test_action), .after = 1)

  df <- add_column(df, "{interface$aoisets$outcomephase$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$outcomephase, startend_test_outcome), .after = 2)

  df <- add_column(df, "{interface$aoisets$screen$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$screen), .after = 3)

  ##################################################################################################
  # Initialize empty subject tibble (the better data.frame)
  df_subject <- tibble(.rows = current_test_trials)

  # Build Summary table
  # ================================================================================================
  # NAME INFORMATIONS
  # ------------------------------------------------------------------------------------------------
  df_subject$ID <- value_parser_by_key(interface$keys_filename, subject)$id
  df_subject$Trial <- 1:current_test_trials
  df_subject$TrialCon <- c(rep(1, 6), rep(2, 6))
  df_subject$Sex <- value_parser_by_key(interface$keys_filename, subject)$sex
  df_subject$Age_Days <- value_parser_by_key(interface$keys_filename, subject)$age_days
  df_subject$Rec <- value_parser_by_key(interface$keys_filename, subject)$rec
  df_subject$Exp <- value_parser_by_key(interface$keys_filename, subject, trim_right = 4)$experiment


  # ------------------------------------------------------------------------------------------------
  # Looking Times
  # ------------------------------------------------------------------------------------------------
  df_subject$TotalLTScreenOut <-
    get_looks(df, interface$aoisets$screen, startend_test_outcome, c(120, "end"))$looking_times









}

