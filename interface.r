# Set subject raw data directory
raw_dir <- "./raw/"

# ==================================================
# Define AOI sets
# ==================================================

aoiset_actionphasebody <- list(
  column_name = "AOIActionPhaseBody",
  no_evaluation_label = "NO EVAL",
  missing_coordinate_label = "noXY",
  outside_aoi_label = "outside",
  aoilist = list(
    aoi1 = list(
      hit_name = "left",
      x_topleft = 60,
      y_topleft = 290,
      x_bottomright = 690,
      y_bottomright = 1130
    ),
    aoi2 = list(
      hit_name = "right",
      x_topleft = 1225,
      y_topleft = 290,
      x_bottomright = 1855,
      y_bottomright = 1130
    ),
    aoi3 = list(
      hit_name = "top",
      x_topleft = 790,
      y_topleft = 10,
      x_bottomright = 1130,
      y_bottomright = 350
    ),
    aoi4 = list(
      hit_name = "bottom",
      x_topleft = 790,
      y_topleft = 730,
      x_bottomright = 1130,
      y_bottomright = 1070
    )
  )
)

aoiset_actionphaseface <- list(
  column_name = "AOIActionPhaseFace",
  no_evaluation_label = "NO EVAL",
  missing_coordinate_label = "noXY",
  outside_aoi_label = "outside",
  aoilist = list(
    aoi1 = list(
      hit_name = "left",
      x_topleft = 170,
      y_topleft = 290,
      x_bottomright = 650,
      y_bottomright = 770
    ),
    aoi2 = list(
      hit_name = "right",
      x_topleft = 1310,
      y_topleft = 290,
      x_bottomright = 1790,
      y_bottomright = 770
    ),
    aoi3 = list(
      hit_name = "top",
      x_topleft = 790,
      y_topleft = 10,
      x_bottomright = 1130,
      y_bottomright = 350
    ),
    aoi4 = list(
      hit_name = "bottom",
      x_topleft = 790,
      y_topleft = 730,
      x_bottomright = 1130,
      y_bottomright = 1070
    )
  )
)

aoiset_outcomephase <- list(
  column_name = "AOIOutcomePhase",
  no_evaluation_label = "NO EVAL",
  missing_coordinate_label = "noXY",
  outside_aoi_label = "outside",
  aoilist = list(
    aoi1 = list(
      hit_name = "top",
      x_topleft = 790,
      y_topleft = 10,
      x_bottomright = 1130,
      y_bottomright = 350
    ),
    aoi2 = list(
      hit_name = "bottom",
      x_topleft = 790,
      y_topleft = 730,
      x_bottomright = 1130,
      y_bottomright = 1070
    )
  )
)

aoiset_screen <- list(
  column_name = "AOIScreen",
  no_evaluation_label = "NO EVAL",
  missing_coordinate_label = "noXY",
  outside_aoi_label = "outside",
  aoilist = list(
    aoi1 = list(
      hit_name = TRUE,
      x_topleft = -40,
      y_topleft = -40,
      x_bottomright = 1960,
      y_bottomright = 1120
    )
  )
)


# ==================================================
# Define inter trial naming patterns (i.e., phases)
# ==================================================

inter_trial_chunk_patterns = c(
  ".*a_THIRD-PRETEST.*",
  ".*b_THIRD-PRETEST.*",
  ".*a_THIRD-TEST.*",
  ".*b_THIRD-TEST.*"
)

# ========================================================
# Define Naming Schema (Keys, Look-Up Information/Mappings
# ========================================================

# FILENAME OF THE RECORDING / RAW DATA
#                "MEMOThird_01_M_293_Rec01_Exp3.tsv"
keys_filename <- c("study_name", "id", "sex", "age_days", "rec", "experiment")


# FILENAME OF THE VIDEOS
# 2_b_THIRD-PRETEST_D1_NCOM_NO_OBEN_OBJ-18
# 10_a_THIRD-TEST_D1_COM_LOCATION_OBEN_OBJ-5.wmv
keys_videoname <- c(
  "running_trial",
  "phase",
  "test_phase",
  "dyad",
  "con_soc",
  "con_object_change",
  "object_position",
  "object_id"
)


# ==================================================
# Mandatory Columns (Do not remove columns here,
# unless you know what you are doing)
# ==================================================

mc <-
  c(
    "RecordingTimestamp",
    "StudioEvent",
    "StudioEventData",
    "FixationIndex",
    "GazeEventDuration",
    "GazePointXADCSpx",
    "GazePointYADCSpx"
  )

# ==================================================
# Columns of Interests (Add as many as you want)
# ==================================================

coi <-
  c(
    "GazeEventType",
    "LocalTimeStamp",
    "StudioEventIndex"
  )

