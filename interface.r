# Set subject raw data directory
raw_dir <- "./raw/"

# ==================================================
# Define AOI collections
# ==================================================

aoi_fam_body_object <- list(
  column_name = "AOIFamBodyObj",
  no_evaluation_label = "NO EVAL",
  missing_coordinate_label = NA,
  aoilist = list(
    aoi1 = list(
      hit_name = "left",
      x_topleft = 0,
      y_topleft = 133,
      x_bottomright = 680,
      y_bottomright = 1033
    ),
    aoi2 = list(
      hit_name = "center",
      x_topleft = 790,
      y_topleft = 751,
      x_bottomright = 1130,
      y_bottomright = 1091
    ),
    aoi3 = list(
      hit_name = "right",
      x_topleft = 1240,
      y_topleft = 133,
      x_bottomright = 1920,
      y_bottomright = 1043
    )
  )
)

aoi_fam_face <- list(
  column_name = "AOIFamFace",
  no_evaluation_label = "NO EVAL",
  missing_coordinate_label = NA,
  aoilist = list(
    aoi1 = list(
      hit_name = "left",
      x_topleft = 66,
      y_topleft = 133,
      x_bottomright = 636,
      y_bottomright = 563
    ),
    aoi2 = list(
      hit_name = "right",
      x_topleft = 1284,
      y_topleft = 133,
      x_bottomright = 1854,
      y_bottomright = 563
    )
  )
)

aoi_fam_face_object <- list(
  column_name = "AOIFamFaceObject",
  no_evaluation_label = "NO EVAL",
  missing_coordinate_label = NA,
  aoilist = list(
    aoi1 = list(
      hit_name = "left",
      x_topleft = 66,
      y_topleft = 133,
      x_bottomright = 636,
      y_bottomright = 563
    ),
    aoi2 = list(
      hit_name = "center",
      x_topleft = 790,
      y_topleft = 751,
      x_bottomright = 1130,
      y_bottomright = 1091
    ),
    aoi3 = list(
      hit_name = "right",
      x_topleft = 1284,
      y_topleft = 133,
      x_bottomright = 1854,
      y_bottomright = 563
    )
  )
)

aoi_preflook <- list(
  column_name = "AOIPrefLook",
  no_evaluation_label = "NO EVAL",
  missing_coordinate_label = NA,
  aoilist = list(
    aoi1 = list(
      hit_name = "left",
      x_topleft = 296,
      y_topleft = 370,
      x_bottomright = 636,
      y_bottomright = 710
    ),
    aoi2 = list(
      hit_name = "right",
      x_topleft = 1284,
      y_topleft = 370,
      x_bottomright = 1624,
      y_bottomright = 710
    )
  )
)

aoi_screen <- list(
  column_name = "AOIScreen",
  no_evaluation_label = "NO EVAL",
  missing_coordinate_label = NA,
  aoilist = list(
    aoi1 = list(
      hit_name = TRUE,
      x_topleft = 0,
      y_topleft = 0,
      x_bottomright = 1920,
      y_bottomright = 1080
    )
  )
)


# ==================================================
# Define inter trial naming patterns (i.e., phases)
# ==================================================

inter_trial_chunk_patterns = c(
  ".*Familiar.*",  # Something with *Familiar*
  ".*_Inter.*",    # Something with *_Inter*, compare: 24_Inter_A-Con2_NSOC_LOB_D2a-ObjectX_a-Obj_12_a.wmv
  ".*Preflook.*",  # Something with *Preflook*
  ".*LEFT.*"       # Something with *LEFT*, compare: 9a_ObjectY_b-Obj_16_b-LEFT-ObjectY_a-Obj_16_a-RIGHT.wmv
)

# ==================================================
# Define Name Look-Up Information / Mappings
# ==================================================

#         "OPROThird_10_M_11_Rec16_24.txt"
# e.g.:       |1|    |2|3|4||5|   |6|   |7|
lut_filename <- c("exp_name", "ID", "Sex", "Age_Days", "Rec", "Experiment")


# "24_Inter_A_Con2_NSOC_LOB_D2a_ObjectX_a_Obj_12_a.wmv"
lut_fam_phase <- c("Trial", "Folder", "Letter", "Condition", "Con_SocInt", "Con_Object", "Dyad", "B", "C", "D", "E", "F")


# ==================================================
# Mandatory Columns (Do not remove columns here,
# unless you know what you are doing)
# ==================================================

mc <-
  c(
    "RecordingTimestamp",
    "LocalTimeStamp",
    "StudioEventIndex",
    "StudioEvent",
    "StudioEventData",
    "FixationIndex",
    "GazeEventDuration",
    "GazePointX..ADCSpx.",
    "GazePointY..ADCSpx."
  )

# ==================================================
# Columns of Interests (Add as many as you want)
# ==================================================

coi <-
  c("GazeEventType")

