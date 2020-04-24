# Set subject raw data directory
recs_dir <- "./recs/"

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
      x_topleft = 79,
      y_topleft = 159,
      x_bottomright = 759,
      y_bottomright = 1089
    ),
    aoi2 = list(
      hit_name = "center",
      x_topleft = 844,
      y_topleft = 794,
      x_bottomright = 1204,
      y_bottomright = 1154
    ),
    aoi3 = list(
      hit_name = "right",
      x_topleft = 1305,
      y_topleft = 159,
      x_bottomright = 1985,
      y_bottomright = 1089
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
      x_topleft = 177,
      y_topleft = 177,
      x_bottomright = 727,
      y_bottomright = 627
    ),
    aoi2 = list(
      hit_name = "right",
      x_topleft = 1330,
      y_topleft = 177,
      x_bottomright = 1880,
      y_bottomright = 627
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
      x_topleft = 350,
      y_topleft = 396,
      x_bottomright = 710,
      y_bottomright = 756
    ),
    aoi2 = list(
      hit_name = "right",
      x_topleft = 1338,
      y_topleft = 396,
      x_bottomright = 1698,
      y_bottomright = 756
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
      x_bottomright = 2048,
      y_bottomright = 1152
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

#         "OPROThird_01_W_9_Alva_Rec15_24.txt"
# e.g.:       |1|    |2|3|4||5|   |6|   |7|
lut_filename <- c("exp_name", "ID", "Sex", "Age_Days", "name", "session", "tail")


# "24_Inter_A_Con2_NSOC_LOB_D2a_ObjectX_a_Obj_12_a.wmv"
lut_fam_phase <- c("Trial", "Folder", "Letter", "Condition", "Con_SocInt", "Con_Object", "Dyad", "B", "C", "D", "E", "F")


# ==================================================
# Columns of Interests
# ==================================================

coi <-
  c(
    "RecordingTimestamp",
    "LocalTimeStamp",
    "StudioEventIndex",
    "StudioEvent",
    "StudioEventData",
    "FixationIndex",
    "GazeEventType",
    "GazeEventDuration",
    "GazePointX..ADCSpx.",
    "GazePointY..ADCSpx."
  )


