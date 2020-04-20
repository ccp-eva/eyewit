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
      x_topright = 79,
      y_topright = 159,
      x_bottomright = 759,
      y_bottomright = 1089
    ),
    aoi2 = list(
      hit_name = "center",
      x_topright = 844,
      y_topright = 794,
      x_bottomright = 1204,
      y_bottomright = 1154
    ),
    aoi3 = list(
      hit_name = "right",
      x_topright = 1305,
      y_topright = 159,
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
      x_topright = 177,
      y_topright = 177,
      x_bottomright = 727,
      y_bottomright = 627
    ),
    aoi2 = list(
      hit_name = "right",
      x_topright = 1330,
      y_topright = 177,
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
      x_topright = 350,
      y_topright = 396,
      x_bottomright = 710,
      y_bottomright = 756
    ),
    aoi2 = list(
      hit_name = "right",
      x_topright = 1338,
      y_topright = 396,
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
      x_topright = 0,
      y_topright = 0,
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
