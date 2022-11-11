interface <- list(

  # Set subject raw data directory
  raw_dir = "./raw/9 months/",
  output_dir = "./preproc-tsv/",

  # ==================================================
  # Define inter trial naming patterns
  # (for defining phases, trials, videos, etc)
  # ==================================================
  inter_trial_chunk_patterns = c(
    ".*_f_.*",
    ".*_p_.*"
  ),

  # ========================================================
  # Define Naming Schema (Keys, Look-Up Information/Mappings
  # ========================================================

  # FILENAME OF THE RECORDING / RAW DATA
  #                "REJOINT_01_M_293_Rec01_Exp3.tsv"
  keys_filename = c("study_name", "id", "sex", "age_days", "rec", "experiment"),

  # FILENAME OF THE VIDEOS
  # 1_f_4_eye_nprox_B_right_1a.mp4
  keys_fam = c(
    "running_trial", # 1 - 16
    "phase", # a = familiarization; b = preflook
    "condition", # 1 - 4
    "con_eye", # eye or neye
    "con_proximity", # prox or nprox
    "obj_handling_actor", # A or B
    "position_obj", # equals to the postion of the object-handling actor
    "obj_id"
  ),

  # 1_p_4_eye_nprox_1a_1b.mp4
  keys_preflook = c(
  	"running_trial", # 1 - 16
  	"phase", # a = familiarization; b = preflook
  	"condition", # 1 - 4
  	"con_eye", # eye or neye
  	"con_proximity", # prox or nprox
  	"obj_id_left",
  	"obj_id_right"
  ),


  # ==================================================
  # Mandatory Columns (Do not remove columns here,
  # unless you know what you are doing)
  # ==================================================

  mc =
    c(
      "timestamp",
      "event",
      "eventValue",
      "gazeType",
      "fi",
      "gazeDuration",
      "x",
      "y"
    ),

  # ==================================================
  # Columns of Interests (Add as many as you want)
  # ==================================================

  coi =
    c(
    	# "participant"
    ),

  # ==================================================
  # Only important for Tobii Pro Lab or raw data missing FixationIndex column
  # ==================================================

	type_col = "Eye movement type",
	type_index_col = "Eye movement type index",


  # ==================================================
  # Define AOI sets
  # ==================================================

  aoisets = list(
    aoifamphase_obj_r_prox = list(
      column_name = "aoifamphase_obj_r_prox",
      no_evaluation_label = "NO EVAL",
      missing_coordinate_label = "noXY",
      outside_aoi_label = "outside",
      aoilist = list(
        aoi1 = list(
          hit_name = "face_left",
          x_topleft = 922,
          y_topleft = 65,
          x_bottomright = 1278,
          y_bottomright = 485
        ),
        aoi2 = list(
          hit_name = "face_right",
          x_topleft = 1362,
          y_topleft = 65,
          x_bottomright = 1718,
          y_bottomright = 485
        ),
        aoi3 = list(
          hit_name = "object_right",
          x_topleft = 1415,
          y_topleft = 745,
          x_bottomright = 1655,
          y_bottomright = 985
        )
      )
    ),

    aoifamphase_obj_r_nprox = list(
    	column_name = "aoifamphase_obj_r_nprox",
    	no_evaluation_label = "NO EVAL",
    	missing_coordinate_label = "noXY",
    	outside_aoi_label = "outside",
    	aoilist = list(
    		aoi1 = list(
    			hit_name = "face_left",
    			x_topleft = 242,
    			y_topleft = 65,
    			x_bottomright = 598,
    			y_bottomright = 485
    		),
    		aoi2 = list(
    			hit_name = "face_right",
    			x_topleft = 1362,
    			y_topleft = 65,
    			x_bottomright = 1718,
    			y_bottomright = 485
    		),
    		aoi3 = list(
    			hit_name = "object_right",
    			x_topleft = 1415,
    			y_topleft = 745,
    			x_bottomright = 1655,
    			y_bottomright = 985
    		)
    	)
    ),

    aoifamphase_obj_l_prox = list(
    	column_name = "aoifamphase_obj_l_prox",
    	no_evaluation_label = "NO EVAL",
    	missing_coordinate_label = "noXY",
    	outside_aoi_label = "outside",
    	aoilist = list(
    		aoi1 = list(
    			hit_name = "face_left",
    			x_topleft = 242,
    			y_topleft = 65,
    			x_bottomright = 598,
    			y_bottomright = 485
    		),
    		aoi2 = list(
    			hit_name = "face_right",
    			x_topleft = 682,
    			y_topleft = 65,
    			x_bottomright = 858,
    			y_bottomright = 485
    		),
    		aoi3 = list(
    			hit_name = "object_left",
    			x_topleft = 265,
    			y_topleft = 745,
    			x_bottomright = 505,
    			y_bottomright = 985
    		)
    	)
    ),

    aoifamphase_obj_l_nprox = list(
    	column_name = "aoifamphase_obj_l_nprox",
    	no_evaluation_label = "NO EVAL",
    	missing_coordinate_label = "noXY",
    	outside_aoi_label = "outside",
    	aoilist = list(
    		aoi1 = list(
    			hit_name = "face_left",
    			x_topleft = 242,
    			y_topleft = 65,
    			x_bottomright = 598,
    			y_bottomright = 485
    		),
    		aoi2 = list(
    			hit_name = "face_right",
    			x_topleft = 1362,
    			y_topleft = 65,
    			x_bottomright = 1718,
    			y_bottomright = 485
    		),
    		aoi3 = list(
    			hit_name = "object_left",
    			x_topleft = 265,
    			y_topleft = 745,
    			x_bottomright = 505,
    			y_bottomright = 985
    		)
    	)
    ),

    preflook = list(
      column_name = "aoiPrefLook",
      no_evaluation_label = "NO EVAL",
      missing_coordinate_label = "noXY",
      outside_aoi_label = "outside",
      aoilist = list(
        aoi1 = list(
          hit_name = "left",
          x_topleft = 225,
          y_topleft = 380,
          x_bottomright = 545,
          y_bottomright = 700
        ),
        aoi2 = list(
          hit_name = "right",
          x_topleft = 1375,
          y_topleft = 380,
          x_bottomright = 1695,
          y_bottomright = 700
        )
      )
    ),

    screen = list(
      column_name = "aoiScreen",
      no_evaluation_label = "NO EVAL",
      missing_coordinate_label = "noXY",
      outside_aoi_label = "outside",
      aoilist = list(
        aoi1 = list(
          hit_name = "onscreen",
          x_topleft = 0 - 40,
          y_topleft = 0 - 40,
          x_bottomright = 1920 + 40,
          y_bottomright = 1080 + 40
        )
      )
    )
  )
)
