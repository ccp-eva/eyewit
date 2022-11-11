# make sure that all keynames are the same for every vendor
# keynames need to match with the names in preflight
# set keys values that don't match a any column to NULL

# SYNTAX:
# newColumnName = list("Original Column Name" = readr::col_xyz())

vendor_lookup <- list(
	tobii_studio = list(
		participant = list("ParticipantName" = readr::col_character()),						# id1234
		timestamp = list("RecordingTimestamp" = readr::col_integer()),						# 11, 28, 45 (equidistant values, sound never contain NA)
		event = list("StudioEvent" = readr::col_character()),											# MovieStart, MovieEnd, NA, ... (Markers)
		eventValue = list("StudioEventData" = readr::col_character()),						# 2_b_d2.mp4 (stimuli being displayed)
		gazeType = list("GazeEventType" = readr::col_character()),								# Unclassified, Fixation, Saccade, ...
		fi = list("FixationIndex" = readr::col_integer()),												# 1, 2, 3, NA, 4, 5 ...
		gazeDuration = list("GazeEventDuration" = readr::col_integer()),					# 167, 50, 83, NA
		x = list("GazePointX (ADCSpx)" = readr::col_integer()),										# 806, NA, ...
		fix = list(NULL = NULL),
		y = list("GazePointY (ADCSpx)" = readr::col_integer()),										# 234, NA, ...
		fiy = list(NULL = NULL),
		pupilDiameterLeft = list(NULL = NULL),
		pupilDiameterRight = list(NULL = NULL)
	),
	tobii_prolab = list(
		participant = list("Participant name" = readr::col_character()),					# id1234
		timestamp = list("Recording timestamp" = readr::col_integer()),						# 79057, 79057, ...
		event = list("Event" = readr::col_character()),														# VideoStimulusStart, VideoStimulusEnd, NA, Keyboard, MouseEvent,
		eventValue = list("Event value" = readr::col_character()),								# ImageStimulusStart -> ball_center, Keyboard -> Space, MouseEvent -> Up, Left
		gazeType = list("Eye movement type" = readr::col_character()),						# Fixation, Saccade, Unclassified, NA, EyesNotFound
		gazeDuration = list("Gaze event duration" = readr::col_integer()),				# 168, 842, ...
		x = list("Gaze point X" = readr::col_integer()),														# 800, 742, ... Raw gaze coordinates
		fix = list("Fixation point X" = readr::col_integer()),										# 803, 740, ...Horizontal coordinate of the avg gaze point for both eyes
		y = list("Gaze point Y" = readr::col_integer()),													#
		fiy = list("Fixation point Y" = readr::col_integer()),										#
		pupilDiameterLeft = list("Pupil diameter left" = readr::col_number()),		# 3552
		pupilDiameterRight = list("Pupil diameter right" = readr::col_number())	# 3527
	)
)
