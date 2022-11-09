# make sure that all keynames are the same for every vendor
# keynames need to match with the names in preflight
# set keys values that don't match a any column to NULL
vendor_lookup <- list(
	tobii_studio = list(
		participant = "ParticipantName",							# id1234
		timestamp = "RecordingTimestamp",							# 11, 28, 45 (equidistant values, sound never contain NA)
		event = "StudioEvent",												# MovieStart, MovieEnd, NA, ... (Markers)
		eventValue = "StudioEventData",								# 2_b_d2.mp4 (stimuli being displayed)
		gazeType = "GazeEventType",										# Unclassified, Fixation, Saccade, ...
		fi = "FixationIndex",													# 1, 2, 3, NA, 4, 5 ...
		gazeDuration = "GazeEventDuration",						# 167, 50, 83, NA
		x = "GazePointX (ADCSpx)",										# 806, NA, ...
		fix = NULL,
		y = "GazePointY (ADCSpx)",										# 234, NA, ...
		fiy = NULL,
		pupilDiameterLeft = NULL,
		pupilDiameterRight = NULL
	),
	tobii_prolab = list(
		participant = "Participant name",							# id1234
		timestamp = "Recording timestamp",						# 79057, 79057, ...
		event = "Event",															# VideoStimulusStart, VideoStimulusEnd, NA, Keyboard, MouseEvent,
		eventValue = "Event value",										# ImageStimulusStart -> ball_center, Keyboard -> Space, MouseEvent -> Up, Left
		gazeType = "Eye movement type",								# Fixation, Saccade, Unclassified, NA, EyesNotFound
		fi = "FixationIndex",													# Got fixed with create_fi
		gazeDuration = "Gaze event duration",					# 168, 842, ...
		x = "Gaze point X",														# 800, 742, ... Raw gaze coordinates
		fix = "Fixation point X",											# 803, 740, ...Horizontal coordinate of the avg gaze point for both eyes
		y = "Gaze point Y",														#
		fiy = "Fixation point Y",											#
		pupilDiameterLeft = "Pupil diameter left",		# 3552
		pupilDiameterRight = "Pupil diameter right"		# 3527
	)
)
