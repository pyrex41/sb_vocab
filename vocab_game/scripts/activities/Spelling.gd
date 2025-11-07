extends Control

signal answer_submitted(answer)

@onready var instruction_label = $CenterContainer/VBoxContainer/InstructionLabel
@onready var play_audio_button = $CenterContainer/VBoxContainer/PlayAudioButton
@onready var answer_input = $CenterContainer/VBoxContainer/AnswerInput
@onready var submit_button = $CenterContainer/VBoxContainer/SubmitButton

var word_data: Dictionary

func _ready():
	play_audio_button.pressed.connect(_on_play_audio)
	submit_button.pressed.connect(_on_submit)

	# Connect to loading signals to disable inputs during API calls
	SessionManager.loading_started.connect(_on_loading_started)
	SessionManager.loading_ended.connect(_on_loading_ended)

func setup(activity_data: Dictionary):
	# Validate activity_data has required word field
	if not activity_data.has("word") or typeof(activity_data.word) != TYPE_DICTIONARY:
		push_error("Invalid activity data: missing or invalid 'word' field")
		SessionManager.api_error.emit("Invalid activity data received. Please try again.")
		return

	word_data = activity_data.word
	instruction_label.text = "Listen to the word and spell it correctly"
	answer_input.placeholder_text = "Type the word here..."

	# DEBUG: Print the word to console for testing (only in debug builds)
	if OS.is_debug_build() and word_data.has("definition"):
		var def = word_data.definition
		# Validate definition format before extracting word
		if typeof(def) == TYPE_STRING and def.length() > 0:
			# Extract word from "word (definition)" format
			var parts = def.split(" ")
			if parts.size() > 0:
				var word_match = parts[0]
				print("DEBUG: Current word is '%s'" % word_match)
			else:
				push_warning("Could not extract word from definition format")
		else:
			push_warning("Definition field is empty or invalid")

	# Auto-play the audio once
	_play_audio_hint()

func _on_play_audio():
	_play_audio_hint()

func _play_audio_hint():
	# In a real implementation, this would play the audio file
	# For now, just show a hint message
	instruction_label.text = "🔊 Playing audio... Listen carefully!"
	if is_inside_tree():
		await get_tree().create_timer(1.0).timeout
		instruction_label.text = "Type what you heard:"
	else:
		push_warning("Spelling activity not in scene tree, skipping audio hint timer")
		instruction_label.text = "Type what you heard:"

func _on_submit():
	var answer = answer_input.text.strip_edges()
	if answer.length() > 0:
		answer_submitted.emit(answer)

func _on_loading_started():
	answer_input.editable = false
	submit_button.disabled = true

func _on_loading_ended():
	answer_input.editable = true
	submit_button.disabled = false
