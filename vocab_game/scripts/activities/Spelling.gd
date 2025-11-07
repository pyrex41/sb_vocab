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
	answer_input.text_submitted.connect(_on_input_submitted)

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
	instruction_label.text = "Listen to the word and spell it correctly 🎧"
	answer_input.text = ""
	answer_input.placeholder_text = "Type the word here..."

	# DEBUG: Print the word to console for testing (only in debug builds)
	if OS.is_debug_build() and word_data.has("word"):
		print("DEBUG: Spelling word is '%s'" % word_data.word)

	# Auto-play the audio once
	_play_audio_hint()

	# Focus the input field for better UX
	answer_input.grab_focus()

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
	_submit_answer()

func _on_input_submitted(_text: String):
	# Handle Enter key press
	_submit_answer()

func _submit_answer():
	var answer = answer_input.text.strip_edges()
	if answer.length() > 0:
		answer_submitted.emit(answer)
	else:
		# Visual feedback for empty submission
		_show_validation_error()

func _show_validation_error():
	# Briefly flash the input field to indicate validation error
	var original_color = answer_input.modulate
	answer_input.modulate = Color(1.0, 0.7, 0.7)  # Light red tint
	await get_tree().create_timer(0.3).timeout
	answer_input.modulate = original_color

	# Shake animation
	var original_pos = answer_input.position
	for i in range(3):
		answer_input.position.x = original_pos.x - 5
		await get_tree().create_timer(0.05).timeout
		answer_input.position.x = original_pos.x + 5
		await get_tree().create_timer(0.05).timeout
	answer_input.position = original_pos

func _on_loading_started():
	answer_input.editable = false
	submit_button.disabled = true
	play_audio_button.disabled = true

func _on_loading_ended():
	answer_input.editable = true
	submit_button.disabled = false
	play_audio_button.disabled = false
