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

	# Auto-play the audio once
	_play_audio_hint()

	# Focus the input field for better UX
	answer_input.grab_focus()

func _on_play_audio():
	_play_audio_hint()

func _play_audio_hint():
	# Try to play word pronunciation using available methods
	if not word_data.has("word"):
		push_warning("No word data available for audio playback")
		return

	var word = word_data.word

	# Method 1: Try to play pre-recorded audio file if available
	var audio_path = "res://audio/words/%s.wav" % word.to_lower()
	if ResourceLoader.exists(audio_path):
		instruction_label.text = "🔊 Playing audio... Listen carefully!"
		var audio_stream = load(audio_path)
		var player = AudioStreamPlayer.new()
		add_child(player)
		player.stream = audio_stream
		player.play()
		await player.finished
		player.queue_free()
		instruction_label.text = "Type what you heard:"
		return

	# Method 2: Use text-to-speech if available (fallback)
	if DisplayServer.tts_is_speaking() or DisplayServer.tts_is_paused():
		DisplayServer.tts_stop()

	instruction_label.text = "🔊 Speaking word... Listen carefully!"
	DisplayServer.tts_speak(word, "en", 50, 1.0, 1.0, 0, false)

	# Wait for TTS to finish (estimate based on word length)
	var estimated_duration = max(1.0, word.length() * 0.15)
	if is_inside_tree():
		await get_tree().create_timer(estimated_duration).timeout
	else:
		push_warning("Spelling activity not in scene tree, skipping audio hint timer")

	instruction_label.text = "Type what you heard:"

	# Debug: Print word in debug builds
	if Config and Config.is_debug_mode():
		Logger.debug("Spelling word: " + word, "Spelling")

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
	AnimationHelper.show_validation_error(answer_input)

func _on_loading_started():
	answer_input.editable = false
	submit_button.disabled = true
	play_audio_button.disabled = true

func _on_loading_ended():
	answer_input.editable = true
	submit_button.disabled = false
	play_audio_button.disabled = false
