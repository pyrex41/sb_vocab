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
	word_data = activity_data.word
	instruction_label.text = "Listen to the word and spell it correctly"
	answer_input.placeholder_text = "Type the word here..."

	# DEBUG: Print the word to console for testing (since we don't have audio)
	if word_data.has("definition"):
		var def = word_data.definition
		# Extract word from "word (definition)" format
		var word_match = def.split(" ")[0]
		print("DEBUG: Current word is '%s'" % word_match)

	# Auto-play the audio once
	_play_audio_hint()

func _on_play_audio():
	_play_audio_hint()

func _play_audio_hint():
	# In a real implementation, this would play the audio file
	# For now, just show a hint message
	instruction_label.text = "🔊 Playing audio... Listen carefully!"
	await get_tree().create_timer(1.0).timeout
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
