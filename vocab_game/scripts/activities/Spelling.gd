extends Control

signal answer_submitted(answer)

@onready var instruction_label = $VBoxContainer/InstructionLabel
@onready var play_audio_button = $VBoxContainer/PlayAudioButton
@onready var answer_input = $VBoxContainer/AnswerInput
@onready var submit_button = $VBoxContainer/SubmitButton

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
	
	# Auto-play the audio once
	_play_audio_hint()

func _on_play_audio():
	_play_audio_hint()

func _play_audio_hint():
	# In a real implementation, this would play the audio file
	# For mock, we'll show the word briefly as a hint system
	instruction_label.text = "Spell this word: (playing audio for '%s')" % word_data.word
	await get_tree().create_timer(0.5).timeout
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
