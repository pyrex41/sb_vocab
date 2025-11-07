extends Control

signal answer_submitted(answer)

@onready var instruction_label = $VBoxContainer/InstructionLabel
@onready var sentence_label = $VBoxContainer/SentenceLabel
@onready var answer_input = $VBoxContainer/AnswerInput
@onready var submit_button = $VBoxContainer/SubmitButton

var word_data: Dictionary

func _ready():
	submit_button.pressed.connect(_on_submit)
	answer_input.text_submitted.connect(_on_input_submitted)

	# Connect to loading signals to disable inputs during API calls
	SessionManager.loading_started.connect(_on_loading_started)
	SessionManager.loading_ended.connect(_on_loading_ended)

func setup(activity_data: Dictionary):
	word_data = activity_data.word
	instruction_label.text = "Fill in the blank with the correct word 📝"

	# Replace the word in the example sentence with a blank
	var sentence = word_data.example
	var word = word_data.word
	var blank_sentence = sentence.replace(word, "______")
	# Handle case variations
	blank_sentence = blank_sentence.replace(word.capitalize(), "______")
	blank_sentence = blank_sentence.replace(word.to_upper(), "______")
	blank_sentence = blank_sentence.replace(word.to_lower(), "______")

	sentence_label.text = blank_sentence
	answer_input.text = ""
	answer_input.placeholder_text = "Type your answer here..."

	# Focus the input field for better UX
	answer_input.grab_focus()

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

func _on_loading_ended():
	answer_input.editable = true
	submit_button.disabled = false
