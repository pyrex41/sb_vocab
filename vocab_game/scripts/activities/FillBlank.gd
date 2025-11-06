extends Control

signal answer_submitted(answer)

@onready var instruction_label = $VBoxContainer/InstructionLabel
@onready var sentence_label = $VBoxContainer/SentenceLabel
@onready var answer_input = $VBoxContainer/AnswerInput
@onready var submit_button = $VBoxContainer/SubmitButton

var word_data: Dictionary

func _ready():
	submit_button.pressed.connect(_on_submit)

func setup(activity_data: Dictionary):
	word_data = activity_data.word
	instruction_label.text = "Fill in the blank with the correct word"
	
	# Replace the word in the example sentence with a blank
	var sentence = word_data.example
	var word = word_data.word
	var blank_sentence = sentence.replace(word, "______")
	# Handle case variations
	blank_sentence = blank_sentence.replace(word.capitalize(), "______")
	blank_sentence = blank_sentence.replace(word.to_upper(), "______")
	blank_sentence = blank_sentence.replace(word.to_lower(), "______")
	
	sentence_label.text = blank_sentence
	answer_input.placeholder_text = "Type the missing word..."

func _on_submit():
	var answer = answer_input.text.strip_edges()
	if answer.length() > 0:
		answer_submitted.emit(answer)
