extends Control

signal answer_submitted(answer)

@onready var question_label = $VBoxContainer/QuestionLabel
@onready var options_container = $VBoxContainer/OptionsContainer

var word_data: Dictionary
var is_synonym: bool
var options: Array

func _ready():
	pass

func setup(activity_data: Dictionary):
	word_data = activity_data.word
	is_synonym = randi() % 2 == 0  # Randomly choose synonym or antonym
	
	if is_synonym:
		question_label.text = "Choose a SYNONYM for '%s'" % word_data.word
		options = word_data.synonyms.duplicate()
	else:
		question_label.text = "Choose an ANTONYM for '%s'" % word_data.word
		options = word_data.antonyms.duplicate()
	
	# Add some wrong options
	var all_words = word_data.synonyms + word_data.antonyms
	while options.size() < 4 and all_words.size() > 0:
		var wrong_option = all_words[randi() % all_words.size()]
		if wrong_option not in options:
			options.append(wrong_option)
		all_words.erase(wrong_option)
	
	options.shuffle()
	
	# Create option buttons
	for option in options:
		var button = Button.new()
		button.text = option
		button.custom_minimum_size = Vector2(500, 50)
		button.pressed.connect(_on_option_selected.bind(option))
		options_container.add_child(button)

func _on_option_selected(answer: String):
	answer_submitted.emit(answer)
