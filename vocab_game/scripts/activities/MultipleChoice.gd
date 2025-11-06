extends Control

signal answer_submitted(answer)

@onready var question_label = $VBoxContainer/QuestionLabel
@onready var options_container = $VBoxContainer/OptionsContainer

var word_data: Dictionary
var options: Array
var selected_answer: String = ""

func _ready():
	pass

func setup(activity_data: Dictionary):
	word_data = activity_data.word
	options = activity_data.options
	
	question_label.text = "What does '%s' mean?" % word_data.word
	
	# Create option buttons
	for i in range(options.size()):
		var button = Button.new()
		button.text = options[i]
		button.custom_minimum_size = Vector2(600, 60)
		button.pressed.connect(_on_option_selected.bind(options[i]))
		options_container.add_child(button)

func _on_option_selected(answer: String):
	selected_answer = answer
	answer_submitted.emit(answer)
