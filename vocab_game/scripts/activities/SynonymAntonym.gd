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
	options = activity_data.options if activity_data.has("options") else []

	# Determine if this is synonym or antonym based on activity data or randomly
	is_synonym = randi() % 2 == 0  # Randomly choose synonym or antonym

	if is_synonym:
		question_label.text = "Choose a SYNONYM for '%s'" % word_data.word
	else:
		question_label.text = "Choose an ANTONYM for '%s'" % word_data.word

	# Create option buttons using API-provided options
	for option in options:
		var button = Button.new()
		button.text = option
		button.custom_minimum_size = Vector2(500, 50)
		button.pressed.connect(_on_option_selected.bind(option))
		options_container.add_child(button)

func _on_option_selected(answer: String):
	answer_submitted.emit(answer)
