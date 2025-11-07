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
	# Validate activity_data has required fields
	if not activity_data.has("word"):
		push_error("SynonymAntonym: activity_data missing 'word' field")
		question_label.text = "Error: Invalid activity data"
		return

	word_data = activity_data.word

	# Validate options array exists and is not empty
	if not activity_data.has("options"):
		push_error("SynonymAntonym: activity_data missing 'options' field")
		question_label.text = "Error: No options provided by server"
		_create_error_button()
		return

	options = activity_data.options

	if not options is Array or options.is_empty():
		push_error("SynonymAntonym: options is empty or not an array")
		question_label.text = "Error: Invalid options from server"
		_create_error_button()
		return

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

func _create_error_button():
	"""Create a button to skip this activity if data is invalid"""
	var error_button = Button.new()
	error_button.text = "Skip This Activity"
	error_button.custom_minimum_size = Vector2(500, 50)
	error_button.pressed.connect(_on_skip_activity)
	options_container.add_child(error_button)

func _on_skip_activity():
	# Submit empty answer to trigger skip
	answer_submitted.emit("")

func _on_option_selected(answer: String):
	answer_submitted.emit(answer)
