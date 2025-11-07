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
	# Validate required fields
	if not activity_data.has("word"):
		push_error("MultipleChoice: activity_data missing 'word' field")
		question_label.text = "Error: Invalid activity data"
		return

	word_data = activity_data.word

	if not activity_data.has("options"):
		push_error("MultipleChoice: activity_data missing 'options' field")
		question_label.text = "Error: No options provided"
		_create_error_button()
		return

	options = activity_data.options

	if not options is Array or options.is_empty():
		push_error("MultipleChoice: options is empty or invalid")
		question_label.text = "Error: Invalid options from server"
		_create_error_button()
		return

	question_label.text = "What does '%s' mean?" % word_data.word

	# Create option buttons
	for i in range(options.size()):
		var button = Button.new()
		button.text = options[i]
		button.custom_minimum_size = Vector2(600, 60)
		button.pressed.connect(_on_option_selected.bind(options[i]))
		options_container.add_child(button)

func _create_error_button():
	"""Create skip button if data is invalid"""
	var error_button = Button.new()
	error_button.text = "Skip This Activity"
	error_button.custom_minimum_size = Vector2(600, 60)
	error_button.pressed.connect(_on_skip_activity)
	options_container.add_child(error_button)

func _on_skip_activity():
	answer_submitted.emit("")

func _on_option_selected(answer: String):
	selected_answer = answer
	answer_submitted.emit(answer)
