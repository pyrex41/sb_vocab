extends Control

signal answer_submitted(answer)

@onready var question_label = $VBoxContainer/QuestionLabel
@onready var options_container = $VBoxContainer/OptionsContainer

var word_data: Dictionary
var options: Array
var option_buttons: Array[Button] = []

func _ready():
	# Connect to loading signals to disable buttons during API calls
	SessionManager.loading_started.connect(_on_loading_started)
	SessionManager.loading_ended.connect(_on_loading_ended)

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

	# Clear previous buttons
	option_buttons.clear()
	for child in options_container.get_children():
		child.queue_free()

	# Create option buttons
	for i in range(options.size()):
		var button = Button.new()
		button.text = options[i]
		button.custom_minimum_size = Vector2(600, 60)
		button.pressed.connect(_on_option_selected.bind(options[i], button))
		options_container.add_child(button)
		option_buttons.append(button)

func _create_error_button():
	"""Create skip button if data is invalid"""
	var error_button = Button.new()
	error_button.text = "Skip This Activity"
	error_button.custom_minimum_size = Vector2(600, 60)
	error_button.pressed.connect(_on_skip_activity)
	options_container.add_child(error_button)

func _on_skip_activity():
	answer_submitted.emit("")

func _on_option_selected(answer: String, selected_button: Button):
	# Disable all buttons to prevent double-submission
	for button in option_buttons:
		button.disabled = true

	# Visual feedback: highlight the selected button
	selected_button.modulate = Color(0.7, 1.0, 0.7)  # Light green tint

	answer_submitted.emit(answer)

func _on_loading_started():
	# Disable all option buttons during API call
	for button in option_buttons:
		button.disabled = true

func _on_loading_ended():
	# Re-enable all option buttons after API call
	for button in option_buttons:
		button.disabled = false
		button.modulate = Color.WHITE  # Reset color
