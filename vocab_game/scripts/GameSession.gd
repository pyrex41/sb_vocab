extends Control

@onready var activity_container = $ActivityContainer
@onready var progress_label = $TopBar/ProgressLabel
@onready var feedback_panel = $FeedbackPanel
@onready var feedback_label = $FeedbackPanel/MarginContainer/VBoxContainer/FeedbackLabel
@onready var next_button = $FeedbackPanel/MarginContainer/VBoxContainer/NextButton

# Loading overlay (created programmatically)
var loading_overlay: CanvasLayer
var retry_button: Button

# Activity scene references
var flashcard_scene = preload("res://scenes/activities/Flashcard.tscn")
var multiple_choice_scene = preload("res://scenes/activities/MultipleChoice.tscn")
var spelling_scene = preload("res://scenes/activities/Spelling.tscn")
var fill_blank_scene = preload("res://scenes/activities/FillBlank.tscn")
var synonym_antonym_scene = preload("res://scenes/activities/SynonymAntonym.tscn")

var current_activity_node = null

func _ready():
	feedback_panel.hide()
	next_button.pressed.connect(_on_next_button_pressed)

	# Create loading overlay using CanvasLayer for guaranteed top-level rendering
	loading_overlay = CanvasLayer.new()
	loading_overlay.layer = 100  # High layer to ensure it's on top
	add_child(loading_overlay)

	# Create background ColorRect
	var overlay_bg = ColorRect.new()
	overlay_bg.color = Color(0, 0, 0, 0.5)  # Semi-transparent black
	overlay_bg.set_anchors_preset(Control.PRESET_FULL_RECT)
	overlay_bg.mouse_filter = Control.MOUSE_FILTER_STOP  # Block clicks during loading
	loading_overlay.add_child(overlay_bg)

	# Add loading text
	var loading_label = Label.new()
	loading_label.text = "Loading..."
	loading_label.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	loading_label.vertical_alignment = VERTICAL_ALIGNMENT_CENTER
	loading_label.set_anchors_preset(Control.PRESET_FULL_RECT)
	loading_label.add_theme_font_size_override("font_size", 32)
	overlay_bg.add_child(loading_label)

	loading_overlay.hide()

	SessionManager.activity_changed.connect(_on_activity_changed)
	SessionManager.attempt_result.connect(_on_attempt_result)
	SessionManager.session_ended.connect(_on_session_ended)
	SessionManager.loading_started.connect(_on_loading_started)
	SessionManager.loading_ended.connect(_on_loading_ended)
	SessionManager.api_error.connect(_on_api_error)

	# Start the session
	SessionManager.start_new_session("grade3")

func _on_activity_changed(activity_data: Dictionary, index: int, total: int):
	progress_label.text = "Activity %d / %d" % [index, total]
	_load_activity(activity_data)

func _load_activity(activity_data: Dictionary):
	# Clear previous activity
	if current_activity_node:
		current_activity_node.queue_free()
		current_activity_node = null

	# Load appropriate activity scene
	var scene = null
	var activity_type = activity_data.activityType if activity_data.has("activityType") else activity_data.get("type", "")
	match activity_type:
		"flashcard":
			scene = flashcard_scene
		"multiple_choice", "definition_mc":
			scene = multiple_choice_scene
		"spelling", "spell_typed":
			scene = spelling_scene
		"fill_blank", "cloze_typed":
			scene = fill_blank_scene
		"synonym_antonym":
			scene = synonym_antonym_scene
	
	if scene:
		current_activity_node = scene.instantiate()
		activity_container.add_child(current_activity_node)
		current_activity_node.setup(activity_data)
		
		# Connect answer submission
		if current_activity_node.has_signal("answer_submitted"):
			current_activity_node.answer_submitted.connect(_on_answer_submitted)

func _on_answer_submitted(answer: String):
	SessionManager.submit_answer(answer)

func _on_attempt_result(correct: bool, feedback: String):
	feedback_label.text = feedback
	
	if correct:
		feedback_panel.modulate = Color(0.2, 0.8, 0.3)  # Green
	else:
		feedback_panel.modulate = Color(0.9, 0.3, 0.2)  # Red
	
	feedback_panel.show()

func _on_next_button_pressed():
	feedback_panel.hide()

func _on_session_ended(summary: Dictionary):
	# Navigate to results screen
	get_tree().change_scene_to_file("res://scenes/ResultsScreen.tscn")

func _on_loading_started():
	if loading_overlay:
		loading_overlay.show()

func _on_loading_ended():
	if loading_overlay:
		loading_overlay.hide()

func _on_api_error(message: String):
	# Show error message to user with retry option
	feedback_label.text = message + "\n\nWould you like to try again?"
	feedback_panel.modulate = Color(0.9, 0.3, 0.2)  # Red

	# Change "Continue" button to "Retry"
	next_button.text = "Retry"
	# Disconnect old handler if connected
	if next_button.pressed.is_connected(_on_next_button_pressed):
		next_button.pressed.disconnect(_on_next_button_pressed)
	# Connect retry handler
	next_button.pressed.connect(_on_retry_button_pressed)

	feedback_panel.show()
	push_error("API Error: " + message)

func _on_retry_button_pressed():
	# Restore button text and handler
	next_button.text = "Continue"
	if next_button.pressed.is_connected(_on_retry_button_pressed):
		next_button.pressed.disconnect(_on_retry_button_pressed)
	next_button.pressed.connect(_on_next_button_pressed)

	feedback_panel.hide()
	# Retry the last failed operation
	SessionManager.retry_last_operation()
