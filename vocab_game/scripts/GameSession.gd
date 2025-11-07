extends Control

# Animation constants
const FADE_OUT_DURATION = 0.2
const FADE_IN_DURATION = 0.3
const FEEDBACK_SCALE_DURATION = 0.5
const FEEDBACK_BOUNCE_DURATION = 0.2
const SHAKE_ROTATION_ANGLE = 5
const SHAKE_STEP_DURATION = 0.05
const CELEBRATION_PARTICLE_COUNT = 8
const CELEBRATION_PARTICLE_RISE = 150
const CELEBRATION_DURATION = 1.5
const FEEDBACK_DISPLAY_TIME = 1.5

# Color constants
const COLOR_CORRECT = Color(0.2, 0.8, 0.3)
const COLOR_INCORRECT = Color(0.9, 0.3, 0.2)
const COLOR_OVERLAY = Color(0, 0, 0, 0.5)

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
var is_transitioning: bool = false
var active_particles: Array[Label] = []

func _ready():
	feedback_panel.hide()
	next_button.pressed.connect(_on_next_button_pressed)

	# Create loading overlay using CanvasLayer for guaranteed top-level rendering
	loading_overlay = CanvasLayer.new()
	loading_overlay.layer = 100  # High layer to ensure it's on top
	add_child(loading_overlay)

	# Create background ColorRect
	var overlay_bg = ColorRect.new()
	overlay_bg.color = COLOR_OVERLAY
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
	# Prevent race conditions from rapid user input
	if is_transitioning:
		return
	is_transitioning = true

	# Fade out previous activity
	if current_activity_node:
		var fade_out = create_tween()
		fade_out.tween_property(current_activity_node, "modulate:a", 0, FADE_OUT_DURATION)
		await fade_out.finished
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
		current_activity_node.modulate.a = 0  # Start invisible
		activity_container.add_child(current_activity_node)
		current_activity_node.setup(activity_data)

		# Connect answer submission
		if current_activity_node.has_signal("answer_submitted"):
			current_activity_node.answer_submitted.connect(_on_answer_submitted)

		# Fade in new activity
		var fade_in = create_tween()
		fade_in.tween_property(current_activity_node, "modulate:a", 1, FADE_IN_DURATION)

	is_transitioning = false

func _on_answer_submitted(answer: String):
	SessionManager.submit_answer(answer)

func _on_attempt_result(correct: bool, feedback: String):
	feedback_label.text = feedback

	if correct:
		feedback_panel.modulate = COLOR_CORRECT
		_play_correct_animation()
	else:
		feedback_panel.modulate = COLOR_INCORRECT
		_play_incorrect_animation()

	# Pop in the feedback panel with scale animation
	feedback_panel.scale = Vector2(0, 0)
	feedback_panel.show()

	var tween = create_tween()
	tween.set_ease(Tween.EASE_OUT)
	tween.set_trans(Tween.TRANS_BACK)
	tween.tween_property(feedback_panel, "scale", Vector2(1, 1), FEEDBACK_SCALE_DURATION)

func _play_correct_animation():
	# Bounce animation for the feedback panel
	var tween = create_tween()
	tween.set_ease(Tween.EASE_OUT)
	tween.tween_property(feedback_panel, "scale", Vector2(1.1, 1.1), FEEDBACK_BOUNCE_DURATION)
	tween.tween_property(feedback_panel, "scale", Vector2(1.0, 1.0), FEEDBACK_BOUNCE_DURATION)

	# Add sparkle/star effect
	_create_celebration_particles()

func _play_incorrect_animation():
	# Gentle shake animation using rotation
	var tween = create_tween()
	tween.tween_property(feedback_panel, "rotation", deg_to_rad(-SHAKE_ROTATION_ANGLE), SHAKE_STEP_DURATION)
	tween.tween_property(feedback_panel, "rotation", deg_to_rad(SHAKE_ROTATION_ANGLE), SHAKE_STEP_DURATION)
	tween.tween_property(feedback_panel, "rotation", deg_to_rad(-SHAKE_ROTATION_ANGLE / 2), SHAKE_STEP_DURATION)
	tween.tween_property(feedback_panel, "rotation", deg_to_rad(SHAKE_ROTATION_ANGLE / 2), SHAKE_STEP_DURATION)
	tween.tween_property(feedback_panel, "rotation", 0, SHAKE_STEP_DURATION)

func _create_celebration_particles():
	# Create simple celebration effect with labels
	var celebration_words = ["⭐", "✨", "🌟", "🎉", "🎊"]

	for i in range(CELEBRATION_PARTICLE_COUNT):
		if not is_inside_tree():
			break  # Stop creating particles if scene is being destroyed

		var label = Label.new()
		label.text = celebration_words[i % celebration_words.size()]
		label.add_theme_font_size_override("font_size", 32)
		label.modulate = Color(1, 1, 1, 1)

		# Random position around the feedback panel
		var random_x = randf_range(-200, 200)
		var random_y = randf_range(-100, 100)
		label.position = feedback_panel.position + Vector2(random_x, random_y)

		add_child(label)
		active_particles.append(label)

		# Animate the particle
		var tween = create_tween()
		tween.set_parallel(true)
		tween.tween_property(label, "position:y", label.position.y - CELEBRATION_PARTICLE_RISE, CELEBRATION_DURATION)
		tween.tween_property(label, "modulate:a", 0, CELEBRATION_DURATION)
		tween.tween_property(label, "scale", Vector2(1.5, 1.5), CELEBRATION_DURATION)

		# Clean up after animation - only if still in tree
		tween.finished.connect(func():
			if is_instance_valid(label):
				active_particles.erase(label)
				label.queue_free()
		)

func _on_next_button_pressed():
	# Scale out the feedback panel
	var tween = create_tween()
	tween.set_ease(Tween.EASE_IN)
	tween.set_trans(Tween.TRANS_BACK)
	tween.tween_property(feedback_panel, "scale", Vector2(0, 0), FADE_IN_DURATION)
	await tween.finished
	feedback_panel.hide()
	feedback_panel.scale = Vector2(1, 1)  # Reset for next time

func _exit_tree():
	# Clean up any active particles to prevent memory leaks
	for label in active_particles:
		if is_instance_valid(label):
			label.queue_free()
	active_particles.clear()

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
	feedback_panel.modulate = COLOR_INCORRECT

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
