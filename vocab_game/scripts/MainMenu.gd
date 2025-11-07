extends Control

@onready var start_button = $CenterContainer/VBoxContainer/StartButton
@onready var progress_button = $CenterContainer/VBoxContainer/ProgressButton
@onready var title_label = $CenterContainer/VBoxContainer/TitleLabel
@onready var subtitle_label = $CenterContainer/VBoxContainer/SubtitleLabel

var is_loading: bool = false

func _ready():
	start_button.pressed.connect(_on_start_button_pressed)
	progress_button.pressed.connect(_on_progress_button_pressed)

	# Set initial button text with emoji
	_set_buttons_enabled(true)

	# Add welcoming animation
	_animate_title()

	# Connect to loading signals
	SessionManager.loading_started.connect(_on_loading_started)
	SessionManager.loading_ended.connect(_on_loading_ended)

func _animate_title():
	# Simple pulse animation for the title
	var tween = create_tween()
	tween.set_loops()
	tween.tween_property(title_label, "scale", Vector2(1.05, 1.05), 1.0)
	tween.tween_property(title_label, "scale", Vector2(1.0, 1.0), 1.0)

func _on_start_button_pressed():
	if is_loading:
		return

	_set_buttons_enabled(false)
	start_button.text = "Starting... 🚀"

	# Small delay for visual feedback
	await get_tree().create_timer(0.3).timeout
	get_tree().change_scene_to_file("res://scenes/GameSession.tscn")

func _on_progress_button_pressed():
	if is_loading:
		return

	_set_buttons_enabled(false)
	progress_button.text = "Loading... 📊"

	# Small delay for visual feedback
	await get_tree().create_timer(0.3).timeout
	get_tree().change_scene_to_file("res://scenes/ProgressScreen.tscn")

func _on_loading_started():
	is_loading = true
	_set_buttons_enabled(false)

func _on_loading_ended():
	is_loading = false
	_set_buttons_enabled(true)

func _set_buttons_enabled(enabled: bool):
	start_button.disabled = not enabled
	progress_button.disabled = not enabled

	if enabled:
		start_button.text = "🎮 Start Learning!"
		progress_button.text = "📊 View Progress"
