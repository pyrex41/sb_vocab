extends Control

@onready var words_learned_label = $CenterContainer/VBoxContainer/WordsLearnedLabel
@onready var accuracy_label = $CenterContainer/VBoxContainer/AccuracyLabel
@onready var total_attempts_label = $CenterContainer/VBoxContainer/TotalAttemptsLabel
@onready var back_button = $CenterContainer/VBoxContainer/BackButton
@onready var title_label = $CenterContainer/VBoxContainer/TitleLabel

func _ready():
	back_button.pressed.connect(_on_back_button_pressed)

	# Connect to loading signals for visual feedback
	SessionManager.loading_started.connect(_on_loading_started)
	SessionManager.loading_ended.connect(_on_loading_ended)

	# Show loading state
	_show_loading()

	# Load progress data asynchronously
	_update_progress()

func _show_loading():
	words_learned_label.text = "📚 Loading progress..."
	accuracy_label.text = ""
	total_attempts_label.text = ""
	back_button.disabled = true

func _update_progress():
	var progress = await SessionManager.get_progress_data()

	# Handle missing or error data gracefully
	if progress.is_empty() or progress.get("words_learned", 0) == 0:
		_show_empty_progress()
		return

	# Display progress with emoji and kid-friendly formatting
	words_learned_label.text = "📚 Words Learned: %d" % progress.words_learned
	accuracy_label.text = "🎯 Accuracy: %.0f%%" % progress.accuracy
	total_attempts_label.text = "✨ Total Attempts: %d" % progress.total_attempts

	# Add encouraging message based on progress
	if progress.words_learned >= 50:
		title_label.text = "🌟 Your Amazing Progress! 🌟"
	elif progress.words_learned >= 20:
		title_label.text = "⭐ Your Great Progress! ⭐"
	else:
		title_label.text = "📊 Your Progress 📊"

	back_button.disabled = false

func _show_empty_progress():
	title_label.text = "📊 Your Progress 📊"
	words_learned_label.text = "Start learning to see your progress!"
	accuracy_label.text = "Complete activities to track your accuracy"
	total_attempts_label.text = "Try your first session now! 🚀"
	back_button.disabled = false

func _on_loading_started():
	back_button.disabled = true

func _on_loading_ended():
	back_button.disabled = false

func _on_back_button_pressed():
	get_tree().change_scene_to_file("res://scenes/MainMenu.tscn")
