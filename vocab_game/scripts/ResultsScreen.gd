extends Control

@onready var title_label = $CenterContainer/VBoxContainer/TitleLabel
@onready var activities_label = $CenterContainer/VBoxContainer/ActivitiesLabel
@onready var words_label = $CenterContainer/VBoxContainer/WordsLabel
@onready var continue_button = $CenterContainer/VBoxContainer/ContinueButton
@onready var main_menu_button = $CenterContainer/VBoxContainer/MainMenuButton

func _ready():
	continue_button.pressed.connect(_on_continue_pressed)
	main_menu_button.pressed.connect(_on_main_menu_pressed)
	
	# Display session results
	_show_results()

func _show_results():
	var summary = SessionManager.last_session_summary

	# Handle missing or error summary
	if summary.is_empty() or summary.has("error"):
		title_label.text = "Session Complete!"
		activities_label.text = "Thanks for playing!"
		words_label.text = "Keep up the great work!"
		return

	# Display session statistics
	var total = summary.get("totalAttempts", 0)
	var correct = summary.get("correctAttempts", 0)
	var accuracy = (float(correct) / float(total) * 100.0) if total > 0 else 0.0

	activities_label.text = "Activities Completed: %d" % total
	words_label.text = "Correct: %d / %d (%.0f%%)" % [correct, total, accuracy]

	# Encouraging title based on performance
	if accuracy >= 90:
		title_label.text = "🌟 Outstanding! 🌟"
	elif accuracy >= 80:
		title_label.text = "⭐ Excellent Work! ⭐"
	elif accuracy >= 70:
		title_label.text = "✨ Great Job! ✨"
	elif accuracy >= 60:
		title_label.text = "👍 Good Effort! 👍"
	else:
		title_label.text = "Keep Practicing! 💪"

func _on_continue_pressed():
	get_tree().change_scene_to_file("res://scenes/GameSession.tscn")

func _on_main_menu_pressed():
	get_tree().change_scene_to_file("res://scenes/MainMenu.tscn")
