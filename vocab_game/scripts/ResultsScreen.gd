extends Control

@onready var title_label = $CenterContainer/VBoxContainer/TitleLabel
@onready var activities_label = $CenterContainer/VBoxContainer/ActivitiesLabel
@onready var words_label = $CenterContainer/VBoxContainer/WordsLabel
@onready var continue_button = $CenterContainer/VBoxContainer/ContinueButton
@onready var main_menu_button = $CenterContainer/VBoxContainer/MainMenuButton

func _ready():
	continue_button.pressed.connect(_on_continue_pressed)
	main_menu_button.pressed.connect(_on_main_menu_pressed)

	# Hide buttons initially for dramatic reveal
	continue_button.modulate.a = 0
	main_menu_button.modulate.a = 0

	# Display session results
	_show_results()

	# Animate the results screen entrance
	_animate_entrance()

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

func _animate_entrance():
	# Bounce in title
	title_label.scale = Vector2(0, 0)
	var title_tween = create_tween()
	title_tween.set_ease(Tween.EASE_OUT)
	title_tween.set_trans(Tween.TRANS_BACK)
	title_tween.tween_property(title_label, "scale", Vector2(1, 1), 0.6)

	# Wait a bit, then fade in the stats
	await title_tween.finished
	await get_tree().create_timer(0.2).timeout

	# Fade in stats labels one by one
	var labels = [activities_label, words_label]
	for label in labels:
		label.modulate.a = 0
		var tween = create_tween()
		tween.tween_property(label, "modulate:a", 1.0, 0.4)
		await get_tree().create_timer(0.2).timeout

	# Fade in buttons
	var button_tween = create_tween()
	button_tween.set_parallel(true)
	button_tween.tween_property(continue_button, "modulate:a", 1.0, 0.5)
	button_tween.tween_property(main_menu_button, "modulate:a", 1.0, 0.5)

	# Check if this was a good performance and add celebration
	var summary = SessionManager.last_session_summary
	if not summary.is_empty() and not summary.has("error"):
		var total = summary.get("totalAttempts", 0)
		var correct = summary.get("correctAttempts", 0)
		var accuracy = (float(correct) / float(total) * 100.0) if total > 0 else 0.0

		if accuracy >= 70:
			_create_celebration_effect()

func _create_celebration_effect():
	# Create confetti effect
	var emojis = ["🎉", "🎊", "⭐", "✨", "🌟", "💫", "🏆", "👏"]

	for i in range(20):
		var label = Label.new()
		label.text = emojis[i % emojis.size()]
		label.add_theme_font_size_override("font_size", 40)

		# Random starting position at top of screen
		label.position = Vector2(randf_range(100, get_viewport_rect().size.x - 100), -50)
		add_child(label)

		# Animate falling and rotating
		var duration = randf_range(2.0, 3.5)
		var tween = create_tween()
		tween.set_parallel(true)
		tween.tween_property(label, "position:y", get_viewport_rect().size.y + 50, duration)
		tween.tween_property(label, "position:x", label.position.x + randf_range(-100, 100), duration)
		tween.tween_property(label, "rotation", randf_range(-PI, PI), duration)

		# Cleanup
		await get_tree().create_timer(duration).timeout
		label.queue_free()
