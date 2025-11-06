extends Control

var session_data = {}
var current_activity_index = 0
var total_score = 0
var activities = []

func _ready():
	add_to_group("session")
	$AnswerContainer.clear()

func set_session_data(data: Dictionary):
	session_data = data
	activities = data.get("activities", [])
	load_next_activity()

func load_next_activity():
	if current_activity_index >= activities.size():
		_end_session()
		return

	var activity = activities[current_activity_index]
	_clear_activity_container()

	match activity.get("activity_type"):
		"flashcard":
			_load_flashcard(activity)
		"multiple_choice":
			_load_multiple_choice(activity)
		_:
			_load_placeholder(activity)

	_update_progress_label()

func _load_flashcard(activity: Dictionary):
	var label = Label.new()
	label.text = "Word: %s\n\nDefinition: Learning mode - no answer needed\n\nExample: This is where you'll practice!" % activity.get("word", "Unknown")
	label.custom_minimum_size = Vector2(400, 200)
	$ActivityContainer.add_child(label)

	var continue_button = Button.new()
	continue_button.text = "Continue"
	continue_button.pressed.connect(_on_activity_complete.bindv([activity, "learned"]))
	$AnswerContainer.add_child(continue_button)

func _load_multiple_choice(activity: Dictionary):
	var label = Label.new()
	label.text = "What does '%s' mean?" % activity.get("word", "Unknown")
	label.custom_minimum_size = Vector2(400, 100)
	$ActivityContainer.add_child(label)

	var options = [
		"Lasting for a very short time",
		"Very large and heavy",
		"Quick and fast-moving"
	]

	for option in options:
		var button = Button.new()
		button.text = option
		button.pressed.connect(_on_activity_complete.bindv([activity, option]))
		$AnswerContainer.add_child(button)

func _load_placeholder(activity: Dictionary):
	var label = Label.new()
	label.text = "Activity: %s\n\nWord: %s" % [activity.get("activity_type", "Unknown"), activity.get("word", "Unknown")]
	label.custom_minimum_size = Vector2(400, 200)
	$ActivityContainer.add_child(label)

func _clear_activity_container():
	for child in $ActivityContainer.get_children():
		child.queue_free()
	for child in $AnswerContainer.get_children():
		child.queue_free()

func _on_activity_complete(activity: Dictionary, user_answer: String):
	var result = PlaycademySDK.attempt_activity(
		activity.get("activity_type", ""),
		user_answer,
		activity.get("word_id", "")
	)

	total_score += result.get("score", 0)
	current_activity_index += 1

	load_next_activity()

func _update_progress_label():
	$TopBar/ProgressLabel.text = "Progress: %d / %d" % [current_activity_index + 1, activities.size()]
	$TopBar/ScoreLabel.text = "Score: %d" % total_score

func _end_session():
	var result = PlaycademySDK.end_session()
	get_tree().change_scene_to_file("res://scenes/session_complete.tscn")

func _on_submit_pressed():
	pass
