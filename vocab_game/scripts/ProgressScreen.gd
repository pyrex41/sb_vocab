extends Control

@onready var words_learned_label = $CenterContainer/VBoxContainer/WordsLearnedLabel
@onready var accuracy_label = $CenterContainer/VBoxContainer/AccuracyLabel
@onready var total_attempts_label = $CenterContainer/VBoxContainer/TotalAttemptsLabel
@onready var back_button = $CenterContainer/VBoxContainer/BackButton

func _ready():
	back_button.pressed.connect(_on_back_button_pressed)
	_update_progress()

func _update_progress():
	var progress = SessionManager.get_progress_data()
	
	words_learned_label.text = "Words Learned: %d" % progress.words_learned
	accuracy_label.text = "Accuracy: %.1f%%" % progress.accuracy
	total_attempts_label.text = "Total Attempts: %d" % progress.total_attempts

func _on_back_button_pressed():
	get_tree().change_scene_to_file("res://scenes/MainMenu.tscn")
