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
	var progress = SessionManager.get_progress_data()
	
	title_label.text = "Session Complete!"
	words_label.text = "Total Words Learned: %d" % progress.words_learned
	
	if progress.accuracy >= 80:
		title_label.text = "Excellent Work!"
	elif progress.accuracy >= 60:
		title_label.text = "Good Job!"
	else:
		title_label.text = "Keep Practicing!"

func _on_continue_pressed():
	get_tree().change_scene_to_file("res://scenes/GameSession.tscn")

func _on_main_menu_pressed():
	get_tree().change_scene_to_file("res://scenes/MainMenu.tscn")
