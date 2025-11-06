extends Control

@onready var start_button = $CenterContainer/VBoxContainer/StartButton
@onready var progress_button = $CenterContainer/VBoxContainer/ProgressButton
@onready var title_label = $CenterContainer/VBoxContainer/TitleLabel

func _ready():
	start_button.pressed.connect(_on_start_button_pressed)
	progress_button.pressed.connect(_on_progress_button_pressed)

func _on_start_button_pressed():
	get_tree().change_scene_to_file("res://scenes/GameSession.tscn")

func _on_progress_button_pressed():
	get_tree().change_scene_to_file("res://scenes/ProgressScreen.tscn")
