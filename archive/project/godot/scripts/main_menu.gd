extends Control

func _ready():
	pass

func _on_start_pressed():
	get_tree().change_scene_to_file("res://scenes/session_loader.tscn")

func _on_progress_pressed():
	get_tree().change_scene_to_file("res://scenes/progress_screen.tscn")

func _on_settings_pressed():
	get_tree().change_scene_to_file("res://scenes/settings_screen.tscn")
