extends Control

func _ready():
	var progress = PlaycademySDK.get_progress()
	var stats_text = "Sessions: %d\nWords Learned: %d\nCurrent Streak: %d" % [
		progress.get("total_sessions", 0),
		progress.get("total_words_learned", 0),
		progress.get("current_streak", 0)
	]
	$VBoxContainer/Stats.text = stats_text

func _on_back_pressed():
	get_tree().change_scene_to_file("res://scenes/main_menu.tscn")
