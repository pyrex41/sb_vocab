extends Control

func _ready():
	pass

func _on_grade_selected(grade: int):
	var result = PlaycademySDK.start_session(grade)
	if result.has("session_id"):
		get_tree().call_group_flags(SceneTreeCallGroup.GROUP_CALL_DEFERRED, "session", "set_session_data", result)
		get_tree().change_scene_to_file("res://scenes/session_activity.tscn")

func _on_back_pressed():
	get_tree().change_scene_to_file("res://scenes/main_menu.tscn")
