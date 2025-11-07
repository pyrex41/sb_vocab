extends Node

var backend = preload("res://addons/playcademy_sdk/backend.gd").new()
var session_state = {}
var user_id = "user_demo_001"
var grade_level = 4

func _ready():
	name = "PlaycademySDK"

func start_session(grade: int) -> Dictionary:
	grade_level = grade
	var response = backend.request("POST", "/session/start", {
		"user_id": user_id,
		"grade": grade
	})
	session_state = response
	return response

func attempt_activity(activity_type: String, user_answer: String, word_id: String) -> Dictionary:
	var response = backend.request("POST", "/session/attempt", {
		"session_id": session_state.get("session_id", ""),
		"activity_type": activity_type,
		"user_answer": user_answer,
		"word_id": word_id
	})
	return response

func end_session() -> Dictionary:
	var response = backend.request("POST", "/session/end", {
		"session_id": session_state.get("session_id", "")
	})
	return response

func get_progress() -> Dictionary:
	var response = backend.request("GET", "/progress", {
		"user_id": user_id
	})
	return response
