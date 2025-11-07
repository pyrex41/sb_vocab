extends Node

# Session Manager - Coordinates activity flow and session state

signal session_started(session_data)
signal activity_changed(activity_data, index, total)
signal attempt_result(correct, feedback)
signal session_ended(summary)
signal loading_started()
signal loading_ended()
signal api_error(message)

var current_session_id: String = ""
var activities: Array = []
var current_activity_index: int = 0
var session_active: bool = false

func start_new_session(grade: String = "grade3"):
	loading_started.emit()

	var response = await PlaycademySdk.backend.request(
		"POST",
		"/session/start",
		{
			"user_id": PlaycademySdk.user_id,
			"grade": grade
		}
	)

	loading_ended.emit()

	if response.has("error") and response.error:
		_handle_api_error(response.error)
		return

	current_session_id = response.session_id
	activities = response.activities
	current_activity_index = 0
	session_active = true

	session_started.emit(response)
	_load_current_activity()

func _load_current_activity():
	if current_activity_index >= activities.size():
		_end_session()
		return
	
	var activity = activities[current_activity_index]
	activity_changed.emit(activity, current_activity_index + 1, activities.size())

func submit_answer(answer: String):
	if not session_active:
		return

	loading_started.emit()

	var response = await PlaycademySdk.backend.request(
		"POST",
		"/session/attempt",
		{
			"session_id": current_session_id,
			"activity_index": current_activity_index,
			"answer": answer
		}
	)

	loading_ended.emit()

	if response.has("error") and response.error:
		_handle_api_error(response.error)
		return

	attempt_result.emit(response.correct, response.feedback)

	# Wait a moment before advancing to next activity
	await get_tree().create_timer(1.5).timeout
	next_activity()

func next_activity():
	if not session_active:
		return
	
	current_activity_index += 1
	_load_current_activity()

func _end_session():
	if not session_active:
		return

	session_active = false
	loading_started.emit()

	var response = await PlaycademySdk.backend.request(
		"POST",
		"/session/end",
		{"session_id": current_session_id}
	)

	loading_ended.emit()

	if response.has("error") and response.error:
		_handle_api_error(response.error)
		return

	session_ended.emit(response)

	current_session_id = ""
	activities = []
	current_activity_index = 0

func is_session_active() -> bool:
	return session_active

func _handle_api_error(error: Dictionary):
	var error_message = "Oops! Something went wrong."

	if error.has("code"):
		match error.code:
			"NETWORK_ERROR":
				error_message = "Can't connect to the server. Check your internet!"
			"TIMEOUT":
				error_message = "The server is taking too long. Try again!"
			"AUTH_ERROR":
				error_message = "Please log in again."
			_:
				error_message = error.message if error.has("message") else error_message
	elif error.has("message"):
		error_message = error.message

	# Emit error signal that UI can handle
	api_error.emit(error_message)

	# Log for debugging
	push_error("API Error: " + str(error))
