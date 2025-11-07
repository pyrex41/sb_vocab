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
var last_failed_operation: Dictionary = {}  # Store failed operation for retry
var course_id: String = ""  # Will store the first available course ID

func start_new_session(grade: String = "grade3"):
	# Store operation for retry in case of failure
	last_failed_operation = {"type": "start_session", "grade": grade}

	# Check authentication before making API call
	if not _check_authentication():
		loading_ended.emit()  # Ensure loading state is cleared
		return

	loading_started.emit()

	# First, get available courses if we don't have a course_id yet
	if course_id == "":
		print("Fetching available courses...")
		var courses_response = await PlaycademySdk.backend.request("GET", "/content/course", {})

		if courses_response.has("error"):
			loading_ended.emit()
			_handle_api_error(courses_response.error)
			return

		if courses_response.has("courses") and courses_response.courses.size() > 0:
			course_id = courses_response.courses[0].courseId
			print("Using course ID: ", course_id)
		else:
			loading_ended.emit()
			api_error.emit("No courses available. Please set up courses in the backend.")
			return

	# Now start the session with the course ID
	var response = await PlaycademySdk.backend.request(
		"POST",
		"/session/start",
		{
			"courseId": course_id
		}
	)

	loading_ended.emit()

	# Check for API errors
	if response.has("error") and response.error:
		_handle_api_error(response.error)
		return

	# Validate response structure
	if not _validate_session_response(response):
		api_error.emit("Invalid response from server. Please try again.")
		return

	# Success - clear failed operation and update state
	last_failed_operation.clear()
	current_session_id = response.sessionId
	activities = []  # Will be loaded one at a time via /session/:id/next
	current_activity_index = 0
	session_active = true

	print("Session started! ID: ", current_session_id, " Items: ", response.itemCount)
	session_started.emit(response)

	# Load first activity
	await _load_next_activity()

func _load_next_activity():
	"""Load the next activity from the backend"""
	if not session_active:
		return

	loading_started.emit()

	var response = await PlaycademySdk.backend.request(
		"POST",
		"/session/" + current_session_id + "/next",
		{}
	)

	loading_ended.emit()

	if response.has("error"):
		_handle_api_error(response.error)
		return

	# Check if session is complete (no more items)
	if response.has("completed") and response.completed:
		print("Session complete!")
		_end_session()
		return

	# Emit activity changed with the new activity data
	print("Loaded activity: ", response.activityType)
	activity_changed.emit(response, current_activity_index, -1)  # Total count not available
	current_activity_index += 1

func _load_current_activity():
	# Legacy function - now we use _load_next_activity
	await _load_next_activity()

func submit_answer(answer: String):
	if not session_active:
		return

	# Store operation for retry
	last_failed_operation = {"type": "submit_answer", "answer": answer}

	# Check authentication
	if not _check_authentication():
		loading_ended.emit()
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

	# Check for API errors
	if response.has("error") and response.error:
		_handle_api_error(response.error)
		return

	# Validate response
	if not _validate_attempt_response(response):
		api_error.emit("Invalid response from server. Please try again.")
		return

	# Success - clear failed operation
	last_failed_operation.clear()
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

	# Store operation for retry
	last_failed_operation = {"type": "end_session"}

	session_active = false

	# Check authentication
	if not _check_authentication():
		loading_ended.emit()
		# Still clear session state even if API call fails
		current_session_id = ""
		activities = []
		current_activity_index = 0
		return

	loading_started.emit()

	var response = await PlaycademySdk.backend.request(
		"POST",
		"/session/end",
		{"session_id": current_session_id}
	)

	loading_ended.emit()

	# Check for API errors
	if response.has("error") and response.error:
		_handle_api_error(response.error)
		# Still clear local state and navigate even if API fails
		current_session_id = ""
		activities = []
		current_activity_index = 0
		# Emit a basic session ended signal so UI can transition
		session_ended.emit({"error": true, "total_activities": 0, "correct_count": 0})
		return

	# Success - clear failed operation
	last_failed_operation.clear()
	session_ended.emit(response)

	current_session_id = ""
	activities = []
	current_activity_index = 0

func is_session_active() -> bool:
	return session_active

func get_progress_data() -> Dictionary:
	# Call backend API to get progress
	if not _check_authentication():
		return _get_empty_progress()

	loading_started.emit()

	var response = await PlaycademySdk.backend.request(
		"GET",
		"/progress",
		{"user_id": PlaycademySdk.user_id}
	)

	loading_ended.emit()

	if response.has("error") and response.error:
		_handle_api_error(response.error)
		return _get_empty_progress()

	# Validate response has required fields
	if not _validate_progress_response(response):
		push_error("Invalid progress response format")
		return _get_empty_progress()

	return response

func retry_last_operation():
	"""Retry the last failed operation"""
	if last_failed_operation.is_empty():
		push_error("No failed operation to retry")
		return

	match last_failed_operation.type:
		"start_session":
			start_new_session(last_failed_operation.grade)
		"submit_answer":
			submit_answer(last_failed_operation.answer)
		"end_session":
			_end_session()
		_:
			push_error("Unknown operation type: " + str(last_failed_operation.type))

func _check_authentication() -> bool:
	"""Check if user is authenticated before making API calls"""
	if not PlaycademySdk:
		push_error("PlaycademySdk not available")
		api_error.emit("PlaycademySDK not initialized. Please restart the application.")
		return false

	if not "user_id" in PlaycademySdk or PlaycademySdk.user_id == "":
		push_error("User not authenticated")
		api_error.emit("Please log in to continue.")
		return false

	return true

func _validate_session_response(response: Dictionary) -> bool:
	"""Validate session start response has required fields"""
	if not response.has("sessionId"):
		push_error("Response missing sessionId")
		return false
	if not response.has("itemCount"):
		push_error("Response missing itemCount")
		return false
	return true

func _validate_attempt_response(response: Dictionary) -> bool:
	"""Validate attempt submission response"""
	if not response.has("correct"):
		push_error("Response missing 'correct' field")
		return false
	if not response.has("feedback"):
		push_error("Response missing 'feedback' field")
		return false
	return true

func _validate_progress_response(response: Dictionary) -> bool:
	"""Validate progress response"""
	var required_fields = ["words_learned", "accuracy", "total_attempts"]
	for field in required_fields:
		if not response.has(field):
			push_error("Progress response missing field: " + field)
			return false
	return true

func _get_empty_progress() -> Dictionary:
	"""Return empty progress data for error cases"""
	return {
		"words_learned": 0,
		"accuracy": 0.0,
		"total_attempts": 0,
		"sessions_completed": 0
	}

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
