extends Node

# Session Manager - Coordinates activity flow and session state

signal session_started(session_data)
signal activity_changed(activity_data, index, total)
signal attempt_result(correct, feedback)
signal session_ended(summary)

var current_session_id: String = ""
var activities: Array = []
var current_activity_index: int = 0
var session_active: bool = false

func start_new_session(grade: String = "grade3"):
	var response = MockBackend.start_session(grade)
	
	if response.success:
		current_session_id = response.session_id
		activities = response.activities
		current_activity_index = 0
		session_active = true
		
		session_started.emit(response)
		_load_current_activity()
	else:
		push_error("Failed to start session")

func _load_current_activity():
	if current_activity_index >= activities.size():
		_end_session()
		return
	
	var activity = activities[current_activity_index]
	activity_changed.emit(activity, current_activity_index + 1, activities.size())

func submit_answer(answer: String):
	if not session_active:
		return
	
	var response = MockBackend.submit_attempt(
		current_session_id,
		current_activity_index,
		answer
	)
	
	if response.success:
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
	var response = MockBackend.end_session(current_session_id)
	
	if response.success:
		session_ended.emit(response.session_summary)
	
	current_session_id = ""
	activities = []
	current_activity_index = 0

func get_progress_data() -> Dictionary:
	return MockBackend.get_progress()

func is_session_active() -> bool:
	return session_active
