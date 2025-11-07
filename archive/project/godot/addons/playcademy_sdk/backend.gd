class_name PlaycademyBackend

var mock_words = [
	{"id": "word_1", "word": "Ephemeral", "definition": "Lasting for a very short time", "example": "The beauty of cherry blossoms is ephemeral, lasting only a few weeks."},
	{"id": "word_2", "word": "Persevere", "definition": "To continue despite difficulty", "example": "If you persevere through the challenges, you will succeed."},
	{"id": "word_3", "word": "Benevolent", "definition": "Kind and generous", "example": "The benevolent teacher helped students who struggled."},
	{"id": "word_4", "word": "Diligent", "definition": "Showing care and effort in work", "example": "Her diligent study habits earned her excellent grades."},
	{"id": "word_5", "word": "Meticulous", "definition": "Very careful and precise", "example": "The artist was meticulous in every detail of the painting."}
]

var session_counter = 0
var attempt_counter = 0

func request(method: String, endpoint: String, params: Dictionary) -> Dictionary:
	match endpoint:
		"/session/start":
			return _session_start(params)
		"/session/attempt":
			return _session_attempt(params)
		"/session/end":
			return _session_end(params)
		"/progress":
			return _get_progress(params)
		_:
			return {"error": "Unknown endpoint"}

func _session_start(params: Dictionary) -> Dictionary:
	session_counter += 1
	var activities = []

	for word in mock_words:
		activities.append({
			"word_id": word.id,
			"word": word.word,
			"activity_type": "flashcard"
		})
		activities.append({
			"word_id": word.id,
			"word": word.word,
			"activity_type": "multiple_choice"
		})

	return {
		"session_id": "session_%d" % session_counter,
		"user_id": params.get("user_id"),
		"grade": params.get("grade"),
		"activities": activities,
		"total_activities": activities.size()
	}

func _session_attempt(params: Dictionary) -> Dictionary:
	attempt_counter += 1

	var user_answer = params.get("user_answer", "")
	var word_id = params.get("word_id", "")
	var activity_type = params.get("activity_type", "")

	var score = randi_range(60, 100)
	var is_correct = score >= 80

	return {
		"attempt_id": "attempt_%d" % attempt_counter,
		"score": score,
		"is_correct": is_correct,
		"feedback": "Great job!" if is_correct else "Try again!",
		"word_id": word_id,
		"activity_type": activity_type
	}

func _session_end(params: Dictionary) -> Dictionary:
	return {
		"session_id": params.get("session_id"),
		"status": "completed",
		"total_score": 85,
		"words_mastered": 3,
		"words_learning": 2
	}

func _get_progress(params: Dictionary) -> Dictionary:
	return {
		"user_id": params.get("user_id"),
		"total_sessions": 5,
		"total_words_learned": 12,
		"current_streak": 3,
		"mastery_level": 0.75
	}
