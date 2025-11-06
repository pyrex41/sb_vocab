extends Node

# Mock Backend Service - Simulates REST API responses

var vocabulary_bank = {
	"grade3": [
		{
			"id": "w001",
			"word": "abundant",
			"definition": "existing in large quantities; plentiful",
			"example": "The forest has abundant wildlife.",
			"synonyms": ["plentiful", "ample", "copious"],
			"antonyms": ["scarce", "rare", "limited"],
			"audio_url": "res://audio/abundant.wav"
		},
		{
			"id": "w002",
			"word": "curious",
			"definition": "eager to learn or know something",
			"example": "The curious cat explored every corner.",
			"synonyms": ["inquisitive", "interested", "eager"],
			"antonyms": ["indifferent", "uninterested", "apathetic"],
			"audio_url": "res://audio/curious.wav"
		},
		{
			"id": "w003",
			"word": "delicate",
			"definition": "fragile and easily broken or damaged",
			"example": "Handle the delicate glass vase with care.",
			"synonyms": ["fragile", "dainty", "fine"],
			"antonyms": ["sturdy", "strong", "robust"],
			"audio_url": "res://audio/delicate.wav"
		},
		{
			"id": "w004",
			"word": "enormous",
			"definition": "very large in size or quantity",
			"example": "The elephant is an enormous animal.",
			"synonyms": ["huge", "massive", "gigantic"],
			"antonyms": ["tiny", "small", "miniature"],
			"audio_url": "res://audio/enormous.wav"
		},
		{
			"id": "w005",
			"word": "fierce",
			"definition": "having a violent or aggressive nature",
			"example": "The fierce lion protected its cubs.",
			"synonyms": ["aggressive", "wild", "savage"],
			"antonyms": ["gentle", "tame", "mild"],
			"audio_url": "res://audio/fierce.wav"
		},
		{
			"id": "w006",
			"word": "graceful",
			"definition": "moving in a smooth and attractive way",
			"example": "The ballet dancer was graceful on stage.",
			"synonyms": ["elegant", "fluid", "smooth"],
			"antonyms": ["clumsy", "awkward", "ungainly"],
			"audio_url": "res://audio/graceful.wav"
		},
		{
			"id": "w007",
			"word": "humble",
			"definition": "not proud or arrogant; modest",
			"example": "Despite winning, she remained humble.",
			"synonyms": ["modest", "unassuming", "meek"],
			"antonyms": ["arrogant", "proud", "conceited"],
			"audio_url": "res://audio/humble.wav"
		},
		{
			"id": "w008",
			"word": "swift",
			"definition": "moving or capable of moving at high speed",
			"example": "The swift cheetah caught its prey.",
			"synonyms": ["fast", "quick", "rapid"],
			"antonyms": ["slow", "sluggish", "leisurely"],
			"audio_url": "res://audio/swift.wav"
		},
		{
			"id": "w009",
			"word": "tranquil",
			"definition": "free from disturbance; calm and peaceful",
			"example": "The lake was tranquil at sunset.",
			"synonyms": ["peaceful", "calm", "serene"],
			"antonyms": ["chaotic", "turbulent", "agitated"],
			"audio_url": "res://audio/tranquil.wav"
		},
		{
			"id": "w010",
			"word": "valiant",
			"definition": "showing courage and determination",
			"example": "The valiant knight defended the castle.",
			"synonyms": ["brave", "courageous", "heroic"],
			"antonyms": ["cowardly", "timid", "fearful"],
			"audio_url": "res://audio/valiant.wav"
		}
	]
}

var user_progress = {
	"words_learned": 0,
	"total_attempts": 0,
	"correct_attempts": 0,
	"mastery_levels": {}
}

var current_session_data = null

func _ready():
	print("Mock Backend initialized")

# Mock POST /session/start
func start_session(grade: String = "grade3") -> Dictionary:
	var words = vocabulary_bank.get(grade, vocabulary_bank["grade3"])
	var session_words = []
	
	# Select 5-8 words for the session
	var num_words = min(randi() % 4 + 5, words.size())
	var selected_indices = []
	
	while selected_indices.size() < num_words:
		var idx = randi() % words.size()
		if idx not in selected_indices:
			selected_indices.append(idx)
	
	# Build activity queue
	var activities = []
	for idx in selected_indices:
		var word_data = words[idx]
		session_words.append(word_data)
		
		# Each word gets 2-3 activities
		activities.append({
			"type": "flashcard",
			"word": word_data
		})
		activities.append({
			"type": "multiple_choice",
			"word": word_data,
			"options": _generate_mc_options(word_data, words)
		})
		
		# Randomly add additional activities
		var extra_activities = ["spelling", "fill_blank", "synonym_antonym"]
		var extra = extra_activities[randi() % extra_activities.size()]
		activities.append({
			"type": extra,
			"word": word_data
		})
	
	# Shuffle activities
	activities.shuffle()
	
	current_session_data = {
		"session_id": "session_" + str(Time.get_unix_time_from_system()),
		"grade": grade,
		"activities": activities,
		"start_time": Time.get_unix_time_from_system()
	}
	
	return {
		"success": true,
		"session_id": current_session_data.session_id,
		"activities": activities,
		"total_activities": activities.size()
	}

# Mock POST /session/attempt
func submit_attempt(session_id: String, activity_index: int, answer: String) -> Dictionary:
	if not current_session_data or current_session_data.session_id != session_id:
		return {"success": false, "error": "Invalid session"}
	
	var activity = current_session_data.activities[activity_index]
	var correct = _check_answer(activity, answer)
	
	user_progress.total_attempts += 1
	if correct:
		user_progress.correct_attempts += 1
	
	return {
		"success": true,
		"correct": correct,
		"correct_answer": _get_correct_answer(activity),
		"feedback": _get_feedback(correct, activity)
	}

# Mock POST /session/end
func end_session(session_id: String) -> Dictionary:
	if not current_session_data or current_session_data.session_id != session_id:
		return {"success": false, "error": "Invalid session"}
	
	var duration = Time.get_unix_time_from_system() - current_session_data.start_time
	var activities_completed = current_session_data.activities.size()
	
	# Update mastery levels
	for activity in current_session_data.activities:
		var word_id = activity.word.id
		if word_id not in user_progress.mastery_levels:
			user_progress.mastery_levels[word_id] = 0
		user_progress.mastery_levels[word_id] += 1
	
	user_progress.words_learned = user_progress.mastery_levels.size()
	
	var result = {
		"success": true,
		"session_summary": {
			"duration_seconds": duration,
			"activities_completed": activities_completed,
			"words_practiced": len(user_progress.mastery_levels)
		},
		"progress": get_progress()
	}
	
	current_session_data = null
	return result

# Mock GET /progress
func get_progress() -> Dictionary:
	var accuracy = 0.0
	if user_progress.total_attempts > 0:
		accuracy = float(user_progress.correct_attempts) / float(user_progress.total_attempts) * 100.0
	
	return {
		"success": true,
		"words_learned": user_progress.words_learned,
		"total_attempts": user_progress.total_attempts,
		"accuracy": accuracy,
		"mastery_levels": user_progress.mastery_levels
	}

# Helper functions
func _generate_mc_options(correct_word: Dictionary, all_words: Array) -> Array:
	var options = [correct_word.definition]
	var used_indices = []
	
	# Add 3 wrong answers
	while options.size() < 4:
		var idx = randi() % all_words.size()
		if all_words[idx].id != correct_word.id and idx not in used_indices:
			options.append(all_words[idx].definition)
			used_indices.append(idx)
	
	options.shuffle()
	return options

func _check_answer(activity: Dictionary, answer: String) -> bool:
	match activity.type:
		"multiple_choice":
			return answer == activity.word.definition
		"spelling":
			return answer.to_lower() == activity.word.word.to_lower()
		"fill_blank":
			return answer.to_lower() == activity.word.word.to_lower()
		"synonym_antonym":
			# Check if answer is in synonyms or antonyms
			return answer in activity.word.synonyms or answer in activity.word.antonyms
		_:
			return true  # Flashcards are always "correct"

func _get_correct_answer(activity: Dictionary) -> String:
	match activity.type:
		"multiple_choice":
			return activity.word.definition
		"spelling", "fill_blank":
			return activity.word.word
		"synonym_antonym":
			if activity.word.synonyms.size() > 0:
				return activity.word.synonyms[0]
			return ""
		_:
			return ""

func _get_feedback(correct: bool, activity: Dictionary) -> String:
	if correct:
		var positive_feedback = [
			"Excellent work!",
			"Great job!",
			"Perfect!",
			"You got it!",
			"Wonderful!",
			"Outstanding!",
			"Fantastic!"
		]
		return positive_feedback[randi() % positive_feedback.size()]
	else:
		return "Not quite. The correct answer is: " + _get_correct_answer(activity)
