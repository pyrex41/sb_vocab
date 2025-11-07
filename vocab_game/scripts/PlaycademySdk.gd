extends Node

# PlaycademySDK - Handles backend API communication
# This is a simplified SDK for connecting to the local backend server

# Configuration (loaded from Config autoload)
var base_url: String = ""
var user_id: String = ""
var password: String = ""
var is_ready: bool = false
var session_cookie: String = ""  # Stores the authentication cookie
signal sdk_ready()

# Backend request handler
class Backend:
	var sdk_ref: Node

	func _init(sdk: Node):
		sdk_ref = sdk

	func request(method: String, endpoint: String, body: Dictionary = {}) -> Dictionary:
		var url = sdk_ref.base_url + endpoint
		var http = HTTPRequest.new()
		sdk_ref.add_child(http)

		var headers = ["Content-Type: application/json"]

		# Add session cookie if we have one
		if sdk_ref.session_cookie != "":
			headers.append("Cookie: " + sdk_ref.session_cookie)

		var json_body = JSON.stringify(body) if body.size() > 0 else ""

		# Map method strings to HTTPClient constants
		var http_method = HTTPClient.METHOD_GET
		match method.to_upper():
			"GET":
				http_method = HTTPClient.METHOD_GET
			"POST":
				http_method = HTTPClient.METHOD_POST
			"PUT":
				http_method = HTTPClient.METHOD_PUT
			"DELETE":
				http_method = HTTPClient.METHOD_DELETE

		print("API Request: ", method, " ", url)
		if json_body:
			print("Request Body: ", json_body)

		var error = http.request(url, headers, http_method, json_body)

		if error != OK:
			print("HTTP Request failed with error: ", error)
			sdk_ref.remove_child(http)
			http.queue_free()
			return {
				"error": {
					"code": "NETWORK_ERROR",
					"message": "Failed to connect to server. Is the backend running?"
				}
			}

		# Wait for request to complete
		var result = await http.request_completed
		var response_code = result[1]
		var response_headers = result[2]
		var response_body = result[3]

		# Extract and store session cookie from Set-Cookie header
		for header in response_headers:
			if header.begins_with("Set-Cookie:") or header.begins_with("set-cookie:"):
				var cookie_value = header.substr(header.find(":") + 1).strip_edges()
				# Extract just the cookie name=value before the semicolon
				var cookie_parts = cookie_value.split(";")
				if cookie_parts.size() > 0:
					sdk_ref.session_cookie = cookie_parts[0]
					print("Stored session cookie: ", sdk_ref.session_cookie)

		# Clean up HTTP request node
		sdk_ref.remove_child(http)
		http.queue_free()

		# Parse response
		var json = JSON.new()
		var parse_result = json.parse(response_body.get_string_from_utf8())

		if parse_result != OK:
			print("Failed to parse JSON response")
			return {
				"error": {
					"code": "PARSE_ERROR",
					"message": "Invalid response from server"
				}
			}

		var response_data = json.data

		# Check HTTP status code
		if response_code >= 400:
			print("API Error (", response_code, "): ", response_data)
			if response_data is Dictionary and response_data.has("error"):
				return {"error": response_data.error}
			else:
				return {
					"error": {
						"code": "HTTP_ERROR",
						"message": "Server returned error: " + str(response_code)
					}
				}

		print("API Response: ", response_data)
		return response_data

var backend: Backend

# Session persistence
const SESSION_FILE_PATH = "user://session.dat"

func _ready():
	backend = Backend.new(self)

	# Load configuration from Config autoload
	base_url = Config.get_backend_url()
	user_id = Config.get_user_id()
	password = Config.get_password()

	Logger.info("PlaycademySDK initialized", "PlaycademySdk")
	Logger.info("Backend URL: " + base_url, "PlaycademySdk")
	Logger.debug("User ID: " + user_id, "PlaycademySdk")

	# Try to load and validate persisted session
	var session_loaded = await _load_and_validate_session()

	# If session validation failed or no session, perform login if auto-login enabled
	if not session_loaded and Config.should_auto_login():
		await login()
	elif session_loaded:
		is_ready = true
		sdk_ready.emit()
		Logger.info("SDK Ready (using persisted session)", "PlaycademySdk")
	else:
		is_ready = true
		sdk_ready.emit()
		Logger.info("SDK Ready (manual login required)", "PlaycademySdk")

func login() -> bool:
	Logger.info("Attempting login as: " + user_id, "PlaycademySdk")

	var response = await backend.request("POST", "/auth/sign-in/email", {
		"email": user_id,
		"password": password
	})

	if response.has("error"):
		Logger.error("Login failed: " + str(response.error), "PlaycademySdk")
		return false

	Logger.info("Login successful!", "PlaycademySdk")
	is_ready = true

	# Persist session cookie to file (with error handling)
	if not _save_session_cookie():
		Logger.warn("Failed to persist session cookie - will need to login again on restart", "PlaycademySdk")

	sdk_ready.emit()
	Logger.info("SDK Ready!", "PlaycademySdk")
	return true

func _load_and_validate_session() -> bool:
	"""Load persisted session and validate it's still active"""
	if not FileAccess.file_exists(SESSION_FILE_PATH):
		return false

	var file = FileAccess.open(SESSION_FILE_PATH, FileAccess.READ)
	if not file:
		Logger.warn("Failed to open session file", "PlaycademySdk")
		return false

	# Read and decode session cookie
	var encoded_cookie = file.get_line()
	file.close()

	if encoded_cookie == "":
		return false

	# Simple XOR obfuscation (not encryption, but better than plaintext)
	session_cookie = _decode_session(encoded_cookie)

	if session_cookie == "":
		Logger.warn("Failed to decode session cookie", "PlaycademySdk")
		return false

	Logger.debug("Loaded persisted session cookie", "PlaycademySdk")

	# Validate session by making a test request
	var test_response = await backend.request("GET", "/auth/session", {})

	if test_response.has("error"):
		Logger.warn("Persisted session is invalid or expired, clearing it", "PlaycademySdk")
		clear_session()
		return false

	Logger.info("Persisted session validated successfully", "PlaycademySdk")
	return true

func _save_session_cookie() -> bool:
	"""Save session cookie to file storage with basic obfuscation"""
	if session_cookie == "":
		Logger.warn("Attempted to save empty session cookie", "PlaycademySdk")
		return false

	var file = FileAccess.open(SESSION_FILE_PATH, FileAccess.WRITE)
	if not file:
		Logger.error("Failed to open session file for writing", "PlaycademySdk")
		return false

	# Simple XOR obfuscation (better than plaintext, though not true encryption)
	var encoded_cookie = _encode_session(session_cookie)
	file.store_line(encoded_cookie)
	file.close()

	Logger.debug("Saved session cookie to file", "PlaycademySdk")
	return true

func _encode_session(text: String) -> String:
	"""Simple XOR obfuscation (not secure encryption, just obfuscation)"""
	var key = "PlaycademyVocabGame2025"  # Simple XOR key
	var result = PackedByteArray()
	var text_bytes = text.to_utf8_buffer()

	for i in range(text_bytes.size()):
		result.append(text_bytes[i] ^ key.unicode_at(i % key.length()))

	return Marshalls.raw_to_base64(result)

func _decode_session(encoded: String) -> String:
	"""Decode XOR obfuscated session"""
	var key = "PlaycademyVocabGame2025"
	var decoded_bytes = Marshalls.base64_to_raw(encoded)

	if decoded_bytes.size() == 0:
		return ""

	var result = PackedByteArray()
	for i in range(decoded_bytes.size()):
		result.append(decoded_bytes[i] ^ key.unicode_at(i % key.length()))

	return result.get_string_from_utf8()

func clear_session():
	"""Clear current session and delete persisted cookie"""
	session_cookie = ""
	is_ready = false

	# Delete session file
	if FileAccess.file_exists(SESSION_FILE_PATH):
		DirAccess.remove_absolute(SESSION_FILE_PATH)
		Logger.info("Cleared session cookie and file", "PlaycademySdk")
	else:
		Logger.debug("No session file to clear", "PlaycademySdk")
