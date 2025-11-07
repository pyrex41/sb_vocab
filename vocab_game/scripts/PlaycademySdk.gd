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

	print("PlaycademySDK initialized")
	print("Backend URL: ", base_url)
	print("User ID: ", user_id)

	# Load persisted session cookie if available
	_load_session_cookie()

	# Perform login to get session cookie if auto-login is enabled
	if Config.should_auto_login():
		await login()
	else:
		is_ready = true
		sdk_ready.emit()
		print("SDK Ready (manual login required)")

func login() -> bool:
	print("Logging in as: ", user_id)

	var response = await backend.request("POST", "/auth/sign-in/email", {
		"email": user_id,
		"password": password
	})

	if response.has("error"):
		push_error("Login failed: ", str(response.error))
		return false

	print("Login successful!")
	is_ready = true

	# Persist session cookie to file
	_save_session_cookie()

	sdk_ready.emit()
	print("SDK Ready!")
	return true

func _load_session_cookie():
	"""Load persisted session cookie from file storage"""
	if not FileAccess.file_exists(SESSION_FILE_PATH):
		return

	var file = FileAccess.open(SESSION_FILE_PATH, FileAccess.READ)
	if not file:
		push_warning("Failed to load session cookie")
		return

	session_cookie = file.get_line()
	file.close()

	if session_cookie != "":
		print("Loaded persisted session cookie")

func _save_session_cookie():
	"""Save session cookie to file storage for persistence"""
	if session_cookie == "":
		return

	var file = FileAccess.open(SESSION_FILE_PATH, FileAccess.WRITE)
	if not file:
		push_warning("Failed to save session cookie")
		return

	file.store_line(session_cookie)
	file.close()

	if Config.is_debug_mode():
		print("Saved session cookie to file")

func clear_session():
	"""Clear current session and delete persisted cookie"""
	session_cookie = ""
	is_ready = false

	# Delete session file
	if FileAccess.file_exists(SESSION_FILE_PATH):
		DirAccess.remove_absolute(SESSION_FILE_PATH)
		print("Cleared session cookie")
