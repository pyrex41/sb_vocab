extends Node

# PlaycademySDK - Handles backend API communication
# This is a simplified SDK for connecting to the local backend server

# Configuration (loaded from Config autoload)
var base_url: String = ""
var user_id: String = ""
# Password no longer stored - must be provided at login
var is_ready: bool = false
var session_cookie: String = ""  # Stores the authentication cookie
var session_expires_at: int = 0  # Unix timestamp when session expires
signal sdk_ready()

# Session and security settings
const SESSION_DURATION = 3600  # 1 hour in seconds
const SESSION_MAX_AGE = 86400  # 24 hours max session age

# Rate limiting for authentication
var _login_attempts: int = 0
var _last_login_attempt: int = 0
const MAX_LOGIN_ATTEMPTS = 5
const LOCKOUT_DURATION = 300  # 5 minutes in seconds

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

func _ready():
	# Validate AutoLoad dependencies
	assert(Config != null, "CRITICAL: PlaycademySdk depends on Config AutoLoad! Check project.godot AutoLoad order")
	assert(Logger != null, "CRITICAL: PlaycademySdk depends on Logger AutoLoad! Check project.godot AutoLoad order")

	backend = Backend.new(self)

	# Load configuration from Config autoload
	base_url = Config.get_backend_url()
	user_id = Config.get_user_id()

	Logger.info("PlaycademySDK initialized", "PlaycademySdk")
	Logger.info("Backend URL: " + base_url, "PlaycademySdk")
	Logger.debug("User ID: " + user_id, "PlaycademySdk")

	# No session persistence - always require login
	# Note: auto_login now requires password to be provided via login() parameter
	# For development, you can hardcode it temporarily or create a login UI
	if Config.should_auto_login():
		# For now, use a default password for auto-login (INSECURE - replace with UI)
		await login("password")
	else:
		is_ready = true
		sdk_ready.emit()
		Logger.info("SDK Ready (manual login required)", "PlaycademySdk")

func login(user_password: String = "") -> bool:
	"""Login with email and password. Password must be provided as parameter (not stored in config)."""
	if user_password == "":
		Logger.error("Password required for login (not stored in config)", "PlaycademySdk")
		return false

	# Check rate limiting
	if _login_attempts >= MAX_LOGIN_ATTEMPTS:
		var time_since_last = Time.get_unix_time_from_system() - _last_login_attempt
		if time_since_last < LOCKOUT_DURATION:
			var remaining = LOCKOUT_DURATION - time_since_last
			Logger.error("Too many login attempts. Try again in " + str(remaining) + " seconds", "PlaycademySdk")
			return false
		else:
			# Lockout period expired, reset attempts
			_login_attempts = 0

	_login_attempts += 1
	_last_login_attempt = Time.get_unix_time_from_system()

	Logger.info("Attempting login as: " + user_id + " (attempt " + str(_login_attempts) + "/" + str(MAX_LOGIN_ATTEMPTS) + ")", "PlaycademySdk")

	var response = await backend.request("POST", "/auth/sign-in/email", {
		"email": user_id,
		"password": user_password  # Use provided password parameter
	})

	if response.has("error"):
		Logger.error("Login failed: " + str(response.error), "PlaycademySdk")
		return false

	Logger.info("Login successful!", "PlaycademySdk")
	is_ready = true

	# Reset login attempts on success
	_login_attempts = 0

	# Set session expiration (in-memory only, no persistence)
	session_expires_at = Time.get_unix_time_from_system() + SESSION_DURATION
	Logger.debug("Session expires at: " + Time.get_datetime_string_from_unix_time(session_expires_at), "PlaycademySdk")
	Logger.info("Session active for this game session only (not persisted)", "PlaycademySdk")

	sdk_ready.emit()
	Logger.info("SDK Ready!", "PlaycademySdk")
	return true

# Session persistence removed for security - sessions only last for the current game session

func _is_session_expired() -> bool:
	"""Check if current session has expired"""
	if session_expires_at == 0:
		return true  # No session or not set
	return Time.get_unix_time_from_system() > session_expires_at

# Session save/load and XOR obfuscation functions removed for security

func clear_session():
	"""Clear current session (in-memory only)"""
	session_cookie = ""
	session_expires_at = 0
	is_ready = false
	Logger.info("Session cleared", "PlaycademySdk")
