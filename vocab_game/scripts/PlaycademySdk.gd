extends Node

# PlaycademySDK - Handles backend API communication
# This is a simplified SDK for connecting to the local backend server

# Configuration
var base_url: String = "http://localhost:8788/api"
var user_id: String = "student.fresh@demo.playcademy.com"  # Dev mode auto-authenticates as this user
var password: String = "password"  # Default password for demo users
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

func _ready():
	backend = Backend.new(self)
	print("PlaycademySDK initialized")
	print("Backend URL: ", base_url)
	print("User ID: ", user_id)

	# Perform login to get session cookie
	await login()

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
	sdk_ready.emit()
	print("SDK Ready!")
	return true
