class_name ErrorMessages

## ErrorMessages - Kid-friendly error message system
##
## Provides consistent, encouraging error messages for various error types
## that can occur during gameplay. Designed to be non-technical and supportive
## for elementary school students.
##
## Usage:
##   var error_data = ErrorMessages.get_message("network_timeout")
##   print(error_data.title)  # "Slow Connection"
##   print(error_data.message)  # Kid-friendly explanation
##   print(error_data.action)  # "Retry"
##   print(error_data.icon)  # "⏰"

## Error type constants
const ERROR_NETWORK_TIMEOUT = "network_timeout"
const ERROR_NETWORK_OFFLINE = "network_offline"
const ERROR_AUTH_FAILED = "auth_failed"
const ERROR_SESSION_EXPIRED = "session_expired"
const ERROR_INVALID_DATA = "invalid_data"
const ERROR_SERVER_ERROR = "server_error"
const ERROR_UNKNOWN = "unknown"

## Get error message data for a specific error type
## Returns a Dictionary with keys: title, message, action, icon
static func get_message(error_type: String) -> Dictionary:
	var messages = {
		ERROR_NETWORK_TIMEOUT: {
			"title": "Slow Connection",
			"message": "The internet is taking too long to respond. Let's try again!",
			"action": "Retry",
			"icon": "⏰",
			"can_skip": false
		},
		ERROR_NETWORK_OFFLINE: {
			"title": "No Internet",
			"message": "Looks like you're not connected to the internet. Please check your connection and try again.",
			"action": "Retry",
			"icon": "📡",
			"can_skip": false
		},
		ERROR_AUTH_FAILED: {
			"title": "Login Problem",
			"message": "We couldn't verify who you are. Please log in again to continue learning!",
			"action": "Go to Login",
			"icon": "🔐",
			"can_skip": false
		},
		ERROR_SESSION_EXPIRED: {
			"title": "Session Ended",
			"message": "Your learning session has expired. No worries - let's start a fresh one!",
			"action": "Start New Session",
			"icon": "⏱️",
			"can_skip": false
		},
		ERROR_INVALID_DATA: {
			"title": "Oops! Data Error",
			"message": "Something didn't load correctly. Let's skip this activity and move on to the next one!",
			"action": "Skip Activity",
			"icon": "⚠️",
			"can_skip": true
		},
		ERROR_SERVER_ERROR: {
			"title": "Server Busy",
			"message": "The server is having trouble right now. Let's wait a moment and try again.",
			"action": "Retry",
			"icon": "🔧",
			"can_skip": false
		},
		ERROR_UNKNOWN: {
			"title": "Something Went Wrong",
			"message": "We're not sure what happened, but let's try again! If this keeps happening, ask a teacher for help.",
			"action": "Retry",
			"icon": "❓",
			"can_skip": false
		}
	}

	return messages.get(error_type, messages[ERROR_UNKNOWN])

## Get a simplified error message for display in notifications
static func get_simple_message(error_type: String) -> String:
	var data = get_message(error_type)
	return "%s %s" % [data.icon, data.title]

## Check if an error type can be skipped/bypassed
static func can_skip(error_type: String) -> bool:
	var data = get_message(error_type)
	return data.get("can_skip", false)

## Get the appropriate action button text for an error type
static func get_action_text(error_type: String) -> String:
	var data = get_message(error_type)
	return data.action

## Map a backend error response to an error type
## Attempts to classify errors based on common error patterns
static func classify_error(error: Variant) -> String:
	# Handle string errors
	if typeof(error) == TYPE_STRING:
		var error_lower = error.to_lower()

		if error_lower.contains("timeout"):
			return ERROR_NETWORK_TIMEOUT
		elif error_lower.contains("network") or error_lower.contains("connection"):
			return ERROR_NETWORK_OFFLINE
		elif error_lower.contains("auth") or error_lower.contains("unauthorized"):
			return ERROR_AUTH_FAILED
		elif error_lower.contains("session") or error_lower.contains("expired"):
			return ERROR_SESSION_EXPIRED
		elif error_lower.contains("invalid") or error_lower.contains("malformed"):
			return ERROR_INVALID_DATA
		elif error_lower.contains("server") or error_lower.contains("500"):
			return ERROR_SERVER_ERROR

	# Handle dictionary errors with error codes
	elif typeof(error) == TYPE_DICTIONARY:
		if error.has("code"):
			match error.code:
				"NETWORK_ERROR", "ERR_INTERNET_DISCONNECTED":
					return ERROR_NETWORK_OFFLINE
				"TIMEOUT", "ERR_TIMEOUT":
					return ERROR_NETWORK_TIMEOUT
				"AUTH_ERROR", "UNAUTHORIZED", "ERR_UNAUTHORIZED":
					return ERROR_AUTH_FAILED
				"SESSION_EXPIRED", "ERR_SESSION_EXPIRED":
					return ERROR_SESSION_EXPIRED
				"INVALID_DATA", "ERR_PARSE_ERROR":
					return ERROR_INVALID_DATA
				"SERVER_ERROR", "INTERNAL_ERROR":
					return ERROR_SERVER_ERROR

	# Default to unknown error
	return ERROR_UNKNOWN

## Get all available error types
static func get_all_error_types() -> Array[String]:
	return [
		ERROR_NETWORK_TIMEOUT,
		ERROR_NETWORK_OFFLINE,
		ERROR_AUTH_FAILED,
		ERROR_SESSION_EXPIRED,
		ERROR_INVALID_DATA,
		ERROR_SERVER_ERROR,
		ERROR_UNKNOWN
	]
