extends Node

## Configuration Manager
## Loads configuration from config.json file or uses default values
## This allows different environments (dev, staging, production) to have different settings

# Default configuration values
var config = {
	"backend_url": "https://localhost:8788/api",  # Changed to HTTPS for security
	"user_id": "student.fresh@demo.playcademy.com",
	# Password removed - must be provided at login for security
	"auto_login": true,
	"debug_mode": true,
	"enable_audio": true,
	"log_level": "INFO"  # DEBUG, INFO, WARN, ERROR
}

# Valid log levels
const VALID_LOG_LEVELS = ["DEBUG", "INFO", "WARN", "WARNING", "ERROR", "NONE"]

# Config schema for validation
const CONFIG_SCHEMA = {
	"backend_url": TYPE_STRING,
	"user_id": TYPE_STRING,
	# "password" removed - no longer stored in config
	"auto_login": TYPE_BOOL,
	"debug_mode": TYPE_BOOL,
	"enable_audio": TYPE_BOOL,
	"log_level": TYPE_STRING
}

# Config file path
const CONFIG_FILE_PATH = "res://config.json"
const USER_CONFIG_PATH = "user://config.json"

func _ready():
	load_config()

func load_config():
	"""Load configuration from file, falling back to defaults"""
	var loaded = false

	# Try to load from user:// directory first (writable, persists)
	if FileAccess.file_exists(USER_CONFIG_PATH):
		loaded = _load_from_file(USER_CONFIG_PATH)
		if loaded:
			push_warning("Config: Loaded from user directory")  # Use push_warning since Logger not ready yet

	# Fall back to res:// directory (read-only, bundled with game)
	if not loaded and FileAccess.file_exists(CONFIG_FILE_PATH):
		loaded = _load_from_file(CONFIG_FILE_PATH)
		if loaded:
			push_warning("Config: Loaded from resource directory")  # Use push_warning since Logger not ready yet

	if not loaded:
		push_warning("Config: Using default values (no config file found)")  # Use push_warning since Logger not ready yet

	# Log configuration (hide sensitive data in production)
	# Note: Can't use Logger here as it depends on Config being ready first
	if config.debug_mode:
		push_warning("Config: Backend URL: " + config.backend_url)
		push_warning("Config: User ID: " + config.user_id)
		push_warning("Config: Auto Login: " + str(config.auto_login))
		push_warning("Config: Debug Mode: " + str(config.debug_mode))

func _load_from_file(file_path: String) -> bool:
	"""Load configuration from a JSON file"""
	var file = FileAccess.open(file_path, FileAccess.READ)
	if not file:
		push_warning("Failed to open config file: " + file_path)
		return false

	var json_text = file.get_as_text()
	file.close()

	var json = JSON.new()
	var parse_result = json.parse(json_text)

	if parse_result != OK:
		push_error("Failed to parse config file: " + file_path + " at line " + str(json.get_error_line()))
		return false

	var loaded_config = json.data
	if typeof(loaded_config) != TYPE_DICTIONARY:
		push_error("Config file must contain a JSON object")
		return false

	# Merge loaded config with defaults (loaded values override defaults)
	for key in loaded_config:
		config[key] = loaded_config[key]

	# Validate schema and critical configuration values
	if not _validate_schema():
		push_error("Config validation failed - using defaults for invalid values")

	_validate_config()

	return true

func _validate_schema() -> bool:
	"""Validate that all required config keys exist and have correct types"""
	var is_valid = true

	for key in CONFIG_SCHEMA:
		if not config.has(key):
			push_warning("Missing required config key: '" + key + "', using default")
			is_valid = false
			continue

		var expected_type = CONFIG_SCHEMA[key]
		var actual_type = typeof(config[key])

		if actual_type != expected_type:
			push_warning("Invalid type for config key '" + key + "': expected " + _type_to_string(expected_type) + ", got " + _type_to_string(actual_type))
			is_valid = false

	return is_valid

func _type_to_string(type: int) -> String:
	"""Convert Variant type constant to string"""
	match type:
		TYPE_BOOL: return "bool"
		TYPE_STRING: return "string"
		TYPE_INT: return "int"
		TYPE_FLOAT: return "float"
		TYPE_DICTIONARY: return "dictionary"
		TYPE_ARRAY: return "array"
		_: return "unknown"

func _validate_config():
	"""Validate configuration values and fix or warn about issues"""
	# Validate backend URL with regex (HTTPS only)
	if not _is_valid_url(config.backend_url):
		push_warning("Invalid backend_url format (must be HTTPS): '" + config.backend_url + "', using default")
		config.backend_url = "https://localhost:8788/api"

	# Validate log level
	if not config.log_level.to_upper() in VALID_LOG_LEVELS:
		push_warning("Invalid log_level: '" + config.log_level + "', defaulting to INFO")
		config.log_level = "INFO"

	# Validate boolean values
	if typeof(config.auto_login) != TYPE_BOOL:
		push_warning("auto_login should be a boolean, converting")
		config.auto_login = bool(config.auto_login)

	if typeof(config.debug_mode) != TYPE_BOOL:
		push_warning("debug_mode should be a boolean, converting")
		config.debug_mode = bool(config.debug_mode)

	if typeof(config.enable_audio) != TYPE_BOOL:
		push_warning("enable_audio should be a boolean, converting")
		config.enable_audio = bool(config.enable_audio)

func _is_valid_url(url: String) -> bool:
	"""Validate URL format using regex - HTTPS only for security"""
	var regex = RegEx.new()
	# Match https://host(:port)?(/path)? - HTTP not allowed
	regex.compile("^https://[a-zA-Z0-9.-]+(:[0-9]+)?(/[^\\s]*)?$")
	var result = regex.search(url)
	return result != null

func get_value(key: String, default = null):
	"""Get a configuration value by key"""
	if config.has(key):
		return config[key]
	return default

func set_value(key: String, value):
	"""Set a configuration value (runtime only, not persisted)"""
	config[key] = value

func save_user_config() -> bool:
	"""Save current configuration to user:// directory"""
	var file = FileAccess.open(USER_CONFIG_PATH, FileAccess.WRITE)
	if not file:
		push_error("Failed to save user config")
		return false

	var json_string = JSON.stringify(config, "\t")  # Pretty print with tabs
	file.store_string(json_string)
	file.close()

	if Logger:
		Logger.info("Config saved to user directory", "Config")
	return true

func get_backend_url() -> String:
	return config.backend_url

func get_user_id() -> String:
	return config.user_id

# get_password() removed - passwords no longer stored in config

func should_auto_login() -> bool:
	return config.auto_login

func is_debug_mode() -> bool:
	return config.debug_mode

func is_audio_enabled() -> bool:
	return config.enable_audio

func get_log_level() -> String:
	return config.log_level
