extends Node

## Configuration Manager
## Loads configuration from config.json file or uses default values
## This allows different environments (dev, staging, production) to have different settings

# Default configuration values
var config = {
	"backend_url": "http://localhost:8788/api",
	"user_id": "student.fresh@demo.playcademy.com",
	"password": "password",
	"auto_login": true,
	"debug_mode": true,
	"enable_audio": true,
	"log_level": "INFO"  # DEBUG, INFO, WARN, ERROR
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
			print("Config: Loaded from user directory")

	# Fall back to res:// directory (read-only, bundled with game)
	if not loaded and FileAccess.file_exists(CONFIG_FILE_PATH):
		loaded = _load_from_file(CONFIG_FILE_PATH)
		if loaded:
			print("Config: Loaded from resource directory")

	if not loaded:
		print("Config: Using default values (no config file found)")

	# Log configuration (hide sensitive data in production)
	if config.debug_mode:
		print("Config: Backend URL: ", config.backend_url)
		print("Config: User ID: ", config.user_id)
		print("Config: Auto Login: ", config.auto_login)
		print("Config: Debug Mode: ", config.debug_mode)

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

	return true

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

	print("Config: Saved to user directory")
	return true

func get_backend_url() -> String:
	return config.backend_url

func get_user_id() -> String:
	return config.user_id

func get_password() -> String:
	return config.password

func should_auto_login() -> bool:
	return config.auto_login

func is_debug_mode() -> bool:
	return config.debug_mode

func is_audio_enabled() -> bool:
	return config.enable_audio

func get_log_level() -> String:
	return config.log_level
