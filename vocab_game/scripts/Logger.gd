extends Node

## Logger - Centralized logging system with configurable log levels
## Provides better debugging and production monitoring capabilities

enum LogLevel {
	DEBUG = 0,
	INFO = 1,
	WARN = 2,
	ERROR = 3,
	NONE = 4
}

var current_level: LogLevel = LogLevel.INFO
var log_to_file: bool = false
var log_file_path: String = "user://game.log"
var log_file: FileAccess = null
var log_buffer: Array[String] = []  # Buffer logs before flushing
const FLUSH_THRESHOLD = 10  # Flush every 10 log entries
const MAX_LOG_SIZE = 1024 * 1024  # 1MB max log file size

# Color codes for terminal output (if supported)
const COLOR_DEBUG = "\u001b[36m"   # Cyan
const COLOR_INFO = "\u001b[32m"    # Green
const COLOR_WARN = "\u001b[33m"    # Yellow
const COLOR_ERROR = "\u001b[31m"   # Red
const COLOR_RESET = "\u001b[0m"

func _ready():
	# Validate AutoLoad dependency order
	assert(Config != null, "CRITICAL: Logger depends on Config AutoLoad! Check project.godot AutoLoad order - Config must come before Logger")

	# Load log level from config
	if not Config:
		push_error("Config not available! Logger falling back to INFO level")
		current_level = LogLevel.INFO
	else:
		var level_str = Config.get_log_level()
		current_level = _parse_log_level(level_str)

	# Enable file logging in production builds
	if not OS.is_debug_build():
		log_to_file = true
		_rotate_log_if_needed()
		_open_log_file()

	info("Logger initialized with level: " + _level_to_string(current_level))

func _exit_tree():
	# Flush remaining logs before closing
	_flush_log_buffer()
	if log_file:
		log_file.close()

func debug(message: String, context: String = ""):
	_log(LogLevel.DEBUG, message, context)

func info(message: String, context: String = ""):
	_log(LogLevel.INFO, message, context)

func warn(message: String, context: String = ""):
	_log(LogLevel.WARN, message, context)

func error(message: String, context: String = ""):
	_log(LogLevel.ERROR, message, context)

func _log(level: LogLevel, message: String, context: String = ""):
	if level < current_level:
		return

	var timestamp = Time.get_datetime_string_from_system()
	var level_str = _level_to_string(level)
	var context_str = (" [" + context + "]") if context != "" else ""
	var log_message = "[%s] %s%s: %s" % [timestamp, level_str, context_str, message]

	# Print to console with color
	var color = _get_color(level)
	print(color + log_message + COLOR_RESET)

	# Also use Godot's built-in logging
	match level:
		LogLevel.ERROR:
			push_error(message)
		LogLevel.WARN:
			push_warning(message)

	# Write to file if enabled (buffered for performance)
	if log_to_file and log_file:
		log_buffer.append(log_message)

		# Flush on ERROR level or when buffer is full
		if level == LogLevel.ERROR or log_buffer.size() >= FLUSH_THRESHOLD:
			_flush_log_buffer()

func _parse_log_level(level_str: String) -> LogLevel:
	match level_str.to_upper():
		"DEBUG":
			return LogLevel.DEBUG
		"INFO":
			return LogLevel.INFO
		"WARN", "WARNING":
			return LogLevel.WARN
		"ERROR":
			return LogLevel.ERROR
		"NONE":
			return LogLevel.NONE
		_:
			return LogLevel.INFO

func _level_to_string(level: LogLevel) -> String:
	match level:
		LogLevel.DEBUG:
			return "DEBUG"
		LogLevel.INFO:
			return "INFO"
		LogLevel.WARN:
			return "WARN"
		LogLevel.ERROR:
			return "ERROR"
		LogLevel.NONE:
			return "NONE"
		_:
			return "UNKNOWN"

func _get_color(level: LogLevel) -> String:
	if not OS.is_debug_build():
		return ""  # No colors in production

	match level:
		LogLevel.DEBUG:
			return COLOR_DEBUG
		LogLevel.INFO:
			return COLOR_INFO
		LogLevel.WARN:
			return COLOR_WARN
		LogLevel.ERROR:
			return COLOR_ERROR
		_:
			return COLOR_RESET

func _open_log_file():
	log_file = FileAccess.open(log_file_path, FileAccess.WRITE)
	if not log_file:
		push_error("Failed to open log file: " + log_file_path)
		log_to_file = false
	else:
		var header = "=== Playcademy Vocab Game Log ==="
		log_file.store_line(header)
		log_file.store_line("Started: " + Time.get_datetime_string_from_system())
		log_file.store_line("=================================")

func set_log_level(level: LogLevel):
	"""Change the current log level at runtime"""
	current_level = level
	info("Log level changed to: " + _level_to_string(level))

func get_log_file_contents() -> String:
	"""Read the entire log file contents (for debugging)"""
	if not FileAccess.file_exists(log_file_path):
		return "No log file found"

	var file = FileAccess.open(log_file_path, FileAccess.READ)
	if not file:
		return "Failed to read log file"

	var contents = file.get_as_text()
	file.close()
	return contents

func clear_log_file():
	"""Clear the log file"""
	_flush_log_buffer()

	if log_file:
		log_file.close()

	if FileAccess.file_exists(log_file_path):
		DirAccess.remove_absolute(log_file_path)

	if log_to_file:
		_open_log_file()

func _flush_log_buffer():
	"""Flush buffered log entries to file"""
	if not log_file or log_buffer.is_empty():
		return

	for entry in log_buffer:
		log_file.store_line(entry)

	log_file.flush()
	log_buffer.clear()

func _rotate_log_if_needed():
	"""Rotate log file if it exceeds max size"""
	if not FileAccess.file_exists(log_file_path):
		return

	var file = FileAccess.open(log_file_path, FileAccess.READ)
	if not file:
		return

	var file_size = file.get_length()
	file.close()

	if file_size > MAX_LOG_SIZE:
		# Rename current log to .old
		var old_log_path = log_file_path + ".old"

		# Delete existing .old file if present
		if FileAccess.file_exists(old_log_path):
			DirAccess.remove_absolute(old_log_path)

		# Rename current to .old
		DirAccess.rename_absolute(log_file_path, old_log_path)
		push_warning("Log file rotated (exceeded " + str(MAX_LOG_SIZE / 1024) + "KB)")
