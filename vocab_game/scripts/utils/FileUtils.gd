class_name FileUtils

## FileUtils - Helper functions for safe file operations
## Reduces code duplication and provides consistent error handling

static func safe_read_json(path: String) -> Dictionary:
	"""Safely read and parse a JSON file"""
	var file = FileAccess.open(path, FileAccess.READ)
	if not file:
		push_warning("Failed to open file: " + path)
		return {}

	var json_text = file.get_as_text()
	file.close()

	var json = JSON.new()
	var parse_result = json.parse(json_text)

	if parse_result != OK:
		push_error("Failed to parse JSON in " + path + " at line " + str(json.get_error_line()))
		return {}

	if typeof(json.data) != TYPE_DICTIONARY:
		push_error("JSON file " + path + " does not contain an object")
		return {}

	return json.data

static func safe_write_json(path: String, data: Dictionary, pretty: bool = true) -> bool:
	"""Safely write a dictionary as JSON to a file"""
	var file = FileAccess.open(path, FileAccess.WRITE)
	if not file:
		push_error("Failed to open file for writing: " + path)
		return false

	var json_string = JSON.stringify(data, "\t" if pretty else "")
	file.store_string(json_string)
	file.close()

	return true

static func safe_read_text(path: String) -> String:
	"""Safely read a text file"""
	var file = FileAccess.open(path, FileAccess.READ)
	if not file:
		push_warning("Failed to open file: " + path)
		return ""

	var text = file.get_as_text()
	file.close()

	return text

static func safe_write_text(path: String, text: String) -> bool:
	"""Safely write text to a file"""
	var file = FileAccess.open(path, FileAccess.WRITE)
	if not file:
		push_error("Failed to open file for writing: " + path)
		return false

	file.store_string(text)
	file.close()

	return true

static func file_exists(path: String) -> bool:
	"""Check if a file exists"""
	return FileAccess.file_exists(path)

static func delete_file(path: String) -> bool:
	"""Safely delete a file"""
	if not file_exists(path):
		return true  # Already doesn't exist

	var result = DirAccess.remove_absolute(path)
	if result != OK:
		push_error("Failed to delete file: " + path)
		return false

	return true

static func get_file_size(path: String) -> int:
	"""Get the size of a file in bytes"""
	if not file_exists(path):
		return 0

	var file = FileAccess.open(path, FileAccess.READ)
	if not file:
		return 0

	var size = file.get_length()
	file.close()

	return size

static func rename_file(old_path: String, new_path: String) -> bool:
	"""Safely rename/move a file"""
	if not file_exists(old_path):
		push_error("Source file does not exist: " + old_path)
		return false

	var result = DirAccess.rename_absolute(old_path, new_path)
	if result != OK:
		push_error("Failed to rename file from " + old_path + " to " + new_path)
		return false

	return true

static func ensure_directory(path: String) -> bool:
	"""Ensure a directory exists, creating it if necessary"""
	var dir = DirAccess.open("user://")
	if not dir:
		push_error("Failed to access user directory")
		return false

	if not dir.dir_exists(path):
		var result = dir.make_dir_recursive(path)
		if result != OK:
			push_error("Failed to create directory: " + path)
			return false

	return true
