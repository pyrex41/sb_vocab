extends Control

# Main menu elements
@onready var start_button = $CenterContainer/VBoxContainer/StartButton
@onready var progress_button = $CenterContainer/VBoxContainer/ProgressButton
@onready var title_label = $CenterContainer/VBoxContainer/TitleLabel
@onready var subtitle_label = $CenterContainer/VBoxContainer/SubtitleLabel

# Login UI elements
@onready var login_container = $LoginContainer
@onready var password_input = $LoginContainer/VBoxContainer/PasswordInput
@onready var login_button = $LoginContainer/VBoxContainer/LoginButton
@onready var login_error_label = $LoginContainer/VBoxContainer/ErrorLabel
@onready var login_title = $LoginContainer/VBoxContainer/LoginTitle

var is_loading: bool = false
var is_logged_in: bool = false

func _ready():
	# Connect main menu buttons
	start_button.pressed.connect(_on_start_button_pressed)
	progress_button.pressed.connect(_on_progress_button_pressed)

	# Connect login UI
	login_button.pressed.connect(_on_login_button_pressed)
	password_input.text_submitted.connect(_on_password_submitted)

	# Set initial button text with emoji
	_set_buttons_enabled(true)

	# Add welcoming animation
	_animate_title()

	# Connect to loading signals
	SessionManager.loading_started.connect(_on_loading_started)
	SessionManager.loading_ended.connect(_on_loading_ended)

	# Connect to SDK ready signal
	if PlaycademySdk:
		PlaycademySdk.sdk_ready.connect(_on_sdk_ready)

	# Check if already logged in
	_check_login_status()

func _animate_title():
	# Simple pulse animation for the title
	var tween = create_tween()
	tween.set_loops()
	tween.tween_property(title_label, "scale", Vector2(1.05, 1.05), 1.0)
	tween.tween_property(title_label, "scale", Vector2(1.0, 1.0), 1.0)

func _check_login_status():
	"""Check if user is already logged in and show appropriate UI"""
	if PlaycademySdk and PlaycademySdk.is_ready:
		_show_main_menu()
	else:
		_show_login_screen()

func _show_login_screen():
	"""Show the login form and hide main menu"""
	login_container.visible = true
	$CenterContainer.visible = false
	password_input.grab_focus()
	login_error_label.text = ""
	login_button.disabled = false
	login_button.text = "🔐 Login"

func _show_main_menu():
	"""Show the main menu and hide login form"""
	login_container.visible = false
	$CenterContainer.visible = true
	is_logged_in = true

func _on_login_button_pressed():
	await _attempt_login()

func _on_password_submitted(_password: String):
	await _attempt_login()

func _attempt_login():
	"""Attempt to login with the entered password"""
	if not PlaycademySdk:
		_show_login_error("SDK not available")
		return

	var password = password_input.text.strip_edges()
	if password == "":
		_show_login_error("Please enter a password")
		return

	# Disable login button and show loading
	login_button.disabled = true
	login_button.text = "Logging in... 🔄"
	login_error_label.text = ""
	password_input.editable = false

	# Attempt login
	var success = await PlaycademySdk.login(password)

	if success:
		print("MainMenu: Login successful, showing main menu")
		_show_main_menu()
	else:
		_show_login_error("Login failed. Please check your password.")
		login_button.disabled = false
		login_button.text = "🔐 Login"
		password_input.editable = true
		password_input.grab_focus()

func _show_login_error(message: String):
	"""Show login error message"""
	login_error_label.text = message
	# Shake animation for error feedback
	var tween = create_tween()
	tween.tween_property(login_container, "rotation_degrees", 5, 0.1)
	tween.tween_property(login_container, "rotation_degrees", -5, 0.1)
	tween.tween_property(login_container, "rotation_degrees", 0, 0.1)

func _on_sdk_ready():
	"""Called when SDK becomes ready (auto-login successful)"""
	print("MainMenu: SDK ready signal received")
	_show_main_menu()

func _on_start_button_pressed():
	if is_loading or not is_logged_in:
		return

	# Play button click sound
	if AudioManager:
		AudioManager.play(AudioManager.SOUND_BUTTON_CLICK)

	_set_buttons_enabled(false)
	start_button.text = "Starting... 🚀"

	# Small delay for visual feedback
	await get_tree().create_timer(0.3).timeout
	get_tree().change_scene_to_file("res://scenes/GameSession.tscn")

func _on_progress_button_pressed():
	if is_loading or not is_logged_in:
		return

	# Play button click sound
	if AudioManager:
		AudioManager.play(AudioManager.SOUND_BUTTON_CLICK)

	_set_buttons_enabled(false)
	progress_button.text = "Loading... 📊"

	# Small delay for visual feedback
	await get_tree().create_timer(0.3).timeout
	get_tree().change_scene_to_file("res://scenes/ProgressScreen.tscn")

func _on_loading_started():
	is_loading = true
	_set_buttons_enabled(false)

func _on_loading_ended():
	is_loading = false
	_set_buttons_enabled(true)

func _set_buttons_enabled(enabled: bool):
	start_button.disabled = not enabled
	progress_button.disabled = not enabled

	if enabled:
		start_button.text = "🎮 Start Learning!"
		progress_button.text = "📊 View Progress"
