extends Control

## ErrorDialog - Rich error display with kid-friendly messages
##
## Displays error messages in an engaging, non-threatening way for elementary students.
## Provides appropriate actions (retry, skip, etc.) based on error type.
##
## Usage:
##   error_dialog.show_error(ErrorMessages.ERROR_NETWORK_TIMEOUT)
##   await error_dialog.action_pressed
##   # Handle retry/skip/etc.

signal action_pressed(action_type: String)
signal cancelled()

# Animation constants
const FADE_IN_DURATION = 0.3
const BOUNCE_DURATION = 0.4

# Node references (will be connected when scene is created)
@onready var icon_label: Label = $BackgroundPanel/CenterContainer/ErrorCard/MarginContainer/VBoxContainer/IconLabel
@onready var title_label: Label = $BackgroundPanel/CenterContainer/ErrorCard/MarginContainer/VBoxContainer/TitleLabel
@onready var message_label: Label = $BackgroundPanel/CenterContainer/ErrorCard/MarginContainer/VBoxContainer/MessageLabel
@onready var action_button: Button = $BackgroundPanel/CenterContainer/ErrorCard/MarginContainer/VBoxContainer/ButtonContainer/ActionButton
@onready var cancel_button: Button = $BackgroundPanel/CenterContainer/ErrorCard/MarginContainer/VBoxContainer/ButtonContainer/CancelButton
@onready var error_card: Panel = $BackgroundPanel/CenterContainer/ErrorCard

var current_error_type: String = ""

func _ready():
	# Connect button signals
	if action_button:
		action_button.pressed.connect(_on_action_pressed)
	if cancel_button:
		cancel_button.pressed.connect(_on_cancel_pressed)

	# Start hidden
	hide()
	modulate.a = 0

func show_error(error_type: String):
	## Display an error with kid-friendly messaging

	current_error_type = error_type
	var error_data = ErrorMessages.get_message(error_type)

	# Update UI elements
	if icon_label:
		icon_label.text = error_data.icon
	if title_label:
		title_label.text = error_data.title
	if message_label:
		message_label.text = error_data.message
	if action_button:
		action_button.text = error_data.action

	# Show/hide cancel button based on error type
	if cancel_button:
		cancel_button.visible = error_data.can_skip

	# Play error sound
	if AudioManager:
		AudioManager.play(AudioManager.SOUND_INCORRECT)

	# Show with animation
	show()
	_animate_entrance()

func _animate_entrance():
	## Animate the error dialog entrance

	# Start with error card scaled down
	if error_card:
		error_card.scale = Vector2(0.8, 0.8)

	# Fade in background
	var fade_tween = create_tween()
	fade_tween.set_parallel(true)
	fade_tween.tween_property(self, "modulate:a", 1.0, FADE_IN_DURATION)

	# Bounce in error card
	if error_card:
		var bounce_tween = create_tween()
		bounce_tween.set_ease(Tween.EASE_OUT)
		bounce_tween.set_trans(Tween.TRANS_BACK)
		bounce_tween.tween_property(error_card, "scale", Vector2(1.0, 1.0), BOUNCE_DURATION)

func _animate_exit():
	## Animate the error dialog exit

	# Scale down error card
	if error_card:
		var scale_tween = create_tween()
		scale_tween.set_ease(Tween.EASE_IN)
		scale_tween.tween_property(error_card, "scale", Vector2(0.8, 0.8), 0.2)

	# Fade out
	var fade_tween = create_tween()
	fade_tween.tween_property(self, "modulate:a", 0.0, 0.2)
	await fade_tween.finished

	hide()

	# Reset for next time
	if error_card:
		error_card.scale = Vector2(1.0, 1.0)
	modulate.a = 1.0

func _on_action_pressed():
	## Handle action button press (Retry, Skip, etc.)

	# Play button click sound
	if AudioManager:
		AudioManager.play(AudioManager.SOUND_BUTTON_CLICK)

	# Emit signal with action type
	action_pressed.emit(current_error_type)

	# Animate exit
	_animate_exit()

func _on_cancel_pressed():
	## Handle cancel button press

	# Play button click sound
	if AudioManager:
		AudioManager.play(AudioManager.SOUND_BUTTON_CLICK)

	# Emit cancelled signal
	cancelled.emit()

	# Animate exit
	_animate_exit()

func _input(event):
	## Handle keyboard shortcuts when error dialog is visible

	if not visible:
		return

	if event.is_action_pressed("ui_accept"):  # Enter key
		_on_action_pressed()
		get_viewport().set_input_as_handled()
	elif event.is_action_pressed("ui_cancel"):  # Escape key
		if cancel_button and cancel_button.visible:
			_on_cancel_pressed()
			get_viewport().set_input_as_handled()
