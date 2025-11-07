extends Node

## GlobalInput - Singleton for handling global keyboard shortcuts
##
## Manages keyboard shortcuts that work across all scenes:
## - Escape: Return to menu (with confirmation if in activity)
## - F1: Toggle help overlay
## - M: Toggle audio mute
## - Ctrl+R: Retry current activity
##
## Usage:
##   GlobalInput is an AutoLoad singleton - no manual instantiation needed.
##   GlobalInput.set_context(GlobalInput.CONTEXT_ACTIVITY)
##   GlobalInput.enable_shortcuts(false)  # Temporarily disable

# Context constants for smart shortcut behavior
enum Context {
	MENU,           # Main menu - limited shortcuts
	ACTIVITY,       # In an activity - all shortcuts enabled
	RESULTS,        # Results screen - limited shortcuts
	INPUT_FOCUSED   # Text input has focus - disable most shortcuts
}

signal help_requested()
signal menu_requested()
signal retry_requested()
signal mute_toggled(is_muted: bool)

var current_context: Context = Context.MENU
var shortcuts_enabled: bool = true
var help_overlay_visible: bool = false

func _ready():
	# Set process input to highest priority
	process_mode = Node.PROCESS_MODE_ALWAYS

func _input(event):
	## Handle global keyboard shortcuts based on current context

	if not shortcuts_enabled:
		return

	# Don't process shortcuts when typing in text fields
	if current_context == Context.INPUT_FOCUSED:
		return

	# F1 - Toggle help overlay (works in all contexts except input)
	if event.is_action_pressed("ui_help"):
		_handle_help_toggle()
		get_viewport().set_input_as_handled()
		return

	# M - Toggle mute (works in all contexts)
	if event.is_action_pressed("ui_mute"):
		_handle_mute_toggle()
		get_viewport().set_input_as_handled()
		return

	# Context-specific shortcuts
	match current_context:
		Context.ACTIVITY:
			_handle_activity_shortcuts(event)
		Context.MENU:
			_handle_menu_shortcuts(event)
		Context.RESULTS:
			_handle_results_shortcuts(event)

func _handle_activity_shortcuts(event):
	## Handle shortcuts specific to activity screens

	# Escape - Return to menu (with confirmation)
	if event.is_action_pressed("ui_back"):
		menu_requested.emit()
		get_viewport().set_input_as_handled()

	# Ctrl+R / Cmd+R - Retry activity
	elif event.is_action_pressed("ui_retry"):
		retry_requested.emit()
		get_viewport().set_input_as_handled()

func _handle_menu_shortcuts(event):
	## Handle shortcuts specific to menu screens

	# Escape in menu - quit application (or minimize on web)
	if event.is_action_pressed("ui_back"):
		if OS.has_feature("web"):
			# Can't quit on web, just ignore
			pass
		else:
			# Quit application
			get_tree().quit()
		get_viewport().set_input_as_handled()

func _handle_results_shortcuts(event):
	## Handle shortcuts specific to results screen

	# Escape - Return to menu
	if event.is_action_pressed("ui_back"):
		menu_requested.emit()
		get_viewport().set_input_as_handled()

func _handle_help_toggle():
	## Toggle help overlay visibility
	help_overlay_visible = not help_overlay_visible
	help_requested.emit()

	# Play sound feedback
	if AudioManager:
		AudioManager.play(AudioManager.SOUND_BUTTON_CLICK)

func _handle_mute_toggle():
	## Toggle audio mute state
	if AudioManager:
		var is_enabled = AudioManager.is_enabled()
		AudioManager.set_enabled(not is_enabled)
		mute_toggled.emit(not is_enabled)

		# Play click sound (will only play if we just unmuted)
		AudioManager.play(AudioManager.SOUND_BUTTON_CLICK)

## Public API

func set_context(new_context: Context):
	## Update the current input context for smart shortcut behavior
	## Call this when transitioning between screens
	current_context = new_context

func enable_shortcuts(enabled: bool):
	## Temporarily enable or disable all global shortcuts
	## Useful for modal dialogs or cutscenes
	shortcuts_enabled = enabled

func is_help_visible() -> bool:
	## Check if help overlay is currently visible
	return help_overlay_visible

func set_help_visible(visible: bool):
	## Manually set help overlay visibility state
	## Used by HelpOverlay to sync state
	help_overlay_visible = visible
