extends CanvasLayer

## HelpOverlay - Keyboard shortcuts help display
##
## Displays a modal overlay showing all available keyboard shortcuts.
## Triggered by F1 or ESC keys via GlobalInput singleton.
##
## Usage:
##   help_overlay.show_help()
##   help_overlay.hide_help()
##
## Scene Structure Expected:
##   CanvasLayer (HelpOverlay)
##   └── Panel (BackgroundPanel)
##       └── CenterContainer
##           └── Panel (HelpCard)
##               └── MarginContainer
##                   └── VBoxContainer
##                       ├── Label (TitleLabel)
##                       ├── ScrollContainer
##                       │   └── VBoxContainer (ShortcutList)
##                       └── Label (CloseLabel)

# Animation constants
const FADE_IN_DURATION = 0.2
const FADE_OUT_DURATION = 0.15

# Node references
@onready var help_card: Panel = $BackgroundPanel/CenterContainer/HelpCard
@onready var title_label: Label = $BackgroundPanel/CenterContainer/HelpCard/MarginContainer/VBoxContainer/TitleLabel
@onready var shortcut_list: VBoxContainer = $BackgroundPanel/CenterContainer/HelpCard/MarginContainer/VBoxContainer/ScrollContainer/ShortcutList
@onready var close_label: Label = $BackgroundPanel/CenterContainer/HelpCard/MarginContainer/VBoxContainer/CloseLabel

func _ready():
	# Start hidden
	hide()
	modulate.a = 0

	# Connect to GlobalInput signals
	if GlobalInput:
		GlobalInput.help_requested.connect(_on_help_requested)

	# Populate shortcuts
	_populate_shortcuts()

func _populate_shortcuts():
	## Dynamically populate the shortcuts list
	## This allows context-aware help in the future

	if not shortcut_list:
		return

	# Clear existing shortcuts (in case of repopulation)
	for child in shortcut_list.get_children():
		child.queue_free()

	# Define shortcuts with descriptions
	var shortcuts = [
		{"keys": "Enter / Space", "description": "Submit answer or continue"},
		{"keys": "1, 2, 3, 4", "description": "Select multiple choice option"},
		{"keys": "Escape", "description": "Return to menu (with confirmation)"},
		{"keys": "F1", "description": "Toggle this help overlay"},
		{"keys": "M", "description": "Mute/unmute sound effects"},
		{"keys": "Ctrl+R (⌘+R on Mac)", "description": "Retry current activity"},
	]

	# Create label for each shortcut
	for shortcut in shortcuts:
		var label = Label.new()
		label.text = "%s - %s" % [shortcut.keys, shortcut.description]
		label.autowrap_mode = TextServer.AUTOWRAP_WORD_SMART
		label.add_theme_font_size_override("font_size", 18)
		shortcut_list.add_child(label)

		# Add spacing between shortcuts
		var spacer = Control.new()
		spacer.custom_minimum_size.y = 10
		shortcut_list.add_child(spacer)

func show_help():
	## Display the help overlay with animation

	# Sync state with GlobalInput
	if GlobalInput:
		GlobalInput.set_help_visible(true)

	# Show immediately
	show()

	# Animate entrance
	if help_card:
		help_card.scale = Vector2(0.9, 0.9)

	# Fade in background
	var fade_tween = create_tween()
	fade_tween.set_parallel(true)
	fade_tween.tween_property(self, "modulate:a", 1.0, FADE_IN_DURATION)

	# Scale up help card
	if help_card:
		var scale_tween = create_tween()
		scale_tween.set_ease(Tween.EASE_OUT)
		scale_tween.set_trans(Tween.TRANS_BACK)
		scale_tween.tween_property(help_card, "scale", Vector2(1.0, 1.0), 0.3)

	# Play sound
	if AudioManager:
		AudioManager.play(AudioManager.SOUND_BUTTON_CLICK)

func hide_help():
	## Hide the help overlay with animation

	# Sync state with GlobalInput
	if GlobalInput:
		GlobalInput.set_help_visible(false)

	# Scale down card
	if help_card:
		var scale_tween = create_tween()
		scale_tween.set_ease(Tween.EASE_IN)
		scale_tween.tween_property(help_card, "scale", Vector2(0.9, 0.9), FADE_OUT_DURATION)

	# Fade out
	var fade_tween = create_tween()
	fade_tween.tween_property(self, "modulate:a", 0.0, FADE_OUT_DURATION)
	await fade_tween.finished

	hide()

	# Reset for next time
	if help_card:
		help_card.scale = Vector2(1.0, 1.0)
	modulate.a = 1.0

	# Play sound
	if AudioManager:
		AudioManager.play(AudioManager.SOUND_BUTTON_CLICK)

func _on_help_requested():
	## Handle help toggle from GlobalInput
	if visible:
		hide_help()
	else:
		show_help()

func _input(event):
	## Handle input when help is visible
	## ESC or F1 closes the help overlay

	if not visible:
		return

	if event.is_action_pressed("ui_help") or event.is_action_pressed("ui_cancel"):
		hide_help()
		get_viewport().set_input_as_handled()

## Public API for context-aware help

func add_context_shortcut(keys: String, description: String):
	## Add a context-specific shortcut to the help display
	## Useful for activity-specific shortcuts
	if not shortcut_list:
		return

	var label = Label.new()
	label.text = "%s - %s" % [keys, description]
	label.autowrap_mode = TextServer.AUTOWRAP_WORD_SMART
	label.add_theme_font_size_override("font_size", 18)
	label.add_theme_color_override("font_color", Color(0.8, 1.0, 0.8))  # Slight green tint
	shortcut_list.add_child(label)

func clear_context_shortcuts():
	## Remove context-specific shortcuts
	## Called when changing screens/activities
	_populate_shortcuts()
