extends Node

## AudioManager - Singleton for managing game sound effects
##
## Provides a simple API for playing sounds with automatic pooling
## to support multiple concurrent sounds without clipping.
##
## Usage:
##   AudioManager.play("button_click")
##   AudioManager.play("correct")
##   AudioManager.set_enabled(false)  # Mute all sounds

# Audio player pool for concurrent sounds
const MAX_CONCURRENT_SOUNDS = 4

# Sound effect constants
const SOUND_BUTTON_CLICK = "button_click"
const SOUND_CORRECT = "correct"
const SOUND_INCORRECT = "incorrect"
const SOUND_ACTIVITY_COMPLETE = "activity_complete"
const SOUND_SESSION_START = "session_start"
const SOUND_SESSION_COMPLETE = "session_complete"

var audio_players: Array[AudioStreamPlayer] = []
var sounds: Dictionary = {}
var current_player_index: int = 0
var audio_enabled: bool = true

func _ready():
	# Create audio player pool
	for i in range(MAX_CONCURRENT_SOUNDS):
		var player = AudioStreamPlayer.new()
		player.bus = "Master"  # Use Master audio bus
		add_child(player)
		audio_players.append(player)

	# Pre-load all sound effects
	_load_sounds()

func _load_sounds():
	## Load sound effects from audio directory
	## Currently using placeholder paths - actual audio files need to be created

	# Try to load each sound effect, but don't fail if not found
	_try_load_sound(SOUND_BUTTON_CLICK, "res://audio/button_click.wav")
	_try_load_sound(SOUND_CORRECT, "res://audio/correct.wav")
	_try_load_sound(SOUND_INCORRECT, "res://audio/incorrect.wav")
	_try_load_sound(SOUND_ACTIVITY_COMPLETE, "res://audio/activity_complete.wav")
	_try_load_sound(SOUND_SESSION_START, "res://audio/session_start.wav")
	_try_load_sound(SOUND_SESSION_COMPLETE, "res://audio/session_complete.wav")

	# Log loaded sounds
	if sounds.is_empty():
		push_warning("AudioManager: No sound effects loaded. Audio files may not exist yet.")
	else:
		print("AudioManager: Loaded %d sound effects" % sounds.size())

func _try_load_sound(sound_name: String, path: String):
	## Attempt to load a sound effect, silently failing if not found
	if ResourceLoader.exists(path):
		sounds[sound_name] = load(path)
	else:
		push_warning("AudioManager: Sound not found: %s" % path)

func play(sound_name: String):
	## Play a sound effect by name
	## Uses round-robin through audio player pool to support concurrent playback

	if not audio_enabled:
		return

	if not sounds.has(sound_name):
		push_warning("AudioManager: Sound '%s' not loaded" % sound_name)
		return

	# Round-robin through player pool
	var player = audio_players[current_player_index]
	current_player_index = (current_player_index + 1) % MAX_CONCURRENT_SOUNDS

	# Only play if player is ready (not already playing, or far enough in)
	if not player.playing or player.get_playback_position() > 0.1:
		player.stream = sounds[sound_name]
		player.play()

func set_enabled(enabled: bool):
	## Enable or disable all audio playback
	## Useful for mute functionality
	audio_enabled = enabled

	# Stop all currently playing sounds when disabling
	if not enabled:
		for player in audio_players:
			if player.playing:
				player.stop()

func is_enabled() -> bool:
	## Check if audio is currently enabled
	return audio_enabled

func stop_all():
	## Stop all currently playing sounds
	for player in audio_players:
		if player.playing:
			player.stop()
