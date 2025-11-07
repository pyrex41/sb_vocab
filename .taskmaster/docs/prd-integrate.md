extends Control

signal answer_submitted(answer)

@onready var instruction_label = $VBoxContainer/InstructionLabel
@onready var play_audio_button = $VBoxContainer/PlayAudioButton
@onready var answer_input = $VBoxContainer/AnswerInput
@onready var submit_button = $VBoxContainer/SubmitButton

var word_data: Dictionary
var audio_player: AudioStreamPlayer
var audio_loaded = false

func _ready():
    play_audio_button.pressed.connect(_on_play_audio)
    submit_button.pressed.connect(_on_submit)

    # Setup AudioStreamPlayer node
    audio_player = AudioStreamPlayer.new()
    add_child(audio_player)

func setup(activity_data: Dictionary):
    word_data = activity_data.word
    instruction_label.text = "Listen to the word and spell it correctly"
    answer_input.placeholder_text = "Type the word here..."
    play_audio_button.disabled = true # Disable until audio loads

    # Load audio from backend URL
    if word_data.has("audio_url"):
        _load_audio(word_data.audio_url)
    else:
        _show_text_fallback()
        
    # Auto-play audio once loaded (handled in _on_audio_loaded)

func _load_audio(url: String):
    var http = HTTPRequest.new()
    add_child(http)
    http.request_completed.connect(_on_audio_loaded)
    
    # Request the audio file
    var error = http.request(url)
    if error != OK:
        _show_text_fallback()
        push_error("HTTP request error: " + error_string(error))

func _on_audio_loaded(result: int, code: int, headers: PackedStringArray, body: PackedByteArray):
    if code == 200:
        var stream = AudioStreamMP3.new()
        stream.data = body # Load MP3 data into audio stream
        audio_player.stream = stream
        audio_loaded = true
        play_audio_button.disabled = false
        _on_play_audio() # Auto-play after loading
    else:
        _show_text_fallback()
        push_error("Failed to load audio. HTTP code: " + str(code))

func _show_text_fallback():
    play_audio_button.text = "🔊 Audio Unavailable"
    play_audio_button.disabled = true
    instruction_label.text = "Spell the word from memory:"

func _on_play_audio():
    if audio_loaded:
        audio_player.play()
    else:
        # If button is enabled but audio_loaded is false, indicates a problem
        _show_text_fallback()

func _on_submit():
    var answer = answer_input.text.strip_edges()
    if answer.length() > 0:
        SessionManager.submit_answer(answer)

------
This is excellent work, demonstrating a clear understanding of the core integration required.

To ensure the solution is complete, robust, and aligns with best practices for a Godot/Playcademy integration, here are the remaining tasks and context, structured as the next steps in your implementation plan.

---

## Phase 5: Final Code Polishing and Testing Checklist

Your last response successfully implemented the four core integration points, the basic error handler, and the audio logic. This phase focuses on connecting the remaining logic pieces, handling cleanup, and verifying the new asynchronous flow.

### 5.1 Update Remaining Callers (Critical Cleanup)

You must ensure that all references to the now-removed `MockBackend` and its logic are updated or removed.

| File Path | Function | Change Required |
| :--- | :--- | :--- |
| **`vocab_game/scripts/activities/multiple_choice_activity.gd`** | `setup` | **Remove** the call to `MockBackend.get_random_wrong_definitions`. The new API `/session/start` already returns all `options` in the activity data. |
| **`vocab_game/scripts/activities/synonym_antonym_activity.gd`** | `setup_synonym_choices`, `setup_antonym_choices` | **Remove** the mock logic that iterates over `MockBackend.vocabulary_data` to synthesize wrong options/synonyms. The new API returns **all** required `options` in the activity data. |
| **`vocab_game/scripts/activity_controller.gd`** | `_on_answer_submitted`, `_ready`, `update_progress_display` | **Remove** all calls to `MockBackend.get_current_activity()` and `MockBackend.get_progress()` as these methods no longer exist. The core logic should now rely on `SessionManager` signals (`activity_changed`) to receive data. |
| **`vocab_game/scripts/session_manager.gd`** | `get_progress_data` | **Remove** this function as its logic was moved into the new `_load_progress()` in `ProgressScreen.gd` and is no longer needed by internal components. |
| **`vocab_game/project.godot`** | `[autoload]` | **Verify** that `MockBackend` is entirely removed from the `[autoload]` section. |

### 5.2 Implement Loading State UI (UX Polish)

Your `SessionManager.gd` now correctly emits `loading_started` and `loading_ended`. You need to integrate this with the UI to prevent user input during an API call.

| File Path | Function | Change Required |
| :--- | :--- | :--- |
| **`vocab_game/scenes/GameSession.tscn`** | N/A | Add a `ColorRect` node as a transparent overlay/spinner area named `LoadingOverlay`. |
| **`vocab_game/scripts/GameSession.gd`** | `_ready`, `_on_activity_changed` | **Connect** to `SessionManager.loading_started` to show the overlay and `SessionManager.loading_ended` to hide it. |
| **`vocab_game/scripts/activities/*.gd`** | `_ready` (or similar) | **Connect** to `SessionManager.loading_started` to temporarily disable input fields and submit buttons. |

### 5.3 Test and Debug Checklist

Before considering the task complete, verify the following critical functional and error paths.

#### Functional Tests (Happy Path)
- [ ] **Start Flow:** Hitting "Start Session" now fetches real data and correctly sets `session_id`, `activities`, and `current_activity_index`.
- [ ] **Activity Flow:** Can complete a full session from start to finish.
- [ ] **Data Consistency:** The `activity_data` dictionary passed through signals contains the necessary data for all 5 activities (including the `options` array for MCQs/Synonym).
- [ ] **Progress:** The "View Progress" screen loads the correct data from the `/progress` API call (no more mock data).

#### Error Path Tests (Robustness)
- [ ] **Network Error:** Simulate a network failure (e.g., block the API URL in a proxy). The `loading_ended` signal should fire, and the `api_error` signal should show a user-friendly message ("Can't connect to the server...").
- [ ] **Session Not Found:** Attempting to submit an answer with an invalid `session_id` should result in a `403` or `404` API error response, which the handler should catch gracefully.
- [ ] **Timeouts:** Ensure a long-running request is properly handled by the async flow (it should not freeze the game, and should return a `TIMEOUT` error).

---

## Phase 6: Final Deliverable Preparation

The final step is to prepare the project for deployment and finalize the deliverables mentioned in the original prompt.

### 6.1 Documentation and Reporting

- **Feedback Report:** Use the structure provided in `.taskmaster/docs/playcademy-implementation-checklist.md` as a template for your final **Feedback Report**. This is a required deliverable.
- **`README.md` / `QUICKSTART.md`:** Update project documentation to reference the `PlaycademySdk` and the real backend instead of the `MockBackend`.

### 6.2 Deployment Preparation

- **Export Config:** Update `vocab_game/export_presets.cfg` to include the Playcademy HTML shell for web export:
  ```ini
  [preset.0.options]
  ; ... other options ...
  html/custom_html_shell="res://addons/playcademy/shell.html"
  ; ... other options ...
  ```
- **CLI Deployment:** Be ready to run the final command: `playcademy deploy`.
