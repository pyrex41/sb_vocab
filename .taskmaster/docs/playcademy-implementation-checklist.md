# Playcademy Vocab Integration - Step-by-Step Checklist

Use this checklist to track your progress through the integration. Check off each item as you complete it.

---

## Pre-Integration Setup

### Environment Setup
- [ ] Godot 4.3+ installed and working
- [ ] Git configured with GitHub access
- [ ] Playcademy CLI installed (`npm install -g @playcademy/cli`)
- [ ] Playcademy account created
- [ ] Backend API endpoint URLs obtained
- [ ] Staging environment credentials received

### Project Setup
- [ ] Copied `vocab_game/` to working directory
- [ ] Opened project in Godot (no errors)
- [ ] Ran mock implementation successfully
- [ ] Completed one full mock session (Main Menu → Activities → Results)
- [ ] Read `DEV_NOTES.md` thoroughly
- [ ] Read `QUICKSTART.md`
- [ ] Reviewed signal flow in `SessionManager.gd`

### Documentation Review
- [ ] Reviewed https://docs.playcademy.net
- [ ] Read Godot platform guide
- [ ] Studied SDK API reference
- [ ] Reviewed authentication patterns
- [ ] Read deployment documentation

---

## Phase 1: SDK Installation (30-45 minutes)

### Install Playcademy SDK
- [ ] Opened AssetLib tab in Godot
- [ ] Searched for "Playcademy"
- [ ] Downloaded Playcademy bundle
- [ ] Installed to `addons/playcademy/`
- [ ] Verified installation (folder exists with SDK files)

### Enable Plugins
- [ ] Opened Project > Project Settings > Plugins
- [ ] Found "Playcademy Manifest Exporter"
- [ ] Enabled Manifest Exporter ✓
- [ ] Found "Playcademy Sandbox"
- [ ] Enabled Sandbox ✓
- [ ] Restarted Godot editor

### Configure AutoLoad
- [ ] Opened Project > Project Settings > Globals > AutoLoad
- [ ] Clicked "Add" button
- [ ] Set Path: `res://addons/playcademy/sdk/PlaycademySDK.gd`
- [ ] Set Name: `PlaycademySdk` (exact spelling!)
- [ ] Clicked "Add" to confirm
- [ ] Verified it appears in AutoLoad list
- [ ] Restarted Godot editor
- [ ] Checked Output panel (no errors)

### Verify SDK Works
- [ ] Created test script:
```gdscript
extends Node

func _ready():
    print("SDK loaded: ", PlaycademySdk != null)
    print("Backend available: ", PlaycademySdk.backend != null)
```
- [ ] Ran test (F5)
- [ ] Verified both prints show `true`
- [ ] Deleted test script

**✓ Checkpoint 1:** SDK installed and working

---

## Phase 2: Integration Part 1 - Session Start (1-2 hours)

### Backup Original Files
- [ ] Created backup of `scripts/SessionManager.gd`
- [ ] Copied to `scripts/SessionManager.gd.backup`

### Update Session Start Function
- [ ] Opened `scripts/SessionManager.gd`
- [ ] Located `start_new_session(grade: int)` function
- [ ] Found line: `var response = MockBackend.start_session(grade)`

**Original Code (BEFORE):**
```gdscript
func start_new_session(grade: int):
    var response = MockBackend.start_session(grade)
    current_session_id = response.session_id
    activity_queue = response.activities
    current_activity_index = 0
    session_started.emit(response)
    _load_first_activity()
```

**New Code (AFTER):**
```gdscript
func start_new_session(grade: int):
    # Show loading state
    emit_signal("loading_started")
    
    # Call real backend via SDK
    var response = await PlaycademySdk.backend.request(
        "POST",
        "/session/start",
        {
            "user_id": PlaycademySdk.user_id,
            "grade": grade
        }
    )
    
    # Hide loading state
    emit_signal("loading_ended")
    
    # Handle errors
    if response.error:
        _handle_api_error(response.error)
        return
    
    # Process successful response
    current_session_id = response.session_id
    activity_queue = response.activities
    current_activity_index = 0
    
    session_started.emit(response)
    _load_first_activity()
```

### Implementation Steps
- [ ] Replaced MockBackend call with SDK call
- [ ] Added `await` keyword
- [ ] Added loading signals (if signals exist, else create them)
- [ ] Added error handling call
- [ ] Saved file
- [ ] No syntax errors in Output panel

### Add Loading Signals (if not present)
- [ ] Added to top of SessionManager.gd:
```gdscript
signal loading_started
signal loading_ended
```

### Create Error Handler Function
- [ ] Added to SessionManager.gd:
```gdscript
func _handle_api_error(error: Dictionary):
    var message = "Oops! Something went wrong."
    
    if error.has("code"):
        match error.code:
            "NETWORK_ERROR":
                message = "Can't connect to the server. Check your internet!"
            "TIMEOUT":
                message = "The server is taking too long. Please try again!"
            "AUTH_ERROR":
                message = "Please log in again."
            _:
                if error.has("message"):
                    message = error.message
    
    # Show error to user (implement UI later)
    push_error("API Error: " + str(error))
    print("API Error: ", message)
    
    # Emit signal for UI to handle
    emit_signal("api_error", message)
```
- [ ] Added signal definition: `signal api_error(message)`

### Test Session Start
- [ ] Ran game (F5)
- [ ] Clicked "Start Session" 
- [ ] Checked Output panel for errors
- [ ] Verified session starts (or shows meaningful error)
- [ ] If errors, noted them down:
  - Error message: _______________
  - Potential cause: _______________

**✓ Checkpoint 2:** Session starts with real backend (or shows clear error)

---

## Phase 2: Integration Part 2 - Answer Submission (1-2 hours)

### Update Submit Answer Function
- [ ] Located `submit_answer(answer: String)` in SessionManager.gd

**Original Code (BEFORE):**
```gdscript
func submit_answer(answer: String):
    var response = MockBackend.submit_attempt(
        current_session_id,
        current_activity_index,
        answer
    )
    attempt_result.emit(response.correct, response.feedback)
    
    if response.correct:
        await get_tree().create_timer(1.5).timeout
        _advance_activity()
```

**New Code (AFTER):**
```gdscript
func submit_answer(answer: String):
    # Show loading
    emit_signal("loading_started")
    
    # Submit to backend
    var response = await PlaycademySdk.backend.request(
        "POST",
        "/session/attempt",
        {
            "session_id": current_session_id,
            "activity_index": current_activity_index,
            "answer": answer
        }
    )
    
    # Hide loading
    emit_signal("loading_ended")
    
    # Handle errors
    if response.error:
        _handle_api_error(response.error)
        return
    
    # Process response
    attempt_result.emit(response.correct, response.feedback)
    
    if response.correct:
        await get_tree().create_timer(1.5).timeout
        _advance_activity()
    else:
        # Optionally show correct answer
        if response.has("correct_answer"):
            print("Correct answer was: ", response.correct_answer)
```

### Implementation Steps
- [ ] Replaced MockBackend call with SDK call
- [ ] Added `await` keyword
- [ ] Added loading signals
- [ ] Added error handling
- [ ] Saved file
- [ ] No syntax errors

### Test Answer Submission
- [ ] Ran game (F5)
- [ ] Started session
- [ ] Completed first activity with correct answer
- [ ] Verified feedback shows "Correct!"
- [ ] Verified advances to next activity
- [ ] Completed activity with wrong answer
- [ ] Verified feedback shows "Incorrect"
- [ ] Noted any issues: _______________

**✓ Checkpoint 3:** Can submit answers and receive feedback

---

## Phase 2: Integration Part 3 - Session End (1 hour)

### Update End Session Function
- [ ] Located `_end_session()` in SessionManager.gd

**Original Code (BEFORE):**
```gdscript
func _end_session():
    var summary = MockBackend.end_session(current_session_id)
    session_ended.emit(summary)
```

**New Code (AFTER):**
```gdscript
func _end_session():
    # Show loading
    emit_signal("loading_started")
    
    # End session on backend
    var response = await PlaycademySdk.backend.request(
        "POST",
        "/session/end",
        {"session_id": current_session_id}
    )
    
    # Hide loading
    emit_signal("loading_ended")
    
    # Handle errors
    if response.error:
        _handle_api_error(response.error)
        # Still show results with partial data
        var partial_summary = {
            "total_activities": activity_queue.size(),
            "correct_count": 0,  # Estimate from local data
            "accuracy": 0.0
        }
        session_ended.emit(partial_summary)
        return
    
    # Process successful response
    session_ended.emit(response)
```

### Implementation Steps
- [ ] Replaced MockBackend call with SDK call
- [ ] Added `await` keyword
- [ ] Added loading signals
- [ ] Added error handling with fallback
- [ ] Saved file
- [ ] No syntax errors

### Test Session Completion
- [ ] Ran game (F5)
- [ ] Started session
- [ ] Completed all activities
- [ ] Verified results screen shows
- [ ] Verified score displays correctly
- [ ] Noted accuracy percentage: _____%

**✓ Checkpoint 4:** Complete session flow working

---

## Phase 2: Integration Part 4 - Progress Retrieval (30 minutes)

### Update Progress Screen
- [ ] Opened `scripts/ProgressScreen.gd`
- [ ] Located progress loading function (likely in `_ready()` or `update_progress()`)

**Original Code (BEFORE):**
```gdscript
func _ready():
    var progress = MockBackend.get_progress()
    _display_progress(progress)
```

**New Code (AFTER):**
```gdscript
func _ready():
    await _load_progress()

func _load_progress():
    # Show loading
    if has_signal("loading_started"):
        emit_signal("loading_started")
    
    # Get progress from backend
    var response = await PlaycademySdk.backend.request(
        "GET",
        "/progress",
        {"user_id": PlaycademySdk.user_id}
    )
    
    # Hide loading
    if has_signal("loading_ended"):
        emit_signal("loading_ended")
    
    # Handle errors
    if response.error:
        _show_error("Could not load progress. Please try again.")
        return
    
    # Display progress
    _display_progress(response)
```

### Implementation Steps
- [ ] Replaced MockBackend call with SDK call
- [ ] Added `await` keyword
- [ ] Made _ready() async by calling async function
- [ ] Added error handling
- [ ] Saved file
- [ ] No syntax errors

### Test Progress Display
- [ ] Ran game (F5)
- [ ] Completed a session
- [ ] Returned to main menu
- [ ] Clicked "View Progress"
- [ ] Verified progress data loads
- [ ] Completed another session
- [ ] Verified progress updated
- [ ] Noted total words practiced: _____

**✓ Checkpoint 5:** All backend integration complete!

---

## Phase 3: Polish & Features (2-4 hours)

### Add Loading Overlay (Optional but Recommended)
- [ ] Created new scene: `scenes/LoadingOverlay.tscn`
- [ ] Added components:
  - [ ] ColorRect (covers screen, semi-transparent)
  - [ ] CenterContainer
  - [ ] Label: "Loading..."
  - [ ] Optional: AnimatedSprite (spinner)
- [ ] Created script: `scripts/LoadingOverlay.gd`
- [ ] Added to GameSession as child
- [ ] Connected to SessionManager signals:
```gdscript
SessionManager.loading_started.connect(_show_loading)
SessionManager.loading_ended.connect(_hide_loading)
```

### Implement Audio Playback
- [ ] Opened `scripts/activities/Spelling.gd`
- [ ] Added AudioStreamPlayer node
- [ ] Implemented audio loading:
```gdscript
var audio_player: AudioStreamPlayer
var audio_loaded = false

func _ready():
    audio_player = AudioStreamPlayer.new()
    add_child(audio_player)
    
    if activity_data.word.has("audio_url"):
        _load_audio(activity_data.word.audio_url)
    else:
        _show_text_fallback()

func _load_audio(url: String):
    var http = HTTPRequest.new()
    add_child(http)
    http.request_completed.connect(_on_audio_loaded)
    http.request(url)

func _on_audio_loaded(result, code, headers, body):
    if code == 200:
        var stream = AudioStreamMP3.new()
        stream.data = body
        audio_player.stream = stream
        audio_loaded = true
    else:
        _show_text_fallback()

func _on_play_button_pressed():
    if audio_loaded:
        audio_player.play()
```
- [ ] Added "Play Audio" button to Spelling.tscn
- [ ] Connected button to `_on_play_button_pressed`
- [ ] Added text fallback for missing audio
- [ ] Tested audio playback
- [ ] Tested without audio (fallback works)

### UI Color Updates (Match Playcademy Brand)
- [ ] Updated button colors:
  - Primary: #4F46E5 (indigo)
  - Hover: #4338CA
  - Pressed: #3730A3
- [ ] Updated feedback colors:
  - Correct: #10B981 (green)
  - Incorrect: #EF4444 (red)
- [ ] Updated background: #F9FAFB
- [ ] Updated text color: #1F2937
- [ ] Verified contrast ratios (accessibility)

### Add Button Hover States
- [ ] Added to all button scripts:
```gdscript
func _ready():
    mouse_entered.connect(_on_hover)
    mouse_exited.connect(_on_unhover)

func _on_hover():
    modulate = Color(1.1, 1.1, 1.1)

func _on_unhover():
    modulate = Color(1.0, 1.0, 1.0)
```

### Add Activity Transitions
- [ ] Opened `scripts/GameSession.gd`
- [ ] Added fade transitions:
```gdscript
func _change_activity():
    # Fade out
    var tween = create_tween()
    tween.tween_property(current_activity, "modulate:a", 0.0, 0.2)
    await tween.finished
    
    # Switch activity
    _load_next_activity()
    
    # Fade in
    var tween2 = create_tween()
    tween2.tween_property(current_activity, "modulate:a", 1.0, 0.2)
```

**✓ Checkpoint 6:** Game is polished and user-friendly

---

## Phase 4: Testing (2-3 hours)

### Activity Testing
- [ ] **Flashcard:** Displays word, definition, example
- [ ] **Multiple Choice:** Can select answer
- [ ] **Multiple Choice:** Correct answer works
- [ ] **Multiple Choice:** Incorrect answer shows feedback
- [ ] **Spelling:** Can type word
- [ ] **Spelling:** Correct spelling accepted
- [ ] **Spelling:** Incorrect spelling rejected
- [ ] **Spelling:** Audio plays (if implemented)
- [ ] **Fill Blank:** Shows sentence with blank
- [ ] **Fill Blank:** Correct word accepted
- [ ] **Fill Blank:** Incorrect word rejected
- [ ] **Synonym/Antonym:** Shows options
- [ ] **Synonym/Antonym:** Correct choice works
- [ ] **Synonym/Antonym:** Incorrect choice shows feedback

### Flow Testing
- [ ] Can start session from main menu
- [ ] Session loads with real vocabulary
- [ ] Can complete all activities in sequence
- [ ] Feedback shows after each activity
- [ ] Results screen appears at end
- [ ] Score calculated correctly
- [ ] Can start new session from results
- [ ] Can view progress from main menu
- [ ] Progress accumulates across sessions

### Error Testing
- [ ] Tested with network disconnected
  - Shows error message: ✓ / ✗
  - Error message is user-friendly: ✓ / ✗
- [ ] Tested with slow network (throttle in dev tools)
  - Loading indicator shows: ✓ / ✗
  - Request eventually completes or times out: ✓ / ✗
- [ ] Tested with invalid credentials
  - Shows auth error: ✓ / ✗
  - Prompts to log in: ✓ / ✗

### Performance Testing
- [ ] Opened Performance Monitor (Debug > Monitor)
- [ ] Completed full session
- [ ] Recorded metrics:
  - FPS: _____ (target: >30)
  - Memory: _____ MB (should be stable)
  - Scene transitions: _____ ms (target: <500ms)
- [ ] No performance issues identified

### Cross-Browser Testing
- [ ] **Chrome:**
  - Game loads: ✓ / ✗
  - All activities work: ✓ / ✗
  - Audio works: ✓ / ✗ / N/A
  - Notes: _______________

- [ ] **Firefox:**
  - Game loads: ✓ / ✗
  - All activities work: ✓ / ✗
  - Audio works: ✓ / ✗ / N/A
  - Notes: _______________

- [ ] **Safari:**
  - Game loads: ✓ / ✗
  - All activities work: ✓ / ✗
  - Audio works: ✓ / ✗ / N/A
  - Notes: _______________

- [ ] **Edge:**
  - Game loads: ✓ / ✗
  - All activities work: ✓ / ✗
  - Audio works: ✓ / ✗ / N/A
  - Notes: _______________

### Bug Tracking
**Bugs Found:**
1. _______________________________________________
2. _______________________________________________
3. _______________________________________________

**Bugs Fixed:**
1. _______________________________________________
2. _______________________________________________
3. _______________________________________________

**Known Issues (If Any):**
1. _______________________________________________
2. _______________________________________________

**✓ Checkpoint 7:** All testing complete, major bugs fixed

---

## Phase 5: Deployment (2-3 hours)

### Configure Web Export
- [ ] Opened Project > Export in Godot
- [ ] Found "Web (Runnable)" preset
- [ ] If not present: Clicked "Add..." > "Web"
- [ ] Configured settings:
  - [ ] Export Path: `./export/index.html`
  - [ ] Custom HTML Shell: `res://addons/playcademy/shell.html`
  - [ ] Encryption: Off (for staging)
- [ ] Clicked "Export Project"
- [ ] Verified export folder created
- [ ] Checked files present:
  - [ ] index.html
  - [ ] index.js
  - [ ] index.wasm
  - [ ] index.pck

### Verify Manifest
- [ ] Found `playcademy-vocab.zip` in project root
- [ ] Created by Manifest Exporter plugin automatically
- [ ] If not present, checked plugin is enabled

### Deploy to Staging
- [ ] Opened terminal in project directory
- [ ] Ran: `playcademy login`
- [ ] Entered credentials
- [ ] Verified login: `playcademy status`
- [ ] Ran: `playcademy deploy --env staging`
- [ ] Waited for deployment to complete
- [ ] Noted staging URL: _______________
- [ ] Deployment successful: ✓ / ✗

### Test Deployed Version
- [ ] Opened staging URL in browser
- [ ] Cleared browser cache (Ctrl+Shift+Delete)
- [ ] Refreshed page
- [ ] Game loads without errors
- [ ] Completed one full session
- [ ] Verified all activities work
- [ ] Checked browser console (no errors)
- [ ] Verified authentication works
- [ ] Tested on mobile device (optional)

### Deploy to Production (After Approval)
- [ ] Got approval to deploy to production
- [ ] Ran: `playcademy deploy --env production`
- [ ] Noted production URL: _______________
- [ ] Tested production version
- [ ] Verified everything works
- [ ] Production deployment successful: ✓ / ✗

**✓ Checkpoint 8:** Successfully deployed to Playcademy!

---

## Phase 6: Documentation (1-2 hours)

### Update README
- [ ] Opened `README.md`
- [ ] Added "Playcademy Integration" section
- [ ] Documented SDK installation steps
- [ ] Documented deployment process
- [ ] Added troubleshooting section
- [ ] Updated screenshots (if applicable)
- [ ] Saved and committed

### Create Integration Guide
- [ ] Created `INTEGRATION.md`
- [ ] Documented integration patterns used
- [ ] Included code examples
- [ ] Listed all files modified
- [ ] Added migration notes from mock to real API
- [ ] Saved and committed

### Create Deployment Guide
- [ ] Created `DEPLOYMENT.md`
- [ ] Documented export configuration
- [ ] Documented CLI commands
- [ ] Added troubleshooting tips
- [ ] Saved and committed

### Write Feedback Report
- [ ] Created `FEEDBACK.md`
- [ ] Included sections:
  - [ ] Executive Summary
  - [ ] Overall Experience (rating 1-10): ___
  - [ ] Documentation Review
  - [ ] SDK & API Feedback
  - [ ] Development Experience
  - [ ] Timeline & Effort (actual vs. estimated)
  - [ ] Recommendations
- [ ] Provided constructive feedback
- [ ] Highlighted what worked well
- [ ] Suggested improvements
- [ ] Saved and committed

### Code Cleanup
- [ ] Removed debug `print()` statements
- [ ] Removed commented-out code
- [ ] Added comments to complex sections
- [ ] Verified no TODOs left unfixed
- [ ] Ran Godot error checker (Output panel)
- [ ] Fixed any warnings

### Git Submission
- [ ] Staged all changes: `git add .`
- [ ] Committed: `git commit -m "Complete Playcademy backend integration"`
- [ ] Created feature branch: `git checkout -b feature/playcademy-integration`
- [ ] Pushed to remote: `git push origin feature/playcademy-integration`
- [ ] Created Pull Request on GitHub
- [ ] Set target branch: `dev`
- [ ] Added detailed PR description
- [ ] Attached feedback report
- [ ] Requested review

**✓ Checkpoint 9:** All documentation complete and submitted

---

## Final Verification

### Pre-Submission Checklist
- [ ] All 5 activities work correctly
- [ ] Complete session flow functions
- [ ] Deployed to staging (URL: _______________)
- [ ] Tested on multiple browsers
- [ ] No critical bugs
- [ ] Documentation complete
- [ ] Feedback report written
- [ ] Code pushed to GitHub dev branch
- [ ] Pull request created

### Success Criteria Met
- [ ] **Functional:** All activities work with real backend
- [ ] **Deployed:** Accessible on Playcademy platform
- [ ] **Complete:** Full session flow operational
- [ ] **Documented:** Feedback report and guides complete
- [ ] **Timeline:** Completed within 3-4 days
- [ ] **Quality:** No major bugs, good user experience

**✓ PROJECT COMPLETE!** 🎉

---

## Time Tracking

Record your actual time spent on each phase:

| Phase | Estimated | Actual |
|-------|-----------|--------|
| Setup & SDK Installation | 1h | ___ |
| Session Start Integration | 1h | ___ |
| Answer Submit Integration | 1.5h | ___ |
| Session End Integration | 1h | ___ |
| Progress Integration | 0.5h | ___ |
| Audio Implementation | 1.5h | ___ |
| UI Polish | 1.5h | ___ |
| Loading States | 1h | ___ |
| Testing | 3h | ___ |
| Deployment | 3h | ___ |
| Documentation | 2h | ___ |
| Bug Fixes | 2h | ___ |
| **Total** | **20h** | **___** |

---

## Notes & Lessons Learned

### What Went Well
1. _______________________________________________
2. _______________________________________________
3. _______________________________________________

### Challenges Faced
1. _______________________________________________
2. _______________________________________________
3. _______________________________________________

### Solutions Found
1. _______________________________________________
2. _______________________________________________
3. _______________________________________________

### Tips for Future Developers
1. _______________________________________________
2. _______________________________________________
3. _______________________________________________

---

## Post-Completion Tasks

- [ ] Responded to PR feedback
- [ ] Made requested changes
- [ ] Merged PR to dev branch
- [ ] Verified merge successful
- [ ] Deleted feature branch (optional)
- [ ] Celebrated completion! 🎉

---

**Congratulations on completing the Playcademy Vocab integration!**
