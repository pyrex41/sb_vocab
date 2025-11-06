# Quick Start Guide

## Opening the Project

1. Launch Godot 4.x Editor
2. Click "Import"
3. Navigate to the `vocab_game` folder
4. Select `project.godot`
5. Click "Import & Edit"

## Running the Game

### Option 1: Play from Editor
1. Press **F5** (or click the Play button ▶️ in top-right)
2. The game will launch in a new window

### Option 2: Play Current Scene
1. Open any scene in `scenes/` folder
2. Press **F6** to test that specific scene

## Testing the Game

### Main Menu
- Click "Start Session" → Launches game session
- Click "View Progress" → Shows learning statistics

### During Session
1. Each activity loads automatically
2. Complete the activity (answer question, click button, etc.)
3. Feedback appears (green for correct, red for incorrect)
4. Click "Continue" to advance to next activity
5. Session automatically ends after all activities

### Activity Types You'll See:

1. **Flashcard** - Just read and click "Got it!"
2. **Multiple Choice** - Click the correct definition
3. **Spelling** - Type the word (click Play Audio for hint)
4. **Fill in Blank** - Type the missing word
5. **Synonym/Antonym** - Choose the correct synonym or antonym

## Checking the Mock Data

To see what words are available:
1. Open `scripts/MockBackend.gd`
2. Look at the `vocabulary_bank` dictionary
3. Currently has 10 words: abundant, curious, delicate, enormous, fierce, graceful, humble, swift, tranquil, valiant

## Making Changes

### Add More Words
Edit `scripts/MockBackend.gd` → Add to `vocabulary_bank`

### Change UI Colors
Edit any `.tscn` file → Modify ColorRect colors or theme_override properties

### Adjust Session Length
Edit `scripts/MockBackend.gd` → Line 100: `var num_words = min(randi() % 4 + 5, words.size())`
- Change `5` to adjust minimum words
- Change `4` to adjust range

### Debug Mode
In Godot Editor:
- Open "Output" panel at bottom to see print statements
- MockBackend prints "Mock Backend initialized" on startup
- Add more `print()` statements to debug

## Common Issues

### Scene Not Found Error
- Make sure all .tscn files are in `scenes/` and `scenes/activities/` folders
- Check that script paths in .tscn files match actual file locations

### Script Error
- Check for typos in script files
- Make sure all @onready variables match node names in scenes

### UI Too Small/Large
- Edit .tscn files and adjust `theme_override_font_sizes/font_size` values
- Modify `custom_minimum_size` on buttons and containers

## Project Structure at a Glance

```
scenes/MainMenu.tscn → Main entry point
    ↓ Start Session
scenes/GameSession.tscn → Loads activities dynamically
    ↓ Activity complete
scenes/activities/*.tscn → Individual activity types
    ↓ All done
scenes/ResultsScreen.tscn → Show results
```

## Signals Flow

```
User clicks "Start Session"
    → SessionManager.start_new_session()
    → MockBackend.start_session()
    → SessionManager emits 'session_started'
    → SessionManager emits 'activity_changed'
    → GameSession loads activity scene
    
User submits answer
    → Activity emits 'answer_submitted'
    → SessionManager.submit_answer()
    → MockBackend.submit_attempt()
    → SessionManager emits 'attempt_result'
    → GameSession shows feedback
    → Auto-advance to next activity
    
All activities complete
    → SessionManager._end_session()
    → MockBackend.end_session()
    → SessionManager emits 'session_ended'
    → Navigate to ResultsScreen
```

## Next Steps

Once comfortable with the mock version:
1. Review Playcademy SDK documentation
2. Replace MockBackend calls with PlaycademySdk.backend.request()
3. Add proper error handling
4. Implement audio playback
5. Add animations and polish
6. Test deployment with `playcademy deploy`

Enjoy building! 🎮
