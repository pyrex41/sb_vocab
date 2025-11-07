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

## Backend Integration

The application is now integrated with Playcademy's backend API:
- Vocabulary words are managed by the backend
- Session generation uses FSRS algorithm for optimal learning
- Progress is tracked persistently across sessions

## Making Changes

### Change UI Colors
Edit any `.tscn` file → Modify ColorRect colors or theme_override properties

### Customize Activities
Edit activity scripts in `scripts/activities/` to modify behavior

### Debug Mode
In Godot Editor:
- Open "Output" panel at bottom to see print statements
- SessionManager logs API calls and responses
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
    → SessionManager emits 'loading_started'
    → PlaycademySDK.backend.request('/session/start')
    → SessionManager emits 'loading_ended'
    → SessionManager emits 'session_started'
    → SessionManager emits 'activity_changed'
    → GameSession loads activity scene

User submits answer
    → Activity emits 'answer_submitted'
    → SessionManager.submit_answer()
    → SessionManager emits 'loading_started'
    → PlaycademySDK.backend.request('/session/attempt')
    → SessionManager emits 'loading_ended'
    → SessionManager emits 'attempt_result'
    → GameSession shows feedback
    → Auto-advance to next activity

All activities complete
    → SessionManager._end_session()
    → SessionManager emits 'loading_started'
    → PlaycademySDK.backend.request('/session/end')
    → SessionManager emits 'loading_ended'
    → SessionManager emits 'session_ended'
    → Navigate to ResultsScreen
```

## Deployment

The application is ready for deployment to Playcademy platform:
1. Ensure PlaycademySDK is installed and configured
2. Configure web export with Playcademy HTML shell
3. Test in staging environment
4. Deploy to production with `playcademy deploy`

Enjoy building! 🎮
