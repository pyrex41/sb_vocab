# Playcademy Vocab - Godot Frontend

This is the frontend implementation for the Playcademy Vocabulary learning application built in Godot.

## Project Structure

```
godot/
├── project.godot          # Godot project configuration
├── scenes/                # Game scenes (UI screens, game flow)
│   ├── main_menu.tscn
│   ├── session_loader.tscn
│   ├── session_activity.tscn
│   ├── session_complete.tscn
│   ├── progress_screen.tscn
│   └── settings_screen.tscn
├── scripts/               # GDScript files
│   ├── main_menu.gd
│   ├── session_loader.gd
│   ├── session_activity.gd
│   ├── session_complete.gd
│   ├── progress_screen.gd
│   └── settings_screen.gd
└── addons/               # Playcademy SDK
    └── playcademy_sdk/
        ├── PlaycademySDK.gd  # Main SDK autoload
        └── backend.gd        # Mock backend implementation
```

## Features

### Implemented

- Main menu with navigation
- Grade level selection (3-5)
- Session activity system
- Flashcard activity type
- Multiple choice activity type
- Progress tracking
- Settings screen
- Mock backend with simulated API responses

### To Implement

- Spelling/dictation activity
- Fill-in-the-blank activity
- Synonym/antonym matching
- Sentence generation activity
- Audio playback for word pronunciation
- Timer system for sessions
- Better UI/UX design
- Sound effects and music
- Animations and transitions
- Data persistence
- Real backend integration

## Running the Project

1. Open the `godot/` directory in Godot Editor (Godot 4.x)
2. Click "Run Project" (F5) or press the play button
3. Start with "Start Session" to begin a vocabulary drill

## SDK Integration

The mock PlaycademySDK is located in `addons/playcademy_sdk/`. It provides:

```gdscript
# Start a session
var result = PlaycademySDK.start_session(grade: int) -> Dictionary

# Submit an activity attempt
var result = PlaycademySDK.attempt_activity(
    activity_type: String,
    user_answer: String,
    word_id: String
) -> Dictionary

# End the current session
var result = PlaycademySDK.end_session() -> Dictionary

# Get user progress
var result = PlaycademySDK.get_progress() -> Dictionary
```

## Activity Types

Each activity implements a specific learning pattern:

- **Flashcard**: Introduction and passive learning
- **Multiple Choice**: Recognition and comprehension
- **Spelling**: Recall and spelling practice
- **Fill Blank**: Contextual understanding
- **Synonym**: Vocabulary expansion
- **Sentence**: Active production

## Next Steps

1. Enhance the UI with better styling and animations
2. Implement remaining activity types
3. Add audio pronunciation support
4. Integrate real backend API
5. Add persistent data storage
6. Create more vocabulary words in the backend
7. Implement FSRS scheduling algorithm
