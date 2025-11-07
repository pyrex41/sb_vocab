# Getting Started with Playcademy Vocab

## Project Overview

**Playcademy Vocab** is a vocabulary learning application for elementary students (grades 3-5) that uses spaced repetition learning (FSRS scheduling) to help students master new vocabulary words through interactive activities.

## Architecture

The project is organized into three main components:

### 1. Frontend (Godot)
Located in `/godot/` - The main game client built with Godot engine and GDScript.

**Key Components:**
- `PlaycademySDK.gd` - Main SDK interface (autoload)
- `backend.gd` - Mock backend implementation for development
- Scene files (`.tscn`) - UI layouts for each screen
- Script files (`.gd`) - Game logic and interactions

### 2. Backend (Mock)
Located in `/backend/` - Mock API responses and data structures.

**Files:**
- `mock_api.json` - Vocabulary words and activity definitions

### 3. Documentation
Located in `/docs/` - Project documentation and guides.

## Project Flow

```
Main Menu
├── Start Session
│   ├── Select Grade (3, 4, or 5)
│   └── Session Activity Loop
│       ├── Flashcard Activity
│       ├── Multiple Choice Activity
│       └── ... (repeat for each word)
├── View Progress
└── Settings
```

## Current Implementation Status

### Completed
- Main menu navigation
- Grade level selection
- Session start/end flow
- Mock backend with API responses
- Two activity types implemented:
  - Flashcard (presentation mode)
  - Multiple choice (recognition)
- Progress tracking display
- Settings screen

### In Progress
- Additional activity implementations
- UI/UX enhancements

### To Do
- Spelling/dictation activity
- Fill-in-the-blank activity
- Synonym/antonym matching
- Sentence generation
- Audio pronunciation
- Timer management
- Better styling and animations
- Real backend integration

## Running the Project

### Prerequisites
- Godot Engine 4.x installed
- This project directory

### Steps
1. Open Godot Engine
2. Click "Open Project" and navigate to `/tmp/cc-agent/59762058/project/godot/`
3. Click "Run Project" (F5) or the play button
4. Interact with the game:
   - Click "Start Session"
   - Select a grade level (3, 4, or 5)
   - Complete the activities
   - View results

## Development Guide

### Adding a New Activity

Activities are loaded in `session_activity.gd` in the `_load_*` functions. To add a new activity:

1. Create a new function in `session_activity.gd`:
```gdscript
func _load_new_activity(activity: Dictionary):
    var label = Label.new()
    label.text = "Your activity content"
    $ActivityContainer.add_child(label)

    var answer_button = Button.new()
    answer_button.text = "Answer"
    answer_button.pressed.connect(_on_activity_complete.bindv([activity, "user_answer"]))
    $AnswerContainer.add_child(answer_button)
```

2. Add a case in the `match` statement in `load_next_activity()`:
```gdscript
"new_activity_type":
    _load_new_activity(activity)
```

### Backend Integration

The mock backend can be replaced with real API calls. The interface is in `PlaycademySDK.gd`:

```gdscript
func start_session(grade: int) -> Dictionary
func attempt_activity(activity_type: String, user_answer: String, word_id: String) -> Dictionary
func end_session() -> Dictionary
func get_progress() -> Dictionary
```

## Project Structure Details

```
project/
├── godot/
│   ├── project.godot                    # Godot project configuration
│   ├── scenes/                          # All game scenes (.tscn files)
│   ├── scripts/                         # All game logic (.gd files)
│   ├── addons/
│   │   └── playcademy_sdk/
│   │       ├── PlaycademySDK.gd         # Main SDK autoload
│   │       └── backend.gd              # Mock backend
│   └── README.md
├── backend/
│   └── mock_api.json                   # Mock vocabulary data
├── docs/                               # Documentation (to be added)
├── .env                                # Environment configuration
├── .gitignore
├── README.md
└── GETTING_STARTED.md                  # This file
```

## Environment Variables

The `.env` file contains:
```
PLAYCADEMY_API_URL=http://localhost:8000
PLAYCADEMY_API_KEY=mock_key_12345
ENVIRONMENT=development
```

These are currently set for mock backend development. Update when integrating with real API.

## Next Steps

1. **Enhance UI**: Add better styling, colors, and animations
2. **Implement More Activities**: Add spelling, fill-blank, synonym, and sentence activities
3. **Add Audio Support**: Integrate text-to-speech or recorded audio
4. **Backend Integration**: Connect to real Playcademy API
5. **Data Persistence**: Store user progress and word mastery locally
6. **Expand Vocabulary**: Add more words to the mock data
7. **FSRS Integration**: Implement spaced repetition scheduling
8. **Testing**: Create test scenes and validate all features

## Troubleshooting

**Q: Scenes won't load**
A: Make sure you're opening the project from the `godot/` directory in Godot Editor.

**Q: Autoload not working**
A: Check that `PlaycademySDK` is properly added in Project Settings > Autoload tab.

**Q: Mock backend not responding**
A: The backend is synchronous in development. Check `backend.gd` for the mock data.

## Resources

- [Godot Documentation](https://docs.godotengine.org/)
- [GDScript Reference](https://docs.godotengine.org/en/stable/getting_started/scripting/gdscript/index.html)
- [Playcademy Documentation](https://docs.playcademy.net)
