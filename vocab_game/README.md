# Playcademy Vocab - Godot Implementation

A vocabulary learning game built with Godot 4.x for elementary students (grades 3-5), implementing spaced repetition learning through engaging activities.

## Project Structure

```
vocab_game/
├── project.godot           # Main Godot project configuration
├── icon.svg               # Project icon
├── scripts/
│   ├── MockBackend.gd     # Simulates REST API responses
│   ├── SessionManager.gd  # Coordinates activity flow
│   ├── MainMenu.gd        # Main menu controller
│   ├── GameSession.gd     # Game session controller
│   ├── ProgressScreen.gd  # Progress display
│   ├── ResultsScreen.gd   # Session results
│   └── activities/
│       ├── Flashcard.gd   # Flashcard introduction activity
│       ├── MultipleChoice.gd # Definition matching
│       ├── Spelling.gd    # Dictated spelling
│       ├── FillBlank.gd   # Fill-in-the-blank
│       └── SynonymAntonym.gd # Synonym/antonym selection
└── scenes/
    ├── MainMenu.tscn      # Main menu scene
    ├── GameSession.tscn   # Game session scene
    ├── ProgressScreen.tscn # Progress screen
    ├── ResultsScreen.tscn # Results screen
    └── activities/
        ├── Flashcard.tscn
        ├── MultipleChoice.tscn
        ├── Spelling.tscn
        ├── FillBlank.tscn
        └── SynonymAntonym.tscn
```

## Features

### Core Activities (All Implemented)

1. **Flashcard Introduction** - Learn new words with definitions and example sentences
2. **Multiple Choice** - Match words to their correct definitions
3. **Spelling** - Type the word you hear (audio playback simulated)
4. **Fill in the Blank** - Complete sentences with the correct vocabulary word
5. **Synonym/Antonym Selection** - Choose the correct synonym or antonym

### System Features

- **Mock Backend Service** - Simulates REST API endpoints:
  - `POST /session/start` - Generates a session with mixed activities
  - `POST /session/attempt` - Scores student answers
  - `POST /session/end` - Finalizes session and updates progress
  - `GET /progress` - Returns learning statistics

- **Session Management** - Coordinates activity flow with signals
- **Progress Tracking** - Tracks words learned, accuracy, and mastery levels
- **Age-Appropriate UI** - Large fonts, bright colors, encouraging feedback

### Mock Data

The MockBackend includes 10 sample vocabulary words with:
- Word and definition
- Example sentence
- Synonyms and antonyms
- Simulated audio file references

## How It Works

### Game Flow

1. **Main Menu** → User clicks "Start Session"
2. **Session Start** → MockBackend generates 5-8 words with 2-3 activities each (~15-20 activities total)
3. **Activity Loop** → Student completes each activity in sequence:
   - Activity is loaded dynamically
   - Student submits answer
   - Immediate feedback (correct/incorrect)
   - Advances to next activity
4. **Session End** → Shows results and updates progress
5. **Results Screen** → Option to continue or return to menu

### Architecture

#### AutoLoad Singletons
- **MockBackend** - Manages vocabulary data and simulated API responses
- **SessionManager** - Coordinates activity flow using signals

#### Signal Flow
```
SessionManager signals:
- session_started(session_data)
- activity_changed(activity_data, index, total)
- attempt_result(correct, feedback)
- session_ended(summary)

Activity signals:
- answer_submitted(answer)
```

### Mock Backend Details

The backend simulates real API behavior:

**Session Generation:**
- Randomly selects 5-8 words from the vocabulary bank
- Creates 2-3 activities per word
- Shuffles activities for variety
- Returns activity queue with all necessary data

**Answer Scoring:**
- Multiple Choice: Exact match with definition
- Spelling: Case-insensitive word match
- Fill in Blank: Case-insensitive word match
- Synonym/Antonym: Checks against word's synonym/antonym lists
- Flashcard: Always correct (acknowledgment only)

**Progress Tracking:**
- Counts unique words practiced
- Tracks total attempts and correct attempts
- Calculates accuracy percentage
- Maintains mastery levels per word

## Getting Started

### Prerequisites
- Godot 4.x (tested with 4.3)

### Installation

1. Open Godot Editor
2. Click "Import" and select the `project.godot` file
3. The project will load with all scenes and scripts

### Running the Game

1. Press F5 or click the Play button in Godot
2. Click "Start Session" to begin a vocabulary session
3. Complete the activities
4. View your progress from the main menu

## Customization

### Adding More Words

Edit `scripts/MockBackend.gd` and add entries to the `vocabulary_bank` dictionary:

```gdscript
{
    "id": "w011",
    "word": "brilliant",
    "definition": "exceptionally clever or talented",
    "example": "She had a brilliant idea.",
    "synonyms": ["smart", "bright", "intelligent"],
    "antonyms": ["dull", "stupid", "dim"],
    "audio_url": "res://audio/brilliant.wav"
}
```

### Adjusting Activity Mix

In `MockBackend.gd`, modify the `start_session()` function to change:
- Number of words per session (currently 5-8)
- Number of activities per word (currently 2-3)
- Activity type distribution

### Styling

All UI elements use Godot's theme system. To customize:
1. Adjust colors in the scene `.tscn` files
2. Modify font sizes in theme_override properties
3. Change layouts in the UI containers

## Future Enhancements

When integrating with real backend:

1. **Replace MockBackend** - Update `SessionManager` to use actual `PlaycademySdk.backend.request()` calls
2. **Audio Playback** - Implement real audio playback in Spelling activity
3. **Sentence Generation** - Add 6th activity type for creative sentence writing
4. **Animations** - Add transitions between activities
5. **Sound Effects** - Add feedback sounds for correct/incorrect answers
6. **Adaptive Difficulty** - Implement FSRS-based word selection
7. **Multiple Grade Levels** - Add grade selection in main menu

## Testing

The mock backend makes testing easy:
- No network required
- Predictable responses
- Adjustable difficulty
- Fast iteration

## Notes for Playcademy Integration

Current implementation uses:
- Manual scene loading via `get_tree().change_scene_to_file()`
- No PlaycademySDK calls (mocked instead)
- No deployment configuration

To integrate with Playcademy platform:
1. Replace MockBackend calls with PlaycademySDK.backend.request()
2. Add Playcademy Manifest Exporter plugin
3. Configure web export with custom shell
4. Add authentication flow
5. Implement proper error handling for network requests

## License

Built for Playcademy Gauntlet Cohort 3
