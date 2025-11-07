# Playcademy Vocab - Godot Implementation

A vocabulary learning game built with Godot 4.x for elementary students (grades 3-5), implementing spaced repetition learning through engaging activities.

## Project Structure

```
vocab_game/
├── project.godot           # Main Godot project configuration
├── icon.svg               # Project icon
├── scripts/
│   ├── SessionManager.gd  # Coordinates activity flow (integrated with PlaycademySDK)
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

- **PlaycademySDK Integration** - Connected to real backend API:
  - `POST /session/start` - Generates a session with mixed activities
  - `POST /session/attempt` - Scores student answers
  - `POST /session/end` - Finalizes session and updates progress
  - `GET /progress` - Returns learning statistics

- **Session Management** - Coordinates activity flow with signals
- **Progress Tracking** - Tracks words learned, accuracy, and mastery levels using FSRS algorithm
- **Age-Appropriate UI** - Large fonts, bright colors, encouraging feedback
- **Loading States** - Visual feedback during API calls with input disabling
- **Error Handling** - User-friendly error messages for network issues

## How It Works

### Game Flow

1. **Main Menu** → User clicks "Start Session"
2. **Session Start** → Backend generates 5-10 words with 2-3 activities each using FSRS algorithm
3. **Activity Loop** → Student completes each activity in sequence:
   - Activity is loaded dynamically
   - Student submits answer
   - Backend validates and scores the answer
   - Immediate feedback (correct/incorrect)
   - Advances to next activity
4. **Session End** → Shows results and updates progress via backend
5. **Results Screen** → Option to continue or return to menu

### Architecture

#### AutoLoad Singletons
- **SessionManager** - Coordinates activity flow using signals and PlaycademySDK integration

#### Signal Flow
```
SessionManager signals:
- session_started(session_data)
- activity_changed(activity_data, index, total)
- attempt_result(correct, feedback)
- session_ended(summary)
- loading_started()
- loading_ended()
- api_error(message)

Activity signals:
- answer_submitted(answer)
```

### Backend Integration

The application integrates with Playcademy's backend API via PlaycademySDK:

**Session Generation:**
- Backend uses FSRS (Free Spaced Repetition Scheduler) algorithm
- Selects optimal words based on student progress
- Creates 2-3 activities per word
- Returns activity queue with all necessary data

**Answer Scoring:**
- All scoring happens server-side
- Immediate feedback with correct/incorrect status
- Detailed feedback messages for learning

**Progress Tracking:**
- FSRS-based spaced repetition algorithm
- Tracks words learned, accuracy, and mastery levels
- Persistent progress across sessions
- Optimized review scheduling

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

Activity selection is now controlled by the backend's FSRS algorithm which optimizes:
- Number of words per session based on student performance
- Number of activities per word for optimal retention
- Activity type distribution based on learning progress

### Styling

All UI elements use Godot's theme system. To customize:
1. Adjust colors in the scene `.tscn` files
2. Modify font sizes in theme_override properties
3. Change layouts in the UI containers

## Integration Status

✅ **Completed:**

### Core Integration
1. **Backend Integration** - SessionManager uses PlaycademySDK.backend.request() for all API calls
2. **FSRS Algorithm** - Backend-powered spaced repetition for optimal learning
3. **Progress Tracking** - Persistent progress tracking across sessions via `/progress` endpoint

### Error Handling & Resilience
4. **Authentication Checks** - All API calls validate authentication state before execution
5. **API Response Validation** - Comprehensive validation of all API responses with required field checking
6. **Error Recovery** - Retry logic for failed operations with user-friendly retry button
7. **Graceful Degradation** - Activities handle missing/invalid data with skip options
8. **User-Friendly Messages** - Clear error messages for network, timeout, and auth errors

### User Experience
9. **Loading States** - Visual overlay with CanvasLayer for guaranteed top-level rendering
10. **Input Disabling** - Inputs disabled during API calls to prevent double-submission
11. **State Management** - Consistent session state even after API failures
12. **Retry Functionality** - Users can retry failed operations with a single click

### Code Quality
13. **Response Validation** - Helper functions validate all API response structures
14. **Option Validation** - Activities validate required data before rendering
15. **Error Logging** - Comprehensive error logging for debugging
16. **Documentation** - README, QUICKSTART, and CODE_REVIEW_FIXES.md all updated

See `CODE_REVIEW_FIXES.md` in the project root for detailed information about all improvements.

## Future Enhancements

Potential improvements:

1. **Audio Playback** - Implement real audio playback in Spelling activity (currently simulated)
2. **Sentence Generation** - Add 6th activity type for creative sentence writing
3. **Animations** - Add transitions between activities
4. **Sound Effects** - Add feedback sounds for correct/incorrect answers
5. **Grade Selection** - Add grade level picker in main menu
6. **Achievement System** - Add badges and rewards for milestones

## Deployment

The application is configured for deployment to Playcademy platform:
- Web export configured with Playcademy HTML shell
- PlaycademySDK integration for authentication and API access
- Error handling and loading states for production use
- Ready for staging and production deployment

## License

Built for Playcademy Gauntlet Cohort 3
