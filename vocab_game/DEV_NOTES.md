# Development Notes

## Architecture Decisions

### Why AutoLoad Singletons?

**MockBackend** and **SessionManager** are AutoLoad singletons because:
1. They need to persist across scene changes
2. Multiple scenes need to access the same session state
3. Godot's AutoLoad provides easy global access without tight coupling
4. Makes testing and swapping implementations easier

### Signal-Based Communication

Using signals instead of direct function calls provides:
- **Loose coupling** - Activities don't need to know about GameSession
- **Flexibility** - Easy to add new listeners
- **Godot best practice** - Signals are the recommended way to communicate between nodes
- **Debuggability** - Can inspect signal connections in editor

### Dynamic Scene Loading

Activities are loaded dynamically using `preload()` and `instantiate()`:
- **Pros**: Clean separation of concerns, reusable activity components
- **Cons**: Slight complexity, requires careful signal management
- **Alternative considered**: Single scene with all activities (rejected - too monolithic)

### Mock Backend Design

The mock backend closely mirrors the real REST API structure:
- Same response format (success flag, data payload)
- Same endpoint names (start_session, submit_attempt, etc.)
- Same data structures (session_id, activities array)
- Makes future integration seamless - just swap the implementation

## Design Patterns Used

### Strategy Pattern
Each activity type (Flashcard, MultipleChoice, etc.) implements the same interface:
- `setup(activity_data)` - Configure the activity
- `answer_submitted` signal - Report user input

This allows GameSession to treat all activities uniformly.

### Observer Pattern
Signals implement the observer pattern:
- SessionManager is the subject
- GameSession and activities are observers
- Decouples components while maintaining communication

### State Machine (Implicit)
SessionManager maintains session state:
- **Idle** - No active session
- **Active** - Session running, processing activities
- **Complete** - Session ended
Managed via `session_active` boolean

## Key Implementation Details

### Activity Queue Generation
```gdscript
# In MockBackend.start_session()
1. Select 5-8 random words
2. Generate 2-3 activities per word
3. Shuffle the activity queue
4. Return to SessionManager
```

This creates a natural spaced repetition effect - same word appears multiple times in different contexts.

### Answer Validation
Each activity type has specific validation logic:
- **Multiple Choice**: Exact string match with definition
- **Spelling**: Case-insensitive match
- **Fill Blank**: Case-insensitive match, handles variations
- **Synonym/Antonym**: Checks against arrays
- **Flashcard**: Always correct (acknowledgment only)

### Progress Tracking
```gdscript
# Accumulated across all sessions
- words_learned: Count of unique word IDs encountered
- total_attempts: Total answers submitted
- correct_attempts: Correct answers only
- mastery_levels: Dictionary of word_id → practice count
```

## UI/UX Considerations

### Elementary Student Focus
- **Large text**: 24-56pt fonts throughout
- **Bright colors**: High contrast, engaging palette
- **Immediate feedback**: Green/red color coding
- **Positive reinforcement**: Encouraging messages
- **Simple navigation**: Minimal clicks required

### Accessibility
- Clear visual hierarchy
- High contrast ratios
- Large touch targets (60px+ buttons)
- Simple language in instructions

## Performance Notes

### Memory Management
- Activities are properly freed using `queue_free()`
- Scenes are preloaded at GameSession start (avoids runtime loading)
- Vocabulary data stays in memory (minimal footprint ~10KB)

### Scene Transitions
Using `get_tree().change_scene_to_file()`:
- Cleans up previous scene
- Maintains AutoLoad singletons
- No memory leaks

## Testing Strategy

### Manual Testing Checklist
- [ ] Start session from main menu
- [ ] Complete all 5 activity types
- [ ] Verify feedback shows correctly
- [ ] Check progress updates
- [ ] Test session end flow
- [ ] Verify restart works
- [ ] Check progress persists

### Debug Tools
- Print statements in MockBackend
- Godot debugger for signal flow
- Scene tree inspector for node hierarchy

## Future Optimizations

### When Scaling to Real Backend

**API Integration**
```gdscript
# Replace:
var response = MockBackend.start_session()

# With:
var response = await PlaycademySdk.backend.request(
    "POST",
    "/session/start",
    {"grade": grade}
)
```

**Error Handling**
```gdscript
if not response or not response.success:
    # Show error dialog
    # Retry logic
    # Fallback to cached data
```

**Loading States**
- Add spinner while waiting for API
- Timeout handling
- Offline mode support

### Performance Improvements
1. **Texture atlasing** - Combine UI elements into single texture
2. **Object pooling** - Reuse button nodes instead of creating/destroying
3. **Lazy loading** - Load vocabulary data on-demand
4. **Caching** - Cache API responses

### Feature Additions
1. **Sentence Generation Activity** - 6th activity type with free-form input
2. **Daily Goals** - Track streaks and set targets
3. **Achievements** - Unlock badges for milestones
4. **Leaderboards** - Compare progress with classmates
5. **Parent Dashboard** - View child's progress
6. **Adaptive Difficulty** - FSRS-based word selection

## Known Limitations

### Current Implementation
- No audio playback (simulated in Spelling activity)
- Fixed vocabulary set (10 words hardcoded)
- No authentication
- No data persistence (resets on restart)
- No animation polish
- Basic UI styling

### Intentional Simplifications
- Single grade level (easy to extend)
- No user profiles
- No network code
- Synchronous operations (easy to make async later)

## Code Quality Notes

### What's Good
✅ Clear separation of concerns
✅ Consistent naming conventions
✅ Signal-based architecture
✅ Modular activity system
✅ Comprehensive comments

### What Could Improve
⚠️ Add input validation
⚠️ More error handling
⚠️ Unit tests for MockBackend
⚠️ Accessibility features (keyboard nav)
⚠️ More sophisticated animations

## Integration Checklist

When connecting to real Playcademy backend:

- [ ] Install PlaycademySDK from AssetLib
- [ ] Enable Playcademy plugins
- [ ] Add PlaycademySDK to AutoLoad
- [ ] Replace MockBackend calls with SDK requests
- [ ] Add authentication flow
- [ ] Implement error handling
- [ ] Add loading states
- [ ] Test with real vocabulary data
- [ ] Configure web export
- [ ] Test audio playback
- [ ] Set up deployment
- [ ] Add analytics/telemetry
- [ ] Test on target devices

## Estimated Timeline

**Phase 1: Core Gameplay** (2 days)
- ✅ Mock backend
- ✅ Session management
- ✅ 5 activity types
- ✅ Basic UI

**Phase 2: Polish** (1 day)
- [ ] Animations
- [ ] Sound effects
- [ ] Better UI styling
- [ ] Error messages

**Phase 3: Integration** (1 day)
- [ ] Replace mock with real API
- [ ] Authentication
- [ ] Deployment
- [ ] Testing

**Total: 3-4 days** (matching Gauntlet timeline)

## Lessons Learned

### What Worked Well
- Starting with mock backend allowed rapid iteration
- Signal architecture made components easy to test independently
- Scene-based activities were the right abstraction
- Godot's built-in UI system was sufficient

### What Was Challenging
- Balancing mock realism vs. simplicity
- Getting activity transitions smooth
- Managing state across scene changes
- Keeping UI age-appropriate yet functional

## Resources

- [Godot Signals Documentation](https://docs.godotengine.org/en/stable/getting_started/step_by_step/signals.html)
- [Godot AutoLoad Documentation](https://docs.godotengine.org/en/stable/tutorials/scripting/singletons_autoload.html)
- [Playcademy Docs](https://docs.playcademy.net)
- [FSRS Algorithm](https://github.com/open-spaced-repetition/ts-fsrs)

## Contact

For questions or issues with this implementation, refer to:
- README.md - Overview and setup
- QUICKSTART.md - Running and testing
- This file - Architecture and design decisions

---

Built for Playcademy Gauntlet Cohort 3
Target: Ship functional educational game in <1 week
Status: ✅ Core implementation complete
