# PRD: Incorrect Answer Flow

**Feature:** Incorrect Answer Pedagogy & Flow
**Status:** Planning
**Priority:** P0 (Critical Path)
**Owner:** TBD
**Effort:** 4-6 hours
**Target Release:** Phase 5, Week 1

## Overview

Define and implement the pedagogical approach for handling incorrect answers in vocabulary activities. Current implementation shows generic "incorrect" feedback and immediately moves on, missing crucial learning opportunities.

## Problem Statement

When elementary students make mistakes:
1. **No learning moment:** Correct answer isn't shown or explained
2. **Missed retention:** Words aren't revisited for spaced repetition
3. **Confidence impact:** Generic "wrong" message can be discouraging
4. **No second chance:** Students don't get to try again

Research shows that immediate feedback with correction improves retention by 30-40% compared to simple wrong/right indicators.

## Goals

### Primary Goals
- **Learning Effectiveness:** Students learn from mistakes, not just move on
- **Confidence Building:** Encouraging feedback that doesn't feel like failure
- **Knowledge Retention:** Missed words are revisited for better memory encoding
- **Clear Pedagogy:** Teachers/parents understand the learning approach

### Success Metrics
- **Retention:** Students correctly answer missed words on second attempt > 70%
- **Engagement:** Session completion doesn't drop despite additional steps
- **Learning:** Post-session quiz shows 25%+ improvement on missed words
- **Satisfaction:** Student feedback rates "helpful when wrong" ≥ 4/5

## User Stories

### Student (Primary User)
```
As a 3rd grade student,
When I answer a vocabulary question incorrectly,
I want to understand why I was wrong and learn the right answer,
So that I can remember it next time and feel smarter.
```

### Teacher (Secondary User)
```
As an elementary teacher,
When my students make mistakes in the game,
I want them to receive pedagogically sound correction,
So that errors become learning opportunities, not discouragement.
```

### Parent (Tertiary User)
```
As a parent,
When my child plays this game at home,
I want mistakes to be handled constructively,
So that my child builds vocabulary without frustration.
```

## Proposed Solution

### Option A: Learn & Continue (Recommended)

**Flow:**
1. Student submits incorrect answer
2. Show correction panel:
   - "Not quite! The answer is **[correct answer]**"
   - Brief definition/context (if available from backend)
   - Encouraging message: "Let's keep going!"
3. Wait 2.5 seconds for absorption
4. Store word for end-of-session review
5. Continue to next activity

**Pros:**
- Simple, clear pedagogy
- Doesn't break flow
- Builds spaced repetition foundation
- Low implementation complexity

**Cons:**
- No immediate retry
- Relies on end-of-session review

### Option B: Second Chance with Hints

**Flow:**
1. Student submits incorrect answer (Attempt 1)
2. Show hint panel:
   - "Not quite! Here's a hint: [first letter + definition]"
   - "Try again!" button
3. Student attempts again (Attempt 2)
4. If correct: "Nice work! You got it!"
5. If incorrect: Show correct answer + move on
6. Store word if needed 2 attempts

**Pros:**
- Immediate learning opportunity
- Builds confidence through success
- More engaging (second chance)

**Cons:**
- Can slow down session flow
- More complex UI states
- Risk of frustration on second miss

### Option C: Spaced Retry at Session End

**Flow:**
1. Student submits incorrect answer
2. Show correction (like Option A)
3. Continue through session
4. At session end, before results:
   - "Let's try these tricky words again!"
   - Re-present all missed words
   - Show improvement score

**Pros:**
- Optimal for retention (spaced repetition)
- Doesn't interrupt flow
- Clear learning outcome

**Cons:**
- Extends session length
- Requires new "review" mode
- Complex state management

### **Recommended: Hybrid Approach (A + C)**

**Immediate feedback (Option A) + Session-end review (Option C)**

1. **During activity:** Show correct answer + context, store for later
2. **End of session:** Quick review of missed words (max 3-5 words)
3. **Results screen:** Show improvement in review performance

This balances flow, pedagogy, and retention.

## Technical Specification

### Data Model

**SessionManager.gd additions:**
```gdscript
# New signals
signal incorrect_answer_submitted(word_data: Dictionary, user_answer: String)
signal review_mode_started(missed_words: Array[Dictionary])
signal review_word_answered(word_data: Dictionary, correct: bool)

# New state
var missed_words: Array[Dictionary] = []
var review_mode: bool = false
var review_index: int = 0

# New methods
func store_missed_word(word_data: Dictionary):
    # Add to missed_words array with timestamp

func start_review_mode():
    # Activate review of missed words (max 5)

func get_review_progress() -> Dictionary:
    # Return improvement statistics
```

### GameSession.gd modifications

**Incorrect answer handling:**
```gdscript
func _on_attempt_result(correct: bool, feedback: String):
    if correct:
        _show_correct_feedback(feedback)
    else:
        _show_incorrect_feedback_with_learning()

func _show_incorrect_feedback_with_learning():
    # 1. Play incorrect sound
    AudioManager.play(AudioManager.SOUND_INCORRECT)

    # 2. Show correction panel with:
    #    - Correct answer (large, clear)
    #    - Definition/context from backend
    #    - Encouraging message

    # 3. Wait for absorption (2.5 seconds)
    await get_tree().create_timer(2.5).timeout

    # 4. Store word in SessionManager
    SessionManager.store_missed_word(current_activity_data)

    # 5. Continue to next activity
    SessionManager.next_activity()
```

**Session end review:**
```gdscript
func _on_session_ended(summary: Dictionary):
    # Check if there are missed words to review
    if SessionManager.has_missed_words():
        _start_review_mode()
    else:
        _navigate_to_results()

func _start_review_mode():
    # Show "Let's review these tricky words!" transition
    # Present missed words one by one
    # Track improvement
    # Then show results with review stats
```

### UI Components

**1. Correction Panel (extends existing FeedbackPanel)**
```
┌─────────────────────────────────┐
│   Not quite! 😊                 │
│                                  │
│   The answer is:                │
│   ┏━━━━━━━━━━━━━━━━━━━━━━━━┓   │
│   ┃  BENEVOLENT              ┃   │
│   ┗━━━━━━━━━━━━━━━━━━━━━━━━┛   │
│                                  │
│   💡 It means: kind and          │
│       generous                   │
│                                  │
│   Let's keep learning! ✨        │
└─────────────────────────────────┘
```

**2. Review Mode Transition**
```
┌─────────────────────────────────┐
│                                  │
│   🌟 Great work! 🌟             │
│                                  │
│   Let's review 3 tricky          │
│   words one more time!           │
│                                  │
│      [Continue]                  │
│                                  │
└─────────────────────────────────┘
```

**3. Review Progress Indicator**
```
Progress: ●●●○○  (3 of 5 words)
```

### Backend Integration

**Required backend support:**
- `definition` field in activity responses (optional, graceful fallback)
- `correctAnswer` field in attempt response (currently exists)
- No new endpoints needed

**Graceful degradation:**
```gdscript
var definition = word_data.get("definition", "")
if definition == "":
    # Just show correct answer, no context
```

### Animation & Effects

**Correction Panel:**
- Slide in from top (0.3s)
- Gentle yellow/orange color (not harsh red)
- Pulse animation on correct answer text
- Fade out after timer (0.2s)

**Review Mode:**
- Fade to black transition (0.5s)
- Slide in review card (0.4s)
- Progress bar fills smoothly

## Implementation Plan

### Phase 1: Core Flow (2-3 hours)
1. Add `missed_words` storage to SessionManager
2. Modify `_on_attempt_result()` in GameSession
3. Create correction panel UI
4. Implement 2.5s display + storage
5. Test basic flow

### Phase 2: Review Mode (2-3 hours)
6. Add review mode state to SessionManager
7. Create review transition UI
8. Implement word re-presentation logic
9. Track improvement statistics
10. Update results screen to show review performance

### Phase 3: Polish (1 hour)
11. Add animations to correction panel
12. Polish review mode transitions
13. Add encouraging messages (randomized)
14. Test with target age group

## Testing Strategy

### Unit Tests
```gdscript
# Test missed word storage
func test_store_missed_word():
    SessionManager.store_missed_word(mock_word_data)
    assert SessionManager.missed_words.size() == 1

# Test review mode activation
func test_review_mode_trigger():
    # Add 3 missed words
    # End session
    # Assert review mode activates
```

### Integration Tests
- Submit incorrect answer → verify correction shown → verify word stored
- Complete session with errors → verify review mode starts
- Complete review → verify results include review stats

### User Testing
- Target age: 3rd-5th graders
- Metrics: Comprehension of corrections, engagement during review
- Interview questions: "Did you learn from your mistakes?"

## Success Criteria

### Must Have
- [ ] Incorrect answers show correct answer + context
- [ ] Correction panel displays for 2.5 seconds
- [ ] Missed words stored in SessionManager
- [ ] Session-end review mode for missed words (max 5)
- [ ] Results screen shows review improvement

### Should Have
- [ ] Encouraging messages are varied and age-appropriate
- [ ] Smooth animations between modes
- [ ] Progress indicator during review
- [ ] Graceful fallback when definition unavailable

### Nice to Have
- [ ] Hint system for second attempts
- [ ] Review difficulty adjustment (if student keeps missing)
- [ ] Parent/teacher report on missed words

## Edge Cases

1. **No missed words:** Skip review mode, go straight to results
2. **Many missed words (>5):** Only review top 5 most recent
3. **Missing definition from backend:** Show only correct answer
4. **Student quits during review:** Save progress, resume later
5. **Review word answered incorrectly again:** Show answer, don't loop
6. **Very fast readers:** Allow "Continue" button after 1.5s minimum

## Future Enhancements

### V2: Adaptive Learning
- Track which words are consistently missed
- Increase frequency of difficult words
- Personalized vocabulary lists

### V3: Multi-Attempt System
- Allow 2 attempts before showing answer
- Progressive hints (first letter → syllable count → definition)
- Partial credit for close answers

### V4: Gamification
- "Mastery" badges for words (3 correct in a row)
- "Comeback" achievement for review perfection
- Visualize progress over time

## Dependencies

- **Phase 4 completion:** SessionManager, GameSession foundations
- **Backend:** No new endpoints, graceful with current data
- **Design assets:** Correction panel styling (use existing theme)

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Review mode slows session flow | Medium | Make optional, cap at 5 words |
| Students find corrections discouraging | High | Playtest messaging, use positive tone |
| Backend doesn't provide definitions | Low | Graceful fallback, just show answer |
| Complex state management bugs | Medium | Thorough testing, clear state transitions |

## Open Questions

1. **Timing:** Is 2.5 seconds right for absorption? (Playtest to confirm)
2. **Review limit:** Max 3 or 5 words for review? (Start with 5, adjust)
3. **Retry logic:** Should students get a second immediate attempt? (No for V1, consider V2)
4. **Skip option:** Can students skip review mode? (Yes, "Continue Without Review" button)

## Approval & Sign-off

- [ ] Product Owner approval
- [ ] Pedagogical approach vetted by educator
- [ ] Technical feasibility confirmed
- [ ] Timeline approved

---

**Related Documents:**
- [Phase 5 Overview](./phase5-overview.md)
- [SessionManager.gd](/vocab_game/scripts/SessionManager.gd)
- [GameSession.gd](/vocab_game/scripts/GameSession.gd)
