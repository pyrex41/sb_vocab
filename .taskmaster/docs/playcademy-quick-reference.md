# Playcademy Vocab Integration - Quick Reference Guide

**Status:** Using `vocab_game/` as foundation (95% complete)  
**Timeline:** 3-4 days (21 hours active work)  
**Primary Task:** Replace MockBackend with PlaycademySDK

---

## TL;DR - What You Need to Know

### Why vocab_game/?
- ✅ All 5 activities already implemented and working
- ✅ Mock backend matches real API exactly
- ✅ Signal-based architecture (no major refactoring needed)
- ✅ Excellent documentation included
- ⏱️ Integration is 80% search-and-replace

### The Integration is Simple
```gdscript
// BEFORE (Mock):
var response = MockBackend.start_session(grade)

// AFTER (Real):
var response = await PlaycademySdk.backend.request("POST", "/session/start", {...})
```

---

## Critical Integration Points (4 Places)

### 1. Start Session
**File:** `scripts/SessionManager.gd`  
**Function:** `start_new_session()`  
**Change:** Replace `MockBackend.start_session()` with SDK call

### 2. Submit Answer
**File:** `scripts/SessionManager.gd`  
**Function:** `submit_answer()`  
**Change:** Replace `MockBackend.submit_attempt()` with SDK call

### 3. End Session
**File:** `scripts/SessionManager.gd`  
**Function:** `_end_session()`  
**Change:** Replace `MockBackend.end_session()` with SDK call

### 4. Get Progress
**File:** `scripts/ProgressScreen.gd`  
**Function:** `_ready()` or `update_progress()`  
**Change:** Replace `MockBackend.get_progress()` with SDK call

---

## Day-by-Day Breakdown

### Day 1: Setup & Core Integration (8 hours)
**Morning (4h):**
- Copy vocab_game/ as working directory
- Study architecture (read DEV_NOTES.md)
- Install PlaycademySDK from AssetLib
- Configure plugins and AutoLoad

**Afternoon (4h):**
- Replace session start call
- Replace answer submission
- Replace session end
- Add basic error handling
- Test with real backend

### Day 2: Polish & Testing (8 hours)
**Morning (4h):**
- Implement audio playback (Spelling activity)
- Add loading states and spinners
- Improve error messages
- Polish UI colors/fonts

**Afternoon (4h):**
- Comprehensive testing (all activities)
- Edge case testing (errors, timeouts)
- Cross-browser testing
- Bug fixes

### Day 3: Deploy & Document (5 hours)
**Morning (3h):**
- Configure web export
- Deploy to staging
- Test deployed version
- Fix any deployment issues

**Afternoon (2h):**
- Write feedback report
- Update documentation
- Push to GitHub dev branch
- Final verification

---

## SDK Integration Template

### Standard Pattern for All API Calls

```gdscript
func call_backend(method: String, endpoint: String, data: Dictionary):
    # Show loading
    emit_signal("loading_started")
    
    # Make request
    var response = await PlaycademySdk.backend.request(method, endpoint, data)
    
    # Hide loading
    emit_signal("loading_ended")
    
    # Handle errors
    if response.error:
        _handle_api_error(response.error)
        return null
    
    # Return successful response
    return response

func _handle_api_error(error: Dictionary):
    var message = "Something went wrong!"
    
    match error.code:
        "NETWORK_ERROR": message = "Can't connect to server!"
        "TIMEOUT": message = "Server is taking too long!"
        "AUTH_ERROR": message = "Please log in again!"
    
    emit_signal("api_error", message)
```

---

## Setup Checklist

### Prerequisites
- [ ] Godot 4.3+ installed
- [ ] GitHub access to project repo
- [ ] Playcademy account created
- [ ] Staging environment access

### Initial Setup (30 minutes)
1. [ ] Copy vocab_game/ to working directory
2. [ ] Open project in Godot
3. [ ] Verify all scenes load without errors
4. [ ] Run mock version to understand flow
5. [ ] Read DEV_NOTES.md and QUICKSTART.md

### SDK Installation (15 minutes)
1. [ ] Open AssetLib in Godot
2. [ ] Search "Playcademy"
3. [ ] Install Playcademy bundle
4. [ ] Enable Manifest Exporter plugin
5. [ ] Enable Sandbox plugin
6. [ ] Add PlaycademySDK to AutoLoad as "PlaycademySdk"
7. [ ] Restart Godot editor
8. [ ] Verify no errors in Output panel

---

## Testing Strategy

### Phase 1: Smoke Testing (After Each Integration)
```bash
✓ Session starts successfully
✓ First activity loads
✓ Can submit an answer
✓ Receives feedback
✓ Session completes
✓ No console errors
```

### Phase 2: Comprehensive Testing (End of Day 2)
```bash
✓ All 5 activity types work correctly
✓ Correct answers scored properly
✓ Incorrect answers handled gracefully
✓ Progress accumulates across sessions
✓ Error messages display correctly
✓ Loading states show/hide properly
```

### Phase 3: Deployment Testing (Day 3)
```bash
✓ Game loads on staging URL
✓ No CORS or security errors
✓ Authentication works
✓ All activities function
✓ Performance acceptable (>30 FPS)
✓ Works in Chrome, Firefox, Safari, Edge
```

---

## Common Issues & Solutions

### Issue: "PlaycademySdk is not defined"
**Solution:** Add to Project Settings > Autoload:
- Path: `res://addons/playcademy/sdk/PlaycademySDK.gd`
- Name: `PlaycademySdk` (exact spelling matters!)

### Issue: "await" Syntax Errors
**Solution:** Ensure function is async:
```gdscript
func start_new_session(grade: int):  # This function calls await
    var response = await PlaycademySdk.backend.request(...)
```

### Issue: CORS Errors in Browser
**Solution:** Using PlaycademySDK should handle this. If not:
- Verify you're using SDK's request() method
- Don't create your own HTTPRequest nodes
- Test on actual staging domain, not localhost

### Issue: Session Data Not Persisting
**Solution:** Verify session_id is stored:
```gdscript
current_session_id = response.session_id  # Must save this!
```

### Issue: Audio Won't Play
**Solution:**
- Test audio URL in browser first
- Check audio format (MP3 preferred)
- Add error handling for missing audio
- Provide text fallback

---

## File Modification Summary

### Files to Modify (Main Integration)
- `scripts/SessionManager.gd` - Replace 4 MockBackend calls
- `scripts/ProgressScreen.gd` - Replace get_progress call
- `scripts/activities/Spelling.gd` - Add audio playback

### Files to Add
- `scripts/ErrorHandler.gd` - Centralized error handling (optional)
- `scripts/LoadingOverlay.gd` - Loading state UI (optional)

### Files to Keep As-Is
- All activity scene files (.tscn)
- All activity logic files (except Spelling.gd)
- All UI layouts
- GameSession.gd
- MainMenu.gd
- ResultsScreen.gd

---

## API Endpoint Quick Reference

### POST /session/start
**Request:**
```json
{"user_id": "string", "grade": 3}
```
**Response:**
```json
{
  "session_id": "abc123",
  "activities": [{"type": "flashcard", "word": {...}}]
}
```

### POST /session/attempt
**Request:**
```json
{"session_id": "abc123", "activity_index": 0, "answer": "abundant"}
```
**Response:**
```json
{"correct": true, "feedback": "Great job!"}
```

### POST /session/end
**Request:**
```json
{"session_id": "abc123"}
```
**Response:**
```json
{"total_activities": 20, "correct_count": 16, "accuracy": 80.0}
```

### GET /progress
**Request:**
```json
{"user_id": "string"}
```
**Response:**
```json
{"total_words_practiced": 45, "overall_accuracy": 82.5}
```

---

## Success Criteria

### Minimum Viable Product (MVP)
- ✅ All 5 activities work with real backend
- ✅ Complete session flow (start → activities → results)
- ✅ Deployed to staging environment
- ✅ No critical bugs
- ✅ Basic error handling

### Polish (If Time Permits)
- ✅ Audio playback in Spelling activity
- ✅ Smooth transitions between activities
- ✅ Improved UI matching Playcademy brand
- ✅ Loading spinners and progress indicators
- ✅ Sentence generation (6th activity)

### Documentation
- ✅ Integration guide updated
- ✅ Feedback report written
- ✅ Code pushed to GitHub
- ✅ README updated

---

## Resource Links

### Essential Documentation
- Playcademy Docs: https://docs.playcademy.net
- Godot SDK Guide: https://docs.playcademy.net/platform-guides/godot
- API Reference: https://docs.playcademy.net/api
- Deployment Guide: https://docs.playcademy.net/deployment

### Project Files
- Current Implementation: `/vocab_game/`
- DEV_NOTES: `/vocab_game/DEV_NOTES.md`
- QUICKSTART: `/vocab_game/QUICKSTART.md`
- README: `/vocab_game/README.md`

---

## Time Estimates by Task

| Task | Estimate | Actual |
|------|----------|--------|
| Setup & Study | 3h | ___ |
| SDK Installation | 0.5h | ___ |
| Session Start Integration | 1h | ___ |
| Answer Submit Integration | 1.5h | ___ |
| Session End Integration | 1h | ___ |
| Error Handling | 1.5h | ___ |
| Audio Implementation | 1.5h | ___ |
| UI Polish | 1.5h | ___ |
| Testing | 3h | ___ |
| Deployment | 3h | ___ |
| Documentation | 2h | ___ |
| **Total** | **21h** | ___ |

---

## Quick Win Strategy

### First 2 Hours (Immediate Progress)
1. **Hour 1:** Copy project, install SDK, verify setup
2. **Hour 2:** Replace session start call, test with real backend

**Result:** You'll have the game starting sessions with real data!

### Next 4 Hours (Core Functionality)
3. **Hour 3:** Replace answer submission
4. **Hour 4:** Replace session end
5. **Hour 5-6:** Add error handling and loading states

**Result:** Complete session flow working end-to-end!

### Final Push (Polish & Ship)
7-12. Testing, audio, UI polish
13-15. Deployment
16-21. Documentation

**Result:** Production-ready application!

---

## Daily Goals

### Day 1 Goal
**Can complete a full session with real backend data**
- Session starts
- Activities load with real words
- Answers are scored correctly
- Session ends and shows results

### Day 2 Goal
**Polished and thoroughly tested**
- Audio works (or graceful fallback)
- UI matches brand
- All edge cases handled
- Cross-browser tested

### Day 3 Goal
**Deployed and documented**
- Live on staging
- Feedback report written
- Code pushed to GitHub
- Ready for review

---

## Red Flags to Watch For

🚩 **If you spend >2 hours on any single task** → Ask for help  
🚩 **If tests fail repeatedly** → Check API endpoint URLs  
🚩 **If deployment fails** → Verify export settings  
🚩 **If feeling lost** → Re-read DEV_NOTES.md in vocab_game/  

---

## Contact & Support

**If Stuck:**
1. Check DEV_NOTES.md in vocab_game/
2. Review Playcademy docs
3. Check console for error messages
4. Test with mock backend to isolate issue
5. Ask for help (don't waste time!)

**For Deployment Issues:**
- Verify Playcademy CLI installed: `playcademy --version`
- Check authentication: `playcademy status`
- Review export settings in Godot

---

## Final Tips

### Do's ✅
- Start with vocab_game/ (it's 95% done!)
- Test after each change
- Keep mock backend as fallback
- Read existing code before modifying
- Document what you learn

### Don'ts ❌
- Don't rewrite working code
- Don't skip error handling
- Don't test only on desktop
- Don't forget to commit often
- Don't panic if something breaks (mock is backup!)

---

**You've got this! The hard work is already done. Now just connect the dots.** 🚀

