# Common Lisp Implementation - Task Breakdown

**Project:** Playcademy Vocabulary Game - Common Lisp Frontend
**Total Estimated Time:** 24 hours (4 days)
**Status:** Ready for Implementation
**Created:** 2025-11-07

---

## Overview

This document provides a **detailed hour-by-hour task breakdown** for implementing the Common Lisp vocabulary game frontend. Each task includes:
- Estimated duration
- Files to create/modify
- Acceptance criteria
- Dependencies

---

## Phase 1: Core Infrastructure (Day 1 - 6 hours)

### Task 1.1: Project Setup (1 hour)

**Goal:** Create foundational project structure

**Tasks:**
- [ ] Install SBCL or CCL (if not already installed)
- [ ] Install/update Quicklisp
- [ ] Create project directory structure
- [ ] Initialize git repository
- [ ] Create `.gitignore` for Lisp projects

**Files to Create:**
```
vocab-game-cl/
├── .gitignore
├── README.md
├── vocab-game.asd
└── src/
```

**Acceptance Criteria:**
- Can run `sbcl` from terminal
- Quicklisp loads successfully
- Directory structure matches PRD

**Commands:**
```bash
mkdir -p vocab-game-cl/{src,templates,static,tests,scripts}
mkdir -p vocab-game-cl/templates/{layouts,activities}
mkdir -p vocab-game-cl/static/{css,js,assets}
cd vocab-game-cl
git init
```

---

### Task 1.2: ASDF System Definition (1 hour)

**Goal:** Create loadable ASDF system with dependencies

**Files to Create:**
- `vocab-game.asd`
- `src/package.lisp`

**Dependencies:**
```lisp
:depends-on (#:caveman2
             #:clack
             #:djula
             #:parenscript
             #:dexador
             #:jonathan
             #:lack
             #:lack-middleware-session
             #:alexandria
             #:serapeum
             #:cl-ppcre
             #:local-time
             #:log4cl)
```

**Acceptance Criteria:**
- `(ql:quickload :vocab-game)` loads without errors
- All dependencies install successfully
- Package `#:vocab-game` exists

**Test:**
```common-lisp
(ql:quickload :vocab-game)
(in-package :vocab-game)
```

---

### Task 1.3: Configuration & Models (1 hour)

**Goal:** Implement configuration and data structures

**Files to Create:**
- `src/config.lisp`
- `src/models.lisp`

**Configuration Variables:**
- `*api-base-url*`
- `*api-key*`
- `*server-port*`
- `*debug-mode*`
- `*session-secret*`

**Data Structures to Implement:**
- `word-data` - Vocabulary word struct
- `activity` - Single activity struct
- `game-session` - Session state struct
- `attempt-result` - Answer result struct
- `session-summary` - End summary struct
- `progress-data` - User progress struct

**Acceptance Criteria:**
- All structs defined with proper slots
- Configuration reads from environment variables
- Can create instances: `(make-word-data :word "test")`

**Test:**
```common-lisp
(defparameter *test-word*
  (make-word-data :word "example" :definition "a test"))
(word-data-word *test-word*) ; => "example"
```

---

### Task 1.4: API Client Implementation (2 hours)

**Goal:** Implement all 4 Playcademy API endpoints

**File to Create:**
- `src/api-client.lisp`

**Functions to Implement:**

1. **`api-request`** (30 min)
   - Generic HTTP request function
   - Handle POST and GET
   - Parse JSON responses
   - Error handling (network, timeout, auth)

2. **`start-session`** (30 min)
   - POST `/session/start`
   - Convert JSON → `game-session` struct
   - Parse activities array

3. **`submit-attempt`** (30 min)
   - POST `/session/attempt`
   - Convert JSON → `attempt-result` struct

4. **`end-session`** (15 min)
   - POST `/session/end`
   - Convert JSON → `session-summary` struct

5. **`get-progress`** (15 min)
   - GET `/progress`
   - Convert JSON → `progress-data` struct

**Helper Functions:**
- `parse-activity` - Convert JSON to activity struct
- `parse-word-data` - Convert JSON to word-data struct

**Error Conditions:**
- `api-error` - HTTP errors (4xx, 5xx)
- `network-error` - Connection failures

**Acceptance Criteria:**
- All 4 endpoints callable
- JSON correctly parsed to structs
- Errors handled gracefully
- Can make real API call (with test credentials)

**Test:**
```common-lisp
;; Requires valid API key
(defparameter *test-session*
  (start-session "test-user" 3))
(game-session-session-id *test-session*) ; => "uuid-here"
```

---

### Task 1.5: API Client Tests (1 hour)

**Goal:** Write unit tests for API client

**File to Create:**
- `tests/package.lisp`
- `tests/api-client-tests.lisp`

**Test Suite:** `api-client-suite`

**Tests to Write:**

1. **`test-start-session-success`**
   - Verify session created
   - Check grade set correctly
   - Activities list populated

2. **`test-submit-attempt-correct`**
   - Submit correct answer
   - Verify `correct: true`

3. **`test-submit-attempt-incorrect`**
   - Submit wrong answer
   - Verify `correct: false`
   - Check `correct-answer` provided

4. **`test-end-session`**
   - End session successfully
   - Verify summary returned

5. **`test-get-progress`**
   - Fetch user progress
   - Verify stats present

6. **`test-api-error-handling`**
   - Mock network failure
   - Verify error condition raised

**Acceptance Criteria:**
- All tests pass: `(asdf:test-system :vocab-game)`
- Code coverage > 80%
- Tests run in < 5 seconds

---

### Task 1.6: Integration Verification (30 min)

**Goal:** Verify API integration with real backend

**Tasks:**
- [ ] Get Playcademy API credentials
- [ ] Set environment variables
- [ ] Test each endpoint with curl
- [ ] Verify responses match expected format

**curl Tests:**
```bash
# Test session start
curl -X POST https://api.playcademy.net/session/start \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"user_id":"test","grade":3}'

# Verify response format matches models
```

**Acceptance Criteria:**
- Can authenticate with API
- All endpoints return valid JSON
- Response format matches `models.lisp` structs

---

## Phase 2: Web Server & Routing (Day 2 - 6 hours)

### Task 2.1: Caveman2 Application Setup (1 hour)

**Goal:** Initialize web server skeleton

**Files to Create:**
- `src/main.lisp`
- `src/routes.lisp`

**Components:**

1. **`main.lisp`** (30 min)
   - Define `*app*` parameter
   - Implement `start-server` function
   - Implement `stop-server` function
   - Configure Lack middleware
   - Session middleware setup

2. **`routes.lisp`** (30 min)
   - Set up Caveman2 routing
   - Define basic route structure
   - Configure Djula template directory

**Acceptance Criteria:**
- Server starts: `(start-server)`
- Responds to `GET /` (even if empty)
- Can stop server: `(stop-server)`
- Session middleware active

**Test:**
```common-lisp
(start-server :port 5000)
;; Visit http://localhost:5000
;; Should see default Caveman2 page or error (expected at this point)
```

---

### Task 2.2: Session Management Implementation (2 hours)

**Goal:** Implement game session state management

**File to Create:**
- `src/session.lisp`

**Global State:**
- `*active-sessions*` - Hash table of session-id → game-session

**Functions to Implement:**

1. **`create-new-session`** (30 min)
   - Call `start-session` from api-client
   - Store in `*active-sessions*`
   - Return session object

2. **`get-session`** (15 min)
   - Retrieve session by ID
   - Handle missing sessions

3. **`update-session-score`** (15 min)
   - Increment score if correct
   - Track attempts

4. **`advance-activity`** (30 min)
   - Move to next activity
   - Check if session complete
   - Return boolean

5. **`get-current-activity`** (15 min)
   - Get activity at current index
   - Handle bounds checking

6. **`finish-session`** (30 min)
   - Call `end-session` from api-client
   - Clean up from `*active-sessions*`
   - Return summary

**Acceptance Criteria:**
- Can create and retrieve sessions
- Activity progression works
- Score updates correctly
- Sessions cleaned up after completion

**Test:**
```common-lisp
(defparameter *s* (create-new-session "test" 3))
(get-current-activity *s*) ; => first activity
(advance-activity *s*) ; => nil (not done)
(get-current-activity *s*) ; => second activity
```

---

### Task 2.3: Route Implementation (2 hours)

**Goal:** Implement all web routes

**File to Modify:**
- `src/routes.lisp`

**Routes to Implement:**

1. **`GET /`** (15 min)
   - Render main menu
   - Grade selection form

2. **`POST /start-session`** (30 min)
   - Parse grade from form
   - Call `create-new-session`
   - Store session-id in HTTP session
   - Redirect to `/game`

3. **`GET /game`** (30 min)
   - Retrieve session from HTTP session
   - Get current activity
   - Render appropriate activity template
   - Display progress (X/Y)

4. **`POST /submit-answer`** (45 min)
   - Parse answer from request
   - Call `submit-attempt`
   - Update session score
   - Advance activity or finish session
   - Return JSON result

5. **`GET /results`** (15 min)
   - Retrieve session summary
   - Render results template

6. **`GET /progress`** (15 min)
   - Get user-id from session
   - Call `get-progress`
   - Render progress template

**Helper Functions:**
- `render-activity` - Choose correct template for activity type
- `activity-template-name` - Map activity type → template path
- `generate-guest-id` - Create unique guest user ID

**Acceptance Criteria:**
- All routes respond with 200 OK
- Session flow works (start → game → results)
- Answers submit correctly
- Progress displays

---

### Task 2.4: Error Handling (30 min)

**Goal:** Add graceful error handling

**Files to Modify:**
- `src/routes.lisp`
- `src/api-client.lisp`

**Error Handlers:**

1. **API Errors** (15 min)
   - Catch `api-error` condition
   - Display user-friendly message
   - Log technical details

2. **Session Errors** (15 min)
   - Handle missing session
   - Redirect to main menu
   - Clear invalid session

**Error Page Template:**
- Create `templates/error.html`
- Display friendly error message
- "Return to Home" button

**Acceptance Criteria:**
- Network errors don't crash app
- Invalid session IDs handled
- User sees helpful error messages
- Logs contain debugging info

---

### Task 2.5: Route Testing (30 min)

**Goal:** Verify all routes work

**Manual Tests:**

1. **Test with curl/Postman**
   ```bash
   # Start session
   curl -X POST http://localhost:5000/start-session \
     -d "grade=3" \
     --cookie-jar cookies.txt

   # Visit game
   curl http://localhost:5000/game --cookie cookies.txt

   # Submit answer
   curl -X POST http://localhost:5000/submit-answer \
     -H "Content-Type: application/json" \
     -d '{"answer":"test"}' \
     --cookie cookies.txt
   ```

2. **Test in browser**
   - Complete full session flow
   - Verify redirects work
   - Check session persistence

**Acceptance Criteria:**
- All routes return valid responses
- Session cookies work
- Redirects function correctly
- JSON responses valid

---

## Phase 3: Frontend & Templates (Day 3 - 8 hours)

### Task 3.1: Base Layout Template (30 min)

**Goal:** Create reusable layout

**File to Create:**
- `templates/layouts/default.html`

**Components:**
- `<head>` with CSS links
- Header with logo/title
- Main content block
- Footer
- JavaScript includes

**Acceptance Criteria:**
- Valid HTML5
- Responsive meta tags
- CSS/JS properly linked
- Content block inheritance works

---

### Task 3.2: Main Menu Template (1 hour)

**Goal:** Grade selection screen

**File to Create:**
- `templates/index.html`

**Components:**
- Welcome message
- Grade selection buttons (3, 4, 5)
- "View Progress" link
- Age-appropriate styling

**Form:**
```html
<form method="POST" action="/start-session">
  <button name="grade" value="3">Grade 3</button>
  <button name="grade" value="4">Grade 4</button>
  <button name="grade" value="5">Grade 5</button>
</form>
```

**Acceptance Criteria:**
- Renders correctly
- Grade selection works
- Redirects to `/game` after selection

---

### Task 3.3: Game Session Container (1 hour)

**Goal:** Create activity container

**File to Create:**
- `templates/game-session.html`

**Components:**
- Progress indicator (e.g., "5 / 20")
- Activity content area (dynamic)
- Feedback panel (hidden by default)
- Audio player container (for spelling)

**Acceptance Criteria:**
- Progress displays correctly
- Activity content renders
- Feedback shows/hides dynamically

---

### Task 3.4: Activity Templates (2 hours)

**Goal:** Create all 5 activity templates

**Files to Create:**
- `templates/activities/flashcard.html` (20 min)
- `templates/activities/multiple-choice.html` (30 min)
- `templates/activities/spelling.html` (30 min)
- `templates/activities/fill-blank.html` (20 min)
- `templates/activities/synonym-antonym.html` (20 min)

**Each Template Needs:**
- Activity-specific UI
- Input elements (buttons, text fields)
- Submit button
- Feedback area

**Acceptance Criteria:**
- All 5 templates render
- Input elements functional
- Feedback displays correctly

---

### Task 3.5: Results & Progress Templates (30 min)

**Goal:** Display session summary and progress

**Files to Create:**
- `templates/results.html` (15 min)
- `templates/progress.html` (15 min)

**Results Template:**
- Score display (X / Y correct)
- Accuracy percentage
- Words practiced count
- "Play Again" button

**Progress Template:**
- Total words practiced
- Overall accuracy
- Sessions completed
- Mastery levels breakdown

**Acceptance Criteria:**
- Stats display correctly
- "Play Again" returns to menu

---

### Task 3.6: Parenscript Frontend (2 hours)

**Goal:** Implement client-side logic

**File to Create:**
- `src/parenscript-frontend.lisp`

**Functions to Implement (in Parenscript):**

1. **`submit-answer`** (30 min)
   - Get answer from input
   - POST to `/submit-answer`
   - Handle response (complete or continue)

2. **`handle-submission-result`** (30 min)
   - Parse JSON result
   - Show feedback
   - Redirect if complete
   - Reload page for next activity

3. **`show-feedback`** (20 min)
   - Display correct/incorrect message
   - Apply CSS classes
   - Auto-hide after delay

4. **`play-word-audio`** (15 min)
   - Play HTML5 audio element
   - Handle audio errors

5. **`select-option`** (15 min)
   - Highlight selected option
   - Enable submit button

6. **`start-session`** (10 min)
   - Main menu grade selection
   - POST to `/start-session`

**Compilation Function:**
```common-lisp
(defun compile-frontend-js ()
  "Compile Parenscript to static/js/generated.js"
  (with-open-file (out "static/js/generated.js"
                       :direction :output
                       :if-exists :supersede)
    (write-string (ps:ps ...) out)))
```

**Acceptance Criteria:**
- Compiles without errors
- JavaScript valid (no syntax errors)
- All functions work in browser

---

### Task 3.7: CSS Styling (1.5 hours)

**Goal:** Age-appropriate, responsive design

**Files to Create:**
- `static/css/main.css` (45 min)
- `static/css/activities.css` (45 min)

**main.css Includes:**
- Typography (large, readable fonts)
- Color scheme (bright, child-friendly)
- Button styles (large touch targets)
- Layout (responsive grid)
- Animations (feedback transitions)

**activities.css Includes:**
- Activity-specific styles
- Option buttons
- Input fields
- Feedback panels

**Design Requirements:**
- Font size: 18-24px (body), 32-48px (headers)
- Button min-height: 60px
- High contrast colors
- Smooth transitions

**Acceptance Criteria:**
- Responsive on mobile/tablet/desktop
- Touch targets > 44px
- WCAG AA contrast ratios
- Animations smooth (CSS transitions)

---

## Phase 4: Polish & Testing (Day 4 - 4 hours)

### Task 4.1: End-to-End Integration Tests (1.5 hours)

**Goal:** Test complete user flows

**Manual Test Checklist:**

**Flow 1: Complete Grade 3 Session** (30 min)
- [ ] Start from main menu
- [ ] Select Grade 3
- [ ] Complete all activities
- [ ] Verify score calculation
- [ ] View results screen
- [ ] Check progress updated

**Flow 2: Complete Grade 4 Session** (20 min)
- [ ] Repeat above for Grade 4

**Flow 3: Complete Grade 5 Session** (20 min)
- [ ] Repeat above for Grade 5

**Flow 4: Error Scenarios** (20 min)
- [ ] Submit empty answer
- [ ] Disconnect network mid-session
- [ ] Clear cookies mid-session
- [ ] Invalid session ID
- [ ] API timeout

**Acceptance Criteria:**
- All flows complete without errors
- Errors handled gracefully
- User never sees stack traces

---

### Task 4.2: Cross-Browser Testing (1 hour)

**Goal:** Verify compatibility

**Browsers to Test:**
- Chrome (latest)
- Firefox (latest)
- Safari (latest)
- Edge (latest)

**Test Checklist (per browser):**
- [ ] Main menu loads
- [ ] Can start session
- [ ] All activity types work
- [ ] Audio plays (spelling)
- [ ] Feedback displays
- [ ] Results screen shows

**Acceptance Criteria:**
- Works in all 4 browsers
- No console errors
- Consistent appearance
- Responsive layout functions

---

### Task 4.3: Performance Optimization (30 min)

**Goal:** Ensure fast load times

**Tasks:**
- [ ] Measure page load time (Chrome DevTools)
- [ ] Optimize JavaScript (minify generated.js)
- [ ] Optimize CSS (remove unused rules)
- [ ] Add caching headers
- [ ] Profile server response time

**Targets:**
- Page load: < 500ms
- API response: < 2s
- JavaScript execution: < 100ms

**Acceptance Criteria:**
- Meets performance targets
- No unnecessary requests
- Assets cached properly

---

### Task 4.4: Documentation Updates (1 hour)

**Goal:** Complete documentation

**Files to Update:**
- `README.md` (30 min)
  - Add setup instructions
  - Add deployment guide
  - Add troubleshooting section

- `QUICKSTART.md` (15 min)
  - 5-minute quickstart
  - Common commands
  - Example usage

- Inline Documentation (15 min)
  - Add docstrings to all functions
  - Comment complex logic
  - Update TODOs

**Acceptance Criteria:**
- New developer can set up in < 30 min
- All public functions documented
- No outdated TODOs

---

## Task Summary

| Phase | Tasks | Estimated Time | Files Created |
|-------|-------|----------------|---------------|
| **Phase 1** | 6 | 6 hours | 5 source files, 2 test files |
| **Phase 2** | 5 | 6 hours | 2 source files |
| **Phase 3** | 7 | 8 hours | 10 templates, 2 CSS files, 1 source file |
| **Phase 4** | 4 | 4 hours | Documentation updates |
| **Total** | **22 tasks** | **24 hours** | **~25 files** |

---

## Daily Schedule

### Day 1 (6 hours)
- 9:00 - 10:00: Setup (Task 1.1)
- 10:00 - 11:00: ASDF (Task 1.2)
- 11:00 - 12:00: Config & Models (Task 1.3)
- 12:00 - 14:00: API Client (Task 1.4)
- 14:00 - 15:00: API Tests (Task 1.5)
- 15:00 - 15:30: Integration Check (Task 1.6)

### Day 2 (6 hours)
- 9:00 - 10:00: Caveman Setup (Task 2.1)
- 10:00 - 12:00: Session Mgmt (Task 2.2)
- 12:00 - 14:00: Routes (Task 2.3)
- 14:00 - 14:30: Error Handling (Task 2.4)
- 14:30 - 15:00: Route Testing (Task 2.5)

### Day 3 (8 hours)
- 9:00 - 9:30: Base Layout (Task 3.1)
- 9:30 - 10:30: Main Menu (Task 3.2)
- 10:30 - 11:30: Game Container (Task 3.3)
- 11:30 - 13:30: Activity Templates (Task 3.4)
- 13:30 - 14:00: Results/Progress (Task 3.5)
- 14:00 - 16:00: Parenscript (Task 3.6)
- 16:00 - 17:30: CSS (Task 3.7)

### Day 4 (4 hours)
- 9:00 - 10:30: E2E Tests (Task 4.1)
- 10:30 - 11:30: Cross-browser (Task 4.2)
- 11:30 - 12:00: Performance (Task 4.3)
- 12:00 - 13:00: Documentation (Task 4.4)

---

## Tracking Progress

Use this checklist to track completion:

### Phase 1: Core Infrastructure
- [ ] Task 1.1: Project Setup
- [ ] Task 1.2: ASDF System
- [ ] Task 1.3: Config & Models
- [ ] Task 1.4: API Client
- [ ] Task 1.5: API Tests
- [ ] Task 1.6: Integration Check

### Phase 2: Web Server
- [ ] Task 2.1: Caveman Setup
- [ ] Task 2.2: Session Management
- [ ] Task 2.3: Routes
- [ ] Task 2.4: Error Handling
- [ ] Task 2.5: Route Testing

### Phase 3: Frontend
- [ ] Task 3.1: Base Layout
- [ ] Task 3.2: Main Menu
- [ ] Task 3.3: Game Container
- [ ] Task 3.4: Activity Templates
- [ ] Task 3.5: Results/Progress
- [ ] Task 3.6: Parenscript
- [ ] Task 3.7: CSS Styling

### Phase 4: Polish
- [ ] Task 4.1: E2E Tests
- [ ] Task 4.2: Cross-browser
- [ ] Task 4.3: Performance
- [ ] Task 4.4: Documentation

---

**Status:** Ready to begin implementation
**Last Updated:** 2025-11-07
