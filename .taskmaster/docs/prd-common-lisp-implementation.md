# PRD: Common Lisp Frontend Implementation for Playcademy Vocabulary Game

**Version:** 1.0
**Status:** Ready for Implementation
**Created:** 2025-11-07
**Timeline:** 4 days (24 hours)
**Priority:** P0 - Complete replacement of Godot frontend

---

## Executive Summary

Build a **complete web-based vocabulary learning application** using **Common Lisp** as a replacement for the existing Godot/GDScript frontend. This implementation will integrate with the existing Playcademy REST API backend while providing a native web experience without requiring WebAssembly or game engine exports.

### Key Benefits
- **Native Web Stack**: No game engine overhead, pure web standards
- **Type Safety**: Strong typing catches errors at compile-time
- **REPL-Driven Development**: Live coding with instant feedback
- **Unified Language**: Same language (Lisp) for both server and client (via Parenscript)
- **Production Ready**: Mature, battle-tested libraries
- **Easy Deployment**: Standard web server deployment (Docker, Heroku, VPS)

---

## 1. Technical Architecture

### 1.1 Technology Stack

| Component | Library/Framework | Purpose |
|-----------|------------------|---------|
| **Web Framework** | Caveman2 | Routing, request handling, middleware |
| **HTTP Server** | Clack + Hunchentoot/Woo | Web server interface |
| **Templating** | Djula | Django-style HTML templates |
| **Client-Side JS** | Parenscript | Lisp → JavaScript transpiler |
| **HTTP Client** | Dexador | API communication with Playcademy backend |
| **JSON** | Jonathan | Fast JSON parsing/encoding |
| **Session Management** | Lack-middleware-session | HTTP session handling |
| **Asset Pipeline** | Lack | CSS/JS bundling |
| **Testing** | FiveAM | Unit and integration testing |
| **Logging** | Log4CL | Application logging |
| **Utilities** | Alexandria, Serapeum | Common utilities |

### 1.2 System Architecture Diagram

```
┌─────────────────────────────────────────────────────┐
│         Browser (User Interface)                     │
├─────────────────────────────────────────────────────┤
│                                                      │
│  HTML (Djula Templates) + CSS                       │
│  JavaScript (Generated from Parenscript)            │
│                                                      │
└──────────────────────┬──────────────────────────────┘
                       │ HTTP/AJAX
                       ↓
┌─────────────────────────────────────────────────────┐
│      Common Lisp Web Application (Caveman2)         │
├─────────────────────────────────────────────────────┤
│                                                      │
│  ┌────────────────────────────────────────────┐    │
│  │  Routes (routes.lisp)                      │    │
│  │  - GET  /                (Main menu)       │    │
│  │  - POST /start-session   (Begin game)      │    │
│  │  - GET  /game            (Activity view)   │    │
│  │  - POST /submit-answer   (Answer attempt)  │    │
│  │  - GET  /results         (Session summary) │    │
│  │  - GET  /progress        (User progress)   │    │
│  └────────────────────────────────────────────┘    │
│                                                      │
│  ┌────────────────────────────────────────────┐    │
│  │  Session Manager (session.lisp)            │    │
│  │  - Track active game sessions              │    │
│  │  - Manage activity progression             │    │
│  │  - Score calculation                       │    │
│  └────────────────────────────────────────────┘    │
│                                                      │
│  ┌────────────────────────────────────────────┐    │
│  │  API Client (api-client.lisp)              │    │
│  │  - POST /session/start                     │    │
│  │  - POST /session/attempt                   │    │
│  │  - POST /session/end                       │    │
│  │  - GET  /progress                          │    │
│  └────────────────────────────────────────────┘    │
│                                                      │
│  ┌────────────────────────────────────────────┐    │
│  │  Data Models (models.lisp)                 │    │
│  │  - word-data, activity, game-session       │    │
│  │  - attempt-result, session-summary         │    │
│  └────────────────────────────────────────────┘    │
│                                                      │
└──────────────────────┬──────────────────────────────┘
                       │ REST API (JSON/HTTPS)
                       ↓
┌─────────────────────────────────────────────────────┐
│        Playcademy Backend (Existing)                 │
│        - FSRS spaced repetition                      │
│        - 350+ vocabulary words                       │
│        - Audio storage                               │
│        - Progress persistence                        │
└─────────────────────────────────────────────────────┘
```

---

## 2. File Structure

```
vocab-game-cl/
├── vocab-game.asd              # ASDF system definition
├── README.md                   # Project documentation
├── QUICKSTART.md               # Quick setup guide
├── config.lisp                 # Global configuration
│
├── src/
│   ├── package.lisp            # Package definitions
│   ├── config.lisp             # Runtime config (env vars)
│   ├── models.lisp             # Data structures (structs)
│   ├── api-client.lisp         # Playcademy API client (Dexador)
│   ├── session.lisp            # Session state management
│   ├── routes.lisp             # Caveman2 routes (controllers)
│   ├── parenscript-frontend.lisp  # Client-side Lisp→JS
│   └── main.lisp               # Application entry point
│
├── templates/
│   ├── layouts/
│   │   └── default.html        # Base layout with header/footer
│   ├── index.html              # Main menu (grade selection)
│   ├── game-session.html       # Game container page
│   ├── activities/
│   │   ├── flashcard.html
│   │   ├── multiple-choice.html
│   │   ├── spelling.html
│   │   ├── fill-blank.html
│   │   └── synonym-antonym.html
│   ├── results.html            # Session summary screen
│   └── progress.html           # User progress tracking
│
├── static/
│   ├── css/
│   │   ├── main.css           # Base styles (layout, typography)
│   │   ├── activities.css     # Activity-specific styles
│   │   └── components.css     # Reusable components (buttons, cards)
│   ├── js/
│   │   └── generated.js       # Compiled Parenscript output
│   └── assets/
│       ├── images/
│       │   ├── logo.svg
│       │   └── icons/
│       └── fonts/
│
├── tests/
│   ├── package.lisp
│   ├── api-client-tests.lisp
│   ├── session-tests.lisp
│   └── routes-tests.lisp
│
└── scripts/
    ├── start-dev.lisp         # Development server
    ├── build-production.lisp  # Production build
    └── deploy.sh              # Deployment script
```

---

## 3. Core Implementation Files

### 3.1 ASDF System Definition (vocab-game.asd)

```common-lisp
(defsystem "vocab-game"
  :description "Playcademy Vocabulary Learning Game - Common Lisp Frontend"
  :version "1.0.0"
  :author "Your Name <email@example.com>"
  :license "MIT"
  :depends-on (#:caveman2          ; Web framework
               #:clack             ; Web server interface
               #:djula             ; Templates
               #:parenscript       ; Lisp → JavaScript
               #:dexador           ; HTTP client
               #:jonathan          ; JSON parsing
               #:cl-who            ; HTML generation (alternative)
               #:lack              ; Middleware
               #:lack-middleware-session
               #:alexandria        ; Utilities
               #:serapeum          ; More utilities
               #:cl-ppcre          ; Regular expressions
               #:local-time        ; Time handling
               #:log4cl            ; Logging
               )
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "config" :depends-on ("package"))
                 (:file "models" :depends-on ("package"))
                 (:file "api-client" :depends-on ("package" "config" "models"))
                 (:file "session" :depends-on ("package" "models" "api-client"))
                 (:file "parenscript-frontend" :depends-on ("package"))
                 (:file "routes" :depends-on ("package" "session" "api-client"))
                 (:file "main" :depends-on ("package" "routes" "config")))))
  :in-order-to ((test-op (test-op "vocab-game/tests"))))

(defsystem "vocab-game/tests"
  :depends-on (#:vocab-game
               #:fiveam)
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "api-client-tests" :depends-on ("package"))
                 (:file "session-tests" :depends-on ("package"))))))
```

### 3.2 Package Definition (src/package.lisp)

```common-lisp
(defpackage #:vocab-game
  (:use #:cl #:caveman2 #:alexandria #:serapeum)
  (:import-from #:jonathan
                #:parse
                #:to-json)
  (:import-from #:dexador
                #:post
                #:get)
  (:export #:start-server
           #:stop-server
           #:*app*))

(in-package #:vocab-game)
```

### 3.3 Configuration (src/config.lisp)

```common-lisp
(in-package #:vocab-game)

(defparameter *api-base-url*
  (or (uiop:getenv "PLAYCADEMY_API_URL")
      "https://api.playcademy.net")
  "Playcademy backend API base URL")

(defparameter *api-key*
  (or (uiop:getenv "PLAYCADEMY_API_KEY")
      "your-api-key-here")
  "API authentication key")

(defparameter *server-port*
  (parse-integer (or (uiop:getenv "PORT") "5000"))
  "Web server port")

(defparameter *debug-mode*
  (string= (uiop:getenv "DEBUG") "true")
  "Enable debug logging and hot reload")

(defparameter *session-secret*
  (or (uiop:getenv "SESSION_SECRET")
      "change-me-in-production-please")
  "Secret key for session encryption")
```

### 3.4 Data Models (src/models.lisp)

```common-lisp
(in-package #:vocab-game)

;;; Data structures matching Playcademy API

(defstruct word-data
  "Vocabulary word with metadata"
  id
  word
  definition
  example
  synonyms      ; list of strings
  antonyms      ; list of strings
  audio-url)

(defstruct activity
  "Single activity in a session"
  type          ; :flashcard :multiple-choice :spelling :fill-blank :synonym-antonym
  word          ; word-data struct
  options)      ; list of strings (for multiple choice)

(defstruct game-session
  "Active game session state"
  session-id
  grade
  activities    ; list of activity structs
  current-index
  score
  total-activities
  started-at
  user-id)

(defstruct attempt-result
  "Result of an answer submission"
  correct
  feedback
  correct-answer) ; only if incorrect

(defstruct session-summary
  "End-of-session statistics"
  total-activities
  correct-count
  accuracy
  words-practiced
  session-duration)

(defstruct progress-data
  "User progress tracking"
  total-words-practiced
  overall-accuracy
  sessions-completed
  mastery-levels) ; hash-table {:new 15 :learning 20 ...}
```

---

## 4. Implementation Timeline

### Phase 1: Core Infrastructure (Day 1 - 6 hours)

**Goal:** Set up project foundation and API integration

| Task | Duration | Files | Deliverable |
|------|----------|-------|-------------|
| Create ASDF system + packages | 1h | `vocab-game.asd`, `package.lisp` | Loadable system |
| Implement configuration | 0.5h | `config.lisp` | Environment-based config |
| Implement data models | 1h | `models.lisp` | All structs defined |
| Implement API client | 2h | `api-client.lisp` | All 4 endpoints working |
| Write API client tests | 1h | `tests/api-client-tests.lisp` | Passing test suite |
| Test integration with Playcademy | 0.5h | Manual curl tests | Verified API communication |

**Success Criteria:**
- [ ] Can load system: `(ql:quickload :vocab-game)`
- [ ] Can call `(start-session "test-user" 3)` and get valid response
- [ ] All API client tests pass: `(asdf:test-system :vocab-game)`

---

### Phase 2: Web Server & Routing (Day 2 - 6 hours)

**Goal:** Implement web server with all routes

| Task | Duration | Files | Deliverable |
|------|----------|-------|-------------|
| Set up Caveman2 skeleton | 1h | `main.lisp`, `routes.lisp` | Running web server |
| Implement session management | 2h | `session.lisp` | State tracking |
| Implement all routes | 2h | `routes.lisp` | All endpoints defined |
| Add error handling | 0.5h | `routes.lisp`, `api-client.lisp` | Graceful error messages |
| Test with curl/Postman | 0.5h | Manual testing | All routes respond |

**Routes to Implement:**
- `GET /` - Main menu
- `POST /start-session` - Initialize game
- `GET /game` - Current activity view
- `POST /submit-answer` - Process answer
- `GET /results` - Session summary
- `GET /progress` - User progress

**Success Criteria:**
- [ ] Server starts: `(vocab-game:start-server)`
- [ ] Can POST to `/start-session` and get session-id
- [ ] Can complete full session flow via API calls

---

### Phase 3: Frontend Templates & Parenscript (Day 3 - 8 hours)

**Goal:** Build complete user interface

| Task | Duration | Files | Deliverable |
|------|----------|-------|-------------|
| Create base layout template | 0.5h | `templates/layouts/default.html` | Header/footer layout |
| Create main menu | 1h | `templates/index.html` | Grade selection screen |
| Create game session container | 1h | `templates/game-session.html` | Activity container |
| Create all 5 activity templates | 2h | `templates/activities/*.html` | Flashcard, MC, Spelling, Fill, Syn/Ant |
| Create results screen | 0.5h | `templates/results.html` | Score summary |
| Create progress screen | 0.5h | `templates/progress.html` | Progress tracking |
| Implement Parenscript frontend | 2h | `parenscript-frontend.lisp` | Client-side logic |
| CSS styling | 1.5h | `static/css/*.css` | Age-appropriate UI |

**Parenscript Components:**
- Answer submission (AJAX)
- Feedback display (correct/incorrect)
- Audio playback (spelling activity)
- Multiple choice selection
- Progress indicators

**Success Criteria:**
- [ ] Can navigate through complete session in browser
- [ ] All 5 activity types render correctly
- [ ] Feedback animations work
- [ ] Audio plays on spelling activity

---

### Phase 4: Polish & Testing (Day 4 - 4 hours)

**Goal:** Production-ready application

| Task | Duration | Deliverable |
|------|----------|-------------|
| End-to-end integration tests | 1.5h | Complete session flow works |
| Cross-browser testing | 1h | Chrome, Firefox, Safari verified |
| Performance optimization | 0.5h | Fast page loads (<500ms) |
| Error handling polish | 0.5h | User-friendly error messages |
| Documentation | 0.5h | README, QUICKSTART updated |

**Testing Checklist:**
- [ ] Grade 3 complete session
- [ ] Grade 4 complete session
- [ ] Grade 5 complete session
- [ ] All activity types work correctly
- [ ] Network errors handled gracefully
- [ ] Progress persists correctly

---

## 5. Activity Implementations

### 5.1 Flashcard Activity

**Template: templates/activities/flashcard.html**
```html
{% extends "layouts/default.html" %}

{% block content %}
<div class="activity flashcard-activity">
  <h2 class="word-display">{{ activity.word.word }}</h2>

  <div class="definition-card">
    <p class="definition">{{ activity.word.definition }}</p>
    <p class="example"><em>{{ activity.word.example }}</em></p>
  </div>

  <button id="continue-btn" onclick="continueSession()">
    I understand! Continue
  </button>
</div>
{% endblock %}
```

### 5.2 Multiple Choice Activity

**Template: templates/activities/multiple-choice.html**
```html
{% extends "layouts/default.html" %}

{% block content %}
<div class="activity multiple-choice-activity">
  <h2>What does "{{ activity.word.word }}" mean?</h2>

  <div class="options-container">
    {% for option in activity.options %}
    <button class="option-button" onclick="selectOption('{{ option }}')">
      {{ option }}
    </button>
    {% endfor %}
  </div>

  <div id="feedback" class="feedback hidden"></div>
</div>
{% endblock %}
```

### 5.3 Spelling Activity

**Template: templates/activities/spelling.html**
```html
{% extends "layouts/default.html" %}

{% block content %}
<div class="activity spelling-activity">
  <h2>Listen and spell the word</h2>

  <audio id="word-audio" src="{{ activity.word.audio_url }}"></audio>

  <button id="play-audio-btn" onclick="playWordAudio()">
    🔊 Play Word
  </button>

  <input type="text" id="spelling-input" placeholder="Type the word here...">

  <button onclick="submitAnswer()">Submit</button>

  <div id="feedback" class="feedback hidden"></div>
</div>
{% endblock %}
```

### 5.4 Fill in the Blank Activity

**Template: templates/activities/fill-blank.html**
```html
{% extends "layouts/default.html" %}

{% block content %}
<div class="activity fill-blank-activity">
  <h2>Complete the sentence</h2>

  <p class="sentence">
    {{ activity.word.example | replace(activity.word.word, "______") }}
  </p>

  <input type="text" id="blank-input" placeholder="Fill in the blank">

  <button onclick="submitAnswer()">Submit</button>

  <div id="feedback" class="feedback hidden"></div>
</div>
{% endblock %}
```

### 5.5 Synonym/Antonym Activity

**Template: templates/activities/synonym-antonym.html**
```html
{% extends "layouts/default.html" %}

{% block content %}
<div class="activity synonym-antonym-activity">
  <h2>Select a {{ activity.relationship }} for "{{ activity.word.word }}"</h2>

  <div class="options-container">
    {% for option in activity.options %}
    <button class="option-button" onclick="selectOption('{{ option }}')">
      {{ option }}
    </button>
    {% endfor %}
  </div>

  <div id="feedback" class="feedback hidden"></div>
</div>
{% endblock %}
```

---

## 6. Deployment

### 6.1 Development Server

**scripts/start-dev.lisp**
```common-lisp
#!/usr/bin/env sbcl --script

(load "~/.sbclrc") ; Load Quicklisp
(ql:quickload :vocab-game)
(in-package :vocab-game)

(setf *debug-mode* t)
(start-server :port 5000)

;; Keep running
(loop (sleep 1))
```

Run: `chmod +x scripts/start-dev.lisp && ./scripts/start-dev.lisp`

### 6.2 Production Build

**Dockerfile**
```dockerfile
FROM clfoundation/sbcl:2.4.0

WORKDIR /app
COPY . /app

# Install dependencies
RUN sbcl --eval "(ql:quickload :vocab-game)" --quit

# Build executable
RUN sbcl --eval "(ql:quickload :vocab-game)" \
         --eval "(sb-ext:save-lisp-and-die \"vocab-game-server\" \
                   :toplevel #'vocab-game:start-server \
                   :executable t \
                   :compression 9)"

EXPOSE 5000

CMD ["./vocab-game-server"]
```

Build: `docker build -t vocab-game-cl .`
Run: `docker run -p 5000:5000 -e PLAYCADEMY_API_KEY=xxx vocab-game-cl`

### 6.3 Environment Variables

```bash
# Required
export PLAYCADEMY_API_URL="https://api.playcademy.net"
export PLAYCADEMY_API_KEY="your-api-key-here"
export SESSION_SECRET="random-secret-for-session-encryption"

# Optional
export PORT=5000
export DEBUG=false
```

---

## 7. Testing Strategy

### 7.1 Unit Tests

**tests/api-client-tests.lisp**
```common-lisp
(in-package #:vocab-game-tests)

(def-suite api-client-suite
  :description "Tests for Playcademy API client")

(in-suite api-client-suite)

(test start-session-success
  "Test successful session creation"
  (let ((session (vocab-game:start-session "test-user" 3)))
    (is (not (null (vocab-game:game-session-session-id session))))
    (is (= (vocab-game:game-session-grade session) 3))
    (is (> (length (vocab-game:game-session-activities session)) 0))))

(test submit-attempt-correct
  "Test submitting correct answer"
  (let* ((session (vocab-game:start-session "test-user" 3))
         (activity (vocab-game:get-current-activity session))
         (correct-answer (vocab-game:word-data-word
                          (vocab-game:activity-word activity)))
         (result (vocab-game:submit-attempt
                  (vocab-game:game-session-session-id session)
                  0
                  correct-answer)))
    (is (vocab-game:attempt-result-correct result))))

(run! 'api-client-suite)
```

### 7.2 Integration Tests

**Manual Test Checklist:**
- [ ] Complete Grade 3 session (start to results)
- [ ] Complete Grade 4 session
- [ ] Complete Grade 5 session
- [ ] Test each activity type individually
- [ ] Test incorrect answers (feedback displays correctly)
- [ ] Test network error handling (disconnect internet)
- [ ] Test session timeout handling
- [ ] Verify progress persistence

---

## 8. Success Criteria

### 8.1 Functional Requirements
- [ ] All 5 activity types functional
- [ ] Complete session flow (start → activities → results → progress)
- [ ] Real-time feedback (correct/incorrect)
- [ ] Audio playback for spelling activity
- [ ] Score calculation accurate
- [ ] Progress tracking works

### 8.2 Technical Requirements
- [ ] Page load time < 500ms
- [ ] API response time < 2s
- [ ] No JavaScript errors in console
- [ ] Works on Chrome, Firefox, Safari
- [ ] Mobile responsive design
- [ ] WCAG 2.1 AA accessibility

### 8.3 Code Quality
- [ ] All unit tests pass
- [ ] No compiler warnings
- [ ] Code follows Common Lisp style guide
- [ ] Documentation complete (docstrings)
- [ ] README with setup instructions

---

## 9. Comparison: Godot vs Common Lisp

| Feature | Godot/GDScript | Common Lisp Web App |
|---------|----------------|---------------------|
| **Deployment** | WASM export (~5MB) | Native web server |
| **Performance** | 60fps game loop | Fast HTTP responses |
| **Type Safety** | Optional hints | Strong static typing |
| **Development** | Visual editor | REPL + text editor |
| **Hot Reload** | ✅ Scene editor | ✅ Live REPL updates |
| **Debugging** | Visual debugger | SLIME debugger |
| **Learning Curve** | Moderate | Steep (Lisp knowledge) |
| **Web Standards** | Custom engine | Pure HTML/CSS/JS |
| **SEO** | Limited | Full server-side rendering |
| **Backend Integration** | HTTP client | Native HTTP server |

**When to Use Common Lisp:**
- ✅ Server-side rendered web apps
- ✅ API-heavy applications
- ✅ Need strong typing and REPL development
- ✅ Want standard web deployment
- ❌ Need rich animations/game feel
- ❌ Team unfamiliar with Lisp

---

## 10. Next Steps

### Immediate Actions
1. **Set up development environment**
   - Install SBCL or CCL
   - Install Quicklisp
   - Clone repository

2. **Verify Playcademy API access**
   - Get API credentials
   - Test endpoints with curl
   - Review API documentation

3. **Begin Phase 1 implementation**
   - Create ASDF system
   - Implement data models
   - Build API client

### Week 1 Goals
- [ ] Complete Phase 1 (Core Infrastructure)
- [ ] Complete Phase 2 (Web Server & Routing)

### Week 2 Goals
- [ ] Complete Phase 3 (Frontend & Templates)
- [ ] Complete Phase 4 (Polish & Testing)
- [ ] Deploy to production

---

## 11. References

### Common Lisp Resources
- [Practical Common Lisp](http://www.gigamonkeys.com/book/) - Free online book
- [Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/) - Practical recipes
- [Caveman2 Documentation](https://github.com/fukamachi/caveman)
- [Parenscript Manual](https://parenscript.common-lisp.dev/)

### Playcademy Integration
- Playcademy API Documentation: `docs.playcademy.net`
- Backend API Contract: See `playcademy-vocab-prd.md`
- Godot Reference Implementation: `vocab_game/` directory

---

**Document Status:** Ready for Implementation
**Last Updated:** 2025-11-07
**Version:** 1.0
