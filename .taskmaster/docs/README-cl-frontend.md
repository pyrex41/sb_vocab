# Common Lisp Frontend Implementation - Overview

**Project:** Playcademy Vocabulary Game - Common Lisp Web Frontend
**Status:** Planning Complete, Ready for Development
**Timeline:** 4 days (24 hours)
**Created:** 2025-11-07

---

## Quick Links

📘 **[Full PRD](./prd-common-lisp-implementation.md)** - Complete technical specification
📋 **[Task Breakdown](./tasks-cl-implementation.md)** - Detailed task list with hours
🚀 **[Quick Start Guide](./quickstart-cl.md)** - Get started in 15 minutes

---

## What We're Building

A **complete replacement** of the Godot/GDScript frontend using **Common Lisp**, providing:

- ✅ Native web application (no game engine overhead)
- ✅ Same functionality as Godot version (5 activity types)
- ✅ Direct integration with Playcademy REST API
- ✅ Server-side rendering with Djula templates
- ✅ Type-safe client-side JavaScript (via Parenscript)
- ✅ REPL-driven development workflow

---

## Technology Stack

| Component | Technology | Why? |
|-----------|-----------|------|
| **Web Server** | Caveman2 + Clack | Modern Lisp web framework |
| **Templates** | Djula | Django-style HTML templates |
| **Client JS** | Parenscript | Type-safe Lisp→JavaScript |
| **API Client** | Dexador | HTTP client for backend |
| **JSON** | Jonathan | Fast JSON parsing |
| **Testing** | FiveAM | Unit/integration tests |

---

## Project Structure

```
vocab-game-cl/
├── vocab-game.asd          # System definition
├── src/
│   ├── package.lisp        # Package definitions
│   ├── config.lisp         # Configuration
│   ├── models.lisp         # Data structures
│   ├── api-client.lisp     # Playcademy API client
│   ├── session.lisp        # Session management
│   ├── routes.lisp         # Web routes
│   ├── parenscript-frontend.lisp  # Client-side code
│   └── main.lisp           # Entry point
├── templates/              # Djula HTML templates
├── static/                 # CSS, JS, images
└── tests/                  # Unit tests
```

---

## Implementation Timeline

### Phase 1: Core Infrastructure (Day 1 - 6h)
- Set up ASDF system
- Implement data models
- Build API client (4 endpoints)
- Write unit tests

**Deliverable:** Working API client

### Phase 2: Web Server & Routing (Day 2 - 6h)
- Set up Caveman2 application
- Implement session management
- Create all routes (6 endpoints)
- Add error handling

**Deliverable:** Functioning web server

### Phase 3: Frontend & Templates (Day 3 - 8h)
- Create Djula templates (7 pages)
- Implement Parenscript client code
- CSS styling (age-appropriate UI)
- Audio playback integration

**Deliverable:** Complete UI

### Phase 4: Polish & Testing (Day 4 - 4h)
- End-to-end integration tests
- Cross-browser testing
- Performance optimization
- Documentation updates

**Deliverable:** Production-ready app

---

## Key Features

### Activity Types (All 5 from Godot version)
1. **Flashcard** - Word introduction
2. **Multiple Choice** - Definition selection
3. **Spelling** - Audio-based word spelling
4. **Fill in the Blank** - Contextual usage
5. **Synonym/Antonym** - Word relationships

### Technical Features
- **Session Management** - Track user progress through activities
- **Score Calculation** - Real-time feedback and scoring
- **Progress Tracking** - FSRS-based spaced repetition
- **Audio Playback** - Native HTML5 audio for spelling
- **Responsive Design** - Works on desktop, tablet, mobile
- **Error Handling** - Graceful network error recovery

---

## Development Workflow

### Setup (15 minutes)
```bash
# 1. Install SBCL
brew install sbcl  # macOS
# apt install sbcl  # Linux

# 2. Install Quicklisp
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)"

# 3. Clone and load
cd vocab-game-cl
sbcl --eval "(ql:quickload :vocab-game)"
```

### Development Server
```common-lisp
;; Start REPL
(ql:quickload :vocab-game)
(in-package :vocab-game)

;; Start server
(start-server :port 5000)

;; Visit http://localhost:5000

;; Hot reload on changes
(reload-app)
```

### Testing
```common-lisp
;; Run all tests
(asdf:test-system :vocab-game)

;; Run specific suite
(fiveam:run! 'vocab-game-tests:api-client-suite)
```

---

## API Integration Points

### Playcademy Backend Endpoints

| Endpoint | Method | Purpose | Implementation File |
|----------|--------|---------|-------------------|
| `/session/start` | POST | Begin new session | `api-client.lisp:start-session` |
| `/session/attempt` | POST | Submit answer | `api-client.lisp:submit-attempt` |
| `/session/end` | POST | Finalize session | `api-client.lisp:end-session` |
| `/progress` | GET | Get user stats | `api-client.lisp:get-progress` |

All endpoints return JSON, parsed into Common Lisp structs.

---

## Advantages Over Godot

| Aspect | Godot | Common Lisp |
|--------|-------|-------------|
| **Deployment** | WASM export | Standard web server |
| **Type Safety** | Optional | Strong static typing |
| **Development** | Visual editor | REPL live coding |
| **SEO** | Limited | Full server-side rendering |
| **Backend** | HTTP client | Native HTTP server |
| **Learning Curve** | Moderate | Steep (requires Lisp) |

**When to Use CL:**
- ✅ API-heavy web applications
- ✅ Need strong type safety
- ✅ REPL-driven development preferred
- ✅ Standard web deployment required

**When to Use Godot:**
- ✅ Rich animations and game feel
- ✅ Visual scene editing needed
- ✅ Team unfamiliar with Lisp

---

## Success Metrics

### Functional
- [ ] All 5 activities work correctly
- [ ] Complete session flow (start → results)
- [ ] Progress tracking accurate
- [ ] Audio playback functional

### Technical
- [ ] Page load < 500ms
- [ ] API response < 2s
- [ ] Zero console errors
- [ ] Works on Chrome, Firefox, Safari

### Code Quality
- [ ] All tests pass
- [ ] No compiler warnings
- [ ] Complete documentation
- [ ] Follows CL style guide

---

## Resources

### Documentation
- **[Full PRD](./prd-common-lisp-implementation.md)** - Complete specification
- **[Task List](./tasks-cl-implementation.md)** - Hour-by-hour breakdown
- **[API Reference](./playcademy-quick-reference.md)** - Backend API docs

### Learning Resources
- [Practical Common Lisp](http://www.gigamonkeys.com/book/) - Free book
- [Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/)
- [Caveman2 Guide](https://github.com/fukamachi/caveman)
- [Parenscript Manual](https://parenscript.common-lisp.dev/)

### Reference Implementation
- **Godot Version:** `../vocab_game/` directory
- **Backend API:** See `playcademy-vocab-prd.md`

---

## Next Actions

### Today
1. ✅ Review PRD (this document)
2. ✅ Set up development environment (SBCL + Quicklisp)
3. ✅ Verify Playcademy API access (get credentials)

### Tomorrow (Phase 1)
1. Create ASDF system definition
2. Implement data models (`models.lisp`)
3. Build API client (`api-client.lisp`)
4. Write unit tests

### Day 3 (Phase 2)
1. Set up Caveman2 web server
2. Implement session management
3. Create all routes

### Day 4 (Phase 3)
1. Create Djula templates
2. Implement Parenscript client code
3. Add CSS styling

### Day 5 (Phase 4)
1. Integration testing
2. Cross-browser testing
3. Deploy to production

---

## Questions?

- **Technical questions:** Review [Full PRD](./prd-common-lisp-implementation.md)
- **Task breakdown:** See [Task List](./tasks-cl-implementation.md)
- **Setup help:** Check [Quick Start Guide](./quickstart-cl.md)
- **API details:** Reference [Playcademy PRD](./playcademy-vocab-prd.md)

---

**Status:** 📋 Planning Complete → 🚀 Ready to Start Development
**Last Updated:** 2025-11-07
