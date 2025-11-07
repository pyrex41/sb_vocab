# Playcademy Vocabulary Game - Common Lisp Implementation

A vocabulary learning application for elementary students (grades 3-5) built with Common Lisp, featuring interactive activities and integration with the Playcademy backend platform.

## 🎯 Features

- **5 Activity Types**
  - 📖 Flashcard: Introduction to new words
  - ✅ Multiple Choice: Definition matching
  - ✏️ Spelling: Type the word you hear
  - 📝 Fill in the Blank: Complete sentences
  - 🔄 Synonym/Antonym: Word relationship matching

- **Complete Learning Flow**
  - Grade-based course selection (Grades 3-5)
  - Session management with progress tracking
  - Real-time feedback and scoring
  - Comprehensive progress statistics

- **Modern Web Interface**
  - Responsive design for desktop and tablet
  - Age-appropriate UI (large fonts, bright colors)
  - Smooth animations and transitions
  - Playcademy brand styling

## 🏗️ Architecture

### Technology Stack

- **Backend**: Common Lisp
  - Web Server: Hunchentoot
  - HTML Generation: CL-WHO
  - JSON Handling: Jonathan
  - HTTP Client: Dexador
  - JavaScript Generation: Parenscript

- **Frontend**: HTML5, CSS3, Vanilla JavaScript
  - No framework dependencies
  - Progressive enhancement
  - Mobile-friendly responsive design

### Project Structure

```
vocab_game_cl/
├── vocab-game-cl.asd          # ASDF system definition
├── README.md                  # This file
├── src/                       # Source code
│   ├── package.lisp          # Package definitions
│   ├── config.lisp           # Configuration settings
│   ├── utils.lisp            # Utility functions
│   ├── api-client.lisp       # Playcademy API client
│   ├── session-manager.lisp  # Game session management
│   ├── activities.lisp       # Activity rendering
│   ├── web-server.lisp       # HTTP handlers
│   └── main.lisp             # Entry point
└── static/                    # Static assets
    ├── css/
    │   └── style.css         # Stylesheet
    └── js/
        └── game.js           # Client-side JavaScript
```

## 📋 Prerequisites

- **Common Lisp Implementation**
  - SBCL (recommended), CCL, or CLISP
  - Quicklisp for dependency management

- **Playcademy Backend API**
  - Backend server running at `http://localhost:8788/api`
  - Valid user credentials (configured in `config.lisp`)

## 🚀 Installation

### 1. Install Common Lisp

**On Ubuntu/Debian:**
```bash
sudo apt-get install sbcl
```

**On macOS:**
```bash
brew install sbcl
```

**On Windows:**
Download from http://www.sbcl.org/platform-table.html

### 2. Install Quicklisp

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
```

Add to your `~/.sbclrc`:
```lisp
(load "~/quicklisp/setup.lisp")
```

### 3. Clone and Load the Project

```bash
cd vocab_game_cl
sbcl
```

In the SBCL REPL:
```lisp
;; Load the system
(ql:quickload :vocab-game-cl)

;; Start the server
(vocab-game-cl:start-server)
```

### 4. Access the Application

Open your browser to: **http://localhost:8080**

## 🎮 Usage

### Starting the Server

**From the REPL:**
```lisp
(ql:quickload :vocab-game-cl)
(vocab-game-cl:start-server)
```

**From the command line:**
```bash
sbcl --eval '(ql:quickload :vocab-game-cl)' \
     --eval '(vocab-game-cl:main)'
```

### Stopping the Server

```lisp
(vocab-game-cl:stop-server)
```

Or press `Ctrl+C` in the terminal.

### Configuration

Edit `src/config.lisp` to change:

- **API Base URL**: `*api-base-url*`
- **Server Port**: `*web-server-port*`
- **User Credentials**: `*default-user-email*`, `*default-user-password*`

## 🎯 Game Flow

### 1. Main Menu
- Select grade level (3, 4, or 5)
- View progress statistics
- Start new learning session

### 2. Learning Session
- Complete 10-15 activities per session
- Progress bar shows completion
- Immediate feedback on each answer
- Auto-advance after correct/incorrect responses

### 3. Results Screen
- View accuracy percentage
- See correct answer count
- Compare performance
- Continue to next session

### 4. Progress Tracking
- Total words learned
- Overall accuracy
- Sessions completed
- Detailed statistics

## 🔌 API Integration

### Authentication
```lisp
(api-authenticate email password)
```

### Session Management
```lisp
;; Start a new session
(start-new-session course-id)

;; Load next activity
(load-next-activity)

;; Submit an answer
(submit-answer answer-text)

;; End session
(end-session)

;; Get progress
(get-progress)
```

### API Endpoints

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/auth/sign-in/email` | POST | Authenticate user |
| `/content/course` | GET | Get available courses |
| `/session/start` | POST | Start learning session |
| `/session/:id/next` | POST | Get next activity |
| `/session/:id/attempt` | POST | Submit answer |
| `/session/end` | POST | End session |
| `/progress` | GET | Get user progress |

## 🧪 Development

### REPL-Driven Development

```lisp
;; Load the system
(ql:quickload :vocab-game-cl)

;; Switch to the package
(in-package :vocab-game-cl)

;; Test API calls
(api-authenticate *default-user-email* *default-user-password*)

;; Test session creation
(start-new-session "course-id-here")

;; Restart server after changes
(restart-server)
```

### Adding New Activity Types

1. Add activity rendering in `src/activities.lisp`:
```lisp
(defun render-my-activity (activity-data)
  (with-html-output-to-string (s)
    (htm
     (:div :class "activity my-activity"
           ;; Your HTML here
           ))))
```

2. Update `render-activity` function to handle the new type

3. Add client-side handler in `static/js/game.js`:
```javascript
function renderMyActivity(word, options) {
    // Return HTML string
}
```

### Debugging

Enable debug logging:
```lisp
(setf hunchentoot:*log-lisp-errors-p* t)
(setf hunchentoot:*log-lisp-warnings-p* t)
```

View logs in the REPL output.

## 🐛 Troubleshooting

### Server Won't Start

**Issue**: Port already in use

**Solution**:
```lisp
;; Change port
(start-server :port 8081)

;; Or kill existing process
(stop-server)
```

### Cannot Connect to Backend

**Issue**: Backend API not running

**Solution**:
1. Ensure backend is running at `http://localhost:8788`
2. Check `*api-base-url*` in `config.lisp`
3. Test connection: `curl http://localhost:8788/api/content/course`

### Authentication Fails

**Issue**: Invalid credentials

**Solution**:
- Update credentials in `src/config.lisp`
- Verify user exists in backend database

### Static Files Not Loading

**Issue**: 404 errors for CSS/JS

**Solution**:
```lisp
;; Ensure static handler is set up
(setup-static-handlers)
```

## 📦 Dependencies

- **hunchentoot** - Web server
- **cl-who** - HTML generation
- **parenscript** - JavaScript generation
- **dexador** - HTTP client
- **jonathan** - JSON encoding/decoding
- **alexandria** - Utility library
- **cl-ppcre** - Regular expressions
- **local-time** - Timestamp handling
- **bordeaux-threads** - Thread portability

## 🔒 Security Notes

- Session cookies are used for authentication
- HTTPS should be enabled in production
- User input is sanitized before rendering
- CSRF protection should be added for production

## 🚢 Deployment

### Building a Binary

```bash
sbcl --eval '(ql:quickload :vocab-game-cl)' \
     --eval '(save-lisp-and-die "vocab-game" :toplevel #'"'"'vocab-game-cl:main :executable t)'
```

### Running the Binary

```bash
./vocab-game
```

### Docker Deployment

Create `Dockerfile`:
```dockerfile
FROM clfoundation/sbcl:latest

WORKDIR /app
COPY . .

RUN sbcl --eval '(ql:quickload :vocab-game-cl)' --quit

EXPOSE 8080

CMD ["sbcl", "--eval", "(ql:quickload :vocab-game-cl)", "--eval", "(vocab-game-cl:main)"]
```

Build and run:
```bash
docker build -t vocab-game-cl .
docker run -p 8080:8080 vocab-game-cl
```

## 📝 License

This project is part of the Playcademy vocabulary learning platform.

## 🤝 Contributing

This is a reference implementation for the Playcademy platform integration.

## 📧 Support

For issues and questions, please refer to the Playcademy documentation at https://docs.playcademy.net

## 🎓 Learning Resources

- **Common Lisp**: http://www.gigamonkeys.com/book/
- **Hunchentoot**: https://edicl.github.io/hunchentoot/
- **CL-WHO**: https://edicl.github.io/cl-who/
- **Quicklisp**: https://www.quicklisp.org/

## 🏆 Acknowledgments

Based on the Playcademy Vocabulary Game specification and the Godot implementation at `vocab_game/`.

---

**Version**: 1.0.0
**Last Updated**: November 7, 2025
**Implementation**: Common Lisp with Hunchentoot
