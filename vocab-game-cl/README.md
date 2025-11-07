# Playcademy Vocabulary Game - Common Lisp Frontend

A complete web-based vocabulary learning application built with Common Lisp, integrating with the Playcademy backend API.

## Features

- 🎮 **5 Activity Types**: Flashcard, Multiple Choice, Spelling, Fill in the Blank, Synonym/Antonym
- 📊 **Progress Tracking**: Monitor learning progress with FSRS-based spaced repetition
- 🎨 **Responsive Design**: Works on desktop, tablet, and mobile
- ⚡ **Real-time Feedback**: Instant scoring and feedback on answers
- 🔊 **Audio Support**: Native HTML5 audio for pronunciation practice
- 🚀 **REPL-Driven Development**: Live coding with instant hot reload

## Technology Stack

| Component | Technology | Purpose |
|-----------|-----------|---------|
| **Web Framework** | Caveman2 | HTTP routing & request handling |
| **HTTP Server** | Clack + Hunchentoot | Web server interface |
| **Templates** | Djula | Server-side HTML rendering |
| **Client-Side JS** | Parenscript | Type-safe Lisp→JavaScript |
| **HTTP Client** | Dexador | Playcademy API calls |
| **JSON** | Jonathan | Fast JSON parsing |
| **Testing** | FiveAM | Unit & integration tests |

## Quick Start

### Prerequisites

- **SBCL** (Steel Bank Common Lisp) or **CCL** (Clozure CL)
- **Quicklisp** package manager

### Installation

#### 1. Install SBCL

**macOS:**
```bash
brew install sbcl
```

**Linux (Debian/Ubuntu):**
```bash
sudo apt install sbcl
```

#### 2. Install Quicklisp

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)"
```

Add Quicklisp to your SBCL init file:
```bash
sbcl --eval "(ql:add-to-init-file)"
```

### Running the Application

#### 1. Load the System

```bash
cd vocab-game-cl
sbcl
```

```common-lisp
(ql:quickload :vocab-game)
(in-package :vocab-game)
```

#### 2. Configure Environment

Set environment variables or use defaults:

```bash
export PLAYCADEMY_API_URL="https://api.playcademy.net"
export PLAYCADEMY_API_KEY="your-api-key"
export PORT=5000
export DEBUG=true
```

#### 3. Start the Server

```common-lisp
(start-server :port 5000)
```

Visit http://localhost:5000 in your browser!

#### 4. Hot Reload (Development)

```common-lisp
(reload-app)  ; Reload after making changes
```

## Project Structure

```
vocab-game-cl/
├── vocab-game.asd              # ASDF system definition
├── README.md                   # This file
├── src/
│   ├── package.lisp            # Package definitions
│   ├── config.lisp             # Configuration (env vars)
│   ├── models.lisp             # Data structures
│   ├── api-client.lisp         # Playcademy API client
│   ├── session.lisp            # Session management
│   ├── routes.lisp             # Web routes (controllers)
│   ├── parenscript-frontend.lisp  # Client-side Lisp→JS
│   └── main.lisp               # Entry point
├── templates/
│   ├── layouts/default.html    # Base layout
│   ├── index.html              # Main menu
│   ├── activities/             # 5 activity templates
│   ├── results.html            # Session summary
│   └── progress.html           # User progress
├── static/
│   ├── css/                    # Stylesheets
│   └── js/                     # Generated JavaScript
└── tests/                      # Unit tests
```

## API Endpoints

### Frontend Routes

- `GET /` - Main menu (course selection)
- `POST /start-session` - Start a new session
- `GET /game` - Display current activity
- `POST /submit-answer` - Submit an answer
- `GET /results` - View session results
- `GET /progress` - View user progress

### Backend Integration

The application integrates with the Playcademy backend:

- `POST /v1/session/start` - Initialize session
- `POST /v1/session/:id/next` - Get next activity
- `POST /v1/session/:id/attempt` - Submit attempt
- `POST /v1/session/:id/finalize` - End session
- `GET /v1/me/progress/course/:id` - Get progress

## Development

### REPL Workflow

```common-lisp
;; Load the system
(ql:quickload :vocab-game)
(in-package :vocab-game)

;; Start server
(start-server)

;; Make changes to code...

;; Reload application
(reload-app)

;; Stop server
(stop-server)
```

### Running Tests

```common-lisp
(asdf:test-system :vocab-game)
```

## Configuration Options

| Variable | Default | Description |
|----------|---------|-------------|
| `PLAYCADEMY_API_URL` | `https://api.playcademy.net` | Backend API URL |
| `PLAYCADEMY_API_KEY` | - | API authentication key |
| `PORT` | `5000` | Web server port |
| `DEBUG` | `false` | Enable debug mode |
| `SESSION_SECRET` | - | Session encryption key |

## Deployment

### Docker (Recommended)

```dockerfile
FROM clfoundation/sbcl:latest
WORKDIR /app
COPY . .
RUN sbcl --eval "(ql:quickload :vocab-game)" --quit
CMD sbcl --eval "(ql:quickload :vocab-game)" \
         --eval "(vocab-game:start-server)" \
         --eval "(loop (sleep 1))"
```

### VPS / Heroku

```bash
# Start in production mode
sbcl --eval "(ql:quickload :vocab-game)" \
     --eval "(vocab-game:start-server :port 8080)" \
     --eval "(loop (sleep 1))"
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Write tests
5. Submit a pull request

## License

MIT License - see LICENSE file for details

## Support

For questions or issues:
- GitHub Issues: [Create an issue]
- Documentation: See `.taskmaster/docs/` for detailed specs
- API Reference: `api_docs/API_ROUTES.md`

## Acknowledgments

- Built with [Caveman2](https://github.com/fukamachi/caveman)
- Templates powered by [Djula](https://github.com/mmontone/djula)
- Client-side code via [Parenscript](https://parenscript.common-lisp.dev/)
- Integrates with Playcademy backend API

---

**Created:** 2025-11-07
**Status:** Production Ready
**Version:** 1.0.0
