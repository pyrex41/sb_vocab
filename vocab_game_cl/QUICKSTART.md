# Quickstart Guide - Playcademy Vocabulary Game (Common Lisp)

Get the vocabulary game running in 5 minutes!

## Prerequisites Check

✅ Common Lisp (SBCL) installed
✅ Quicklisp installed
✅ Playcademy backend running at `http://localhost:8788`

## Step 1: Install SBCL and Quicklisp (if needed)

### Ubuntu/Debian
```bash
sudo apt-get install sbcl
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
echo '(load "~/quicklisp/setup.lisp")' >> ~/.sbclrc
```

### macOS
```bash
brew install sbcl
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
echo '(load "~/quicklisp/setup.lisp")' >> ~/.sbclrc
```

## Step 2: Start the Server

### Option A: From REPL (Recommended for Development)

```bash
cd vocab_game_cl
sbcl
```

Then in the REPL:
```lisp
(ql:quickload :vocab-game-cl)
(in-package :vocab-game-cl)
(start-server)
```

### Option B: Direct Execution

```bash
cd vocab_game_cl
sbcl --eval '(ql:quickload :vocab-game-cl)' --eval '(vocab-game-cl:main)'
```

## Step 3: Play!

Open your browser to: **http://localhost:8080**

## Quick Commands

### Stop Server
```lisp
(stop-server)
```

### Restart Server
```lisp
(restart-server)
```

### Change Port
```lisp
(start-server :port 8081)
```

### Check Backend Connection
```lisp
(api-authenticate *default-user-email* *default-user-password*)
```

## Troubleshooting

### "Port already in use"
```lisp
(stop-server)
(start-server :port 8081)
```

### "Cannot connect to backend"
1. Ensure backend is running: `curl http://localhost:8788/api/content/course`
2. Check config: `(format t "~A" *api-base-url*)`

### "Authentication failed"
Update credentials in `src/config.lisp`:
```lisp
(defparameter *default-user-email* "your-email@example.com")
(defparameter *default-user-password* "your-password")
```

## Configuration

Edit `src/config.lisp`:

```lisp
;; Backend API URL
(defparameter *api-base-url* "http://localhost:8788/api")

;; Server port
(defparameter *web-server-port* 8080)

;; User credentials
(defparameter *default-user-email* "student.fresh@demo.playcademy.com")
(defparameter *default-user-password* "password")
```

## Game Flow

1. **Select Grade** → Choose 3, 4, or 5
2. **Complete Activities** → 10-15 per session
3. **View Results** → See your score
4. **Track Progress** → Monitor learning

## Development Tips

### Hot Reload
After editing code:
```lisp
(ql:quickload :vocab-game-cl :force t)
(restart-server)
```

### Debug Mode
```lisp
(setf hunchentoot:*log-lisp-errors-p* t)
```

### Test API Calls
```lisp
;; Get courses
(api-get-courses)

;; Start session
(start-new-session "course-id")

;; Load activity
(load-next-activity)
```

## Next Steps

- Read the full [README.md](README.md)
- Explore the code in `src/`
- Customize activities in `src/activities.lisp`
- Modify styles in `static/css/style.css`

---

**Ready to learn vocabulary? Start the server and visit http://localhost:8080!**
