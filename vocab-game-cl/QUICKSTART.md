# Quick Start Guide - Playcademy Vocabulary Game

Get up and running in 15 minutes!

## Step 1: Install SBCL (5 minutes)

### macOS
```bash
brew install sbcl
```

### Linux (Debian/Ubuntu)
```bash
sudo apt install sbcl
```

### Verify Installation
```bash
sbcl --version
```

## Step 2: Install Quicklisp (3 minutes)

```bash
# Download Quicklisp
curl -O https://beta.quicklisp.org/quicklisp.lisp

# Install Quicklisp
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)"

# Add to init file (makes Quicklisp load automatically)
sbcl --eval "(ql:add-to-init-file)"
```

## Step 3: Set Up Environment (2 minutes)

Create a `.env` file or export variables:

```bash
export PLAYCADEMY_API_URL="https://api.playcademy.net"
export PORT=5000
export DEBUG=true
```

## Step 4: Load and Run (5 minutes)

```bash
cd vocab-game-cl
sbcl
```

In the SBCL REPL:

```common-lisp
;; Load the application
(ql:quickload :vocab-game)

;; Switch to the application package
(in-package :vocab-game)

;; Start the server
(start-server :port 5000)
```

## Step 5: Visit the Application

Open your browser and go to:

```
http://localhost:5000
```

🎉 **You're done!**

## Development Tips

### Hot Reload

After making changes to the code:

```common-lisp
(reload-app)
```

### Stop the Server

```common-lisp
(stop-server)
```

### View Logs

```common-lisp
(log:config :info)  ; Set log level
```

## Common Issues

### Port Already in Use

```common-lisp
;; Use a different port
(start-server :port 5001)
```

### Dependencies Not Loading

```common-lisp
;; Update Quicklisp
(ql:update-all-dists)

;; Try loading again
(ql:quickload :vocab-game)
```

### Template Not Found

Make sure you're in the `vocab-game-cl` directory when starting SBCL.

## Next Steps

- Read the full [README.md](README.md)
- Check out the [API documentation](../api_docs/API_ROUTES.md)
- Review the [PRD](../.taskmaster/docs/prd-common-lisp-implementation.md) for architecture details

## Getting Help

- Check server logs in the REPL
- Review `DEV_NOTES.md` for troubleshooting
- Open an issue on GitHub

Happy coding! 🚀
