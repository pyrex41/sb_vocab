# Playcademy Vocab - Gauntlet Cohort 3 Project

Playcademy provides a complete developer toolchain for building educational games, including:
- Backend API routes
- Database (Drizzle)
- Bucket storage
- Session management
- Deployment tooling

Several games have been built using this stack internally, but haven't tested it with external developers working under time constraints.

For Gauntlet Cohort 3, we're proposing a vocabulary learning application as a validation exercise for our developer experience.

## The Project

Build a vocabulary trainer for elementary students (grades 3-5) that implements spaced repetition learning. Students complete short sessions mixing new word introduction with timed reviews.

You'll be provided with a GitHub repository containing a fully functional backend with REST APIs. Your task is to build a Godot frontend that consumes these APIs and creates an engaging learning experience.

## Backend (Already Built)

The GitHub repository includes complete REST APIs:

- `POST /session/start`: Starts a new session and returns planned queue of words to practice with pre-determined activity types.
- `POST /session/attempt`: Scores student input, manages FSRS state.
- `POST /session/end`: Finalizes progress.
- `GET /progress`: Returns stats and mastery levels.

The backend handles FSRS scheduling (we use `ts-fsrs`) and stores ~350 words per grade with definitions/sentences/audio. All scoring is managed server-side to prevent cheating.

## Frontend (You Build)

Using Godot and our GDScript PlaycademySdk, implement the session flow and 4-6 core activities:

- Flashcard introduction (word + definition + example)
- Meaning multiple choice
- Dictated spelling (type what you hear)
- Context fill-in-the-blank
- Synonym/antonym selection
- Sentence generation

The Godot SDK provides: `PlaycademySdk.backend.request()` for API calls, user auth, and session state.

Our CLI handles local development, project configuration, and eventual deployment.

## What You Get

The repository includes:
- Complete backend implementation with REST APIs
- TypeScript SDK with Godot bindings
- Local dev environment setup

Comprehensive documentation at [docs.playcademy.net](https://docs.playcademy.net) includes Godot-specific guides, SDK reference with GDScript examples, backend integration patterns, and CLI commands for local development and deployment.

## Scope for 3-4 Days

- Playable session flow: tap "Start Session," complete 10-minute vocab drill
- All activities implemented
- Basic progress display
- Age-appropriate UI

This tests whether our docs, SDK, and tooling enable external developers to ship a functional educational game in under a week.

## Defined Deliverables

1. **Updated GitHub Repository**: Push all Godot frontend code to the `dev` branch of the provided repository.
2. **Deployed Application**: Deploy the completed app to Playcademy staging environment using the `playcademy deploy` CLI command.
3. **Feedback Report**: Document your experience working with our platform, including any pain points, unclear documentation, or suggestions for improvement.

## Getting Started

### 1. Clone the Repository

Clone the provided GitHub repository containing the backend and project structure.

### 2. Setup Godot Environment

Install the Playcademy SDK and configure your Godot project:

**Install from AssetLib:**
1. Open the AssetLib tab in Godot editor.
2. Search for "Playcademy".
3. Install the bundle (adds `addons/playcademy/` to your project).

**Enable Plugins:**
1. Go to `Project > Project Settings > Plugins`.
2. Enable:
   - Playcademy Manifest Exporter (for deployment)
   - Playcademy Sandbox (for local development)

**Setup AutoLoad:**
1. Go to `Project > Project Settings > Globals > Autoload`.
2. Add `res://addons/playcademy/sdk/PlaycademySDK.gd`.
3. Name it `PlaycademySdk`.

For complete Godot integration details, see the [Godot Platform Guide](https://docs.playcademy.net/platform-guides/godot).

### 3. Build Your Frontend

Do your thing.

### 4. Export Your Game

Configure and export your Godot project for web deployment:

**Configure Web Export Preset:**
1. In Godot, go to `Project > Export`.
2. Select the "Web (Runnable)" preset (or "Add..." it if not already present).
3. Set `Custom HTML Shell` to: `res://addons/playcademy/shell.html`.
4. Export to Web (Runnable).

The Manifest Exporter plugin automatically generates `<game-name>.zip`.

### 5. Deploy to Staging

Deploy your exported game using the Playcademy CLI:

```bash
# In the root of your project
playcademy deploy
```
