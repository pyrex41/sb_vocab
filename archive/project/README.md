# Playcademy Vocab - Gauntlet Cohort 3

A vocabulary learning application for elementary students (grades 3-5) using spaced repetition learning.

## Project Structure

```
project/
├── godot/              # Godot frontend application
│   ├── project.godot
│   ├── scenes/
│   ├── scripts/
│   ├── assets/
│   └── addons/         # Playcademy SDK (mock for now)
├── backend/            # Mock backend API
│   ├── api.ts
│   └── mock_data.ts
└── docs/              # Documentation
```

## Quick Start

### Backend (Mock)
```bash
cd backend
npm install
npm run dev
```

### Godot Frontend
1. Open `godot/` directory in Godot Editor
2. Enable Playcademy plugins (Project > Project Settings > Plugins)
3. Play the main scene

## Features

- Spaced repetition learning with FSRS scheduling
- Interactive vocabulary activities
- Progress tracking
- Age-appropriate UI for grades 3-5

## Activities

1. Flashcard Introduction
2. Meaning Multiple Choice
3. Dictated Spelling
4. Context Fill-in-the-Blank
5. Synonym/Antonym Selection
6. Sentence Generation
