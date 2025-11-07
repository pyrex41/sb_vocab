# Audio Assets

This directory contains sound effect files for the vocabulary game.

## Required Sound Effects

### 1. button_click.wav
- **Purpose:** Button press feedback
- **Duration:** 0.1-0.3 seconds
- **Style:** Short, pleasant click sound
- **Usage:** All button interactions

### 2. correct.wav
- **Purpose:** Correct answer celebration
- **Duration:** 0.5-1.5 seconds
- **Style:** Upbeat, encouraging, kid-friendly
- **Usage:** When student answers correctly

### 3. incorrect.wav
- **Purpose:** Incorrect answer feedback
- **Duration:** 0.3-0.8 seconds
- **Style:** Gentle, not harsh or discouraging
- **Usage:** When student makes a mistake

### 4. activity_complete.wav
- **Purpose:** Activity completion notification
- **Duration:** 0.5-1.0 seconds
- **Style:** Satisfying completion sound
- **Usage:** After each activity is finished

### 5. session_start.wav
- **Purpose:** Session beginning
- **Duration:** 0.5-1.5 seconds
- **Style:** Welcoming, energetic
- **Usage:** When starting a new learning session

### 6. session_complete.wav
- **Purpose:** Session completion celebration
- **Duration:** 1.0-2.5 seconds
- **Style:** Triumphant, celebratory
- **Usage:** Results screen when session ends

## Audio Specifications

- **Format:** WAV or OGG Vorbis
- **Sample Rate:** 44.1kHz
- **Bit Depth:** 16-bit
- **Channels:** Mono (preferred) or Stereo
- **Max File Size:** 100KB per file

## Sourcing Options

### 1. Royalty-Free Libraries (Recommended)
- **freesound.org** - Creative Commons sounds
- **OpenGameArt.org** - Game-specific sound effects
- **Zapsplat.com** - Free sound effects library
- **Mixkit.co** - Free game sounds

### 2. Godot AudioStreamGenerator
For simple tones, you can generate sounds programmatically in Godot.

### 3. Text-to-Speech (Future Enhancement)
For spelling activity, consider integrating TTS for word pronunciation.

## Implementation Status

🚧 **Currently:** AudioManager is implemented but sound files are not yet created.

📝 **Next Steps:**
1. Source or create the 6 sound effect files
2. Place WAV/OGG files in this directory
3. Test audio playback in game
4. Adjust volumes if needed

## Volume Guidelines

All sounds should be normalized to similar perceived loudness:
- Button clicks: Subtle, ~50-60% volume
- Correct: Moderate, ~70-80% volume
- Incorrect: Gentle, ~60-70% volume
- Completions: Prominent, ~80-90% volume

## Kid-Friendly Requirements

✅ No harsh or scary sounds
✅ Encouraging, positive tones
✅ Age-appropriate (elementary school)
✅ Not overly loud or jarring
✅ Pleasant even after repeated plays
