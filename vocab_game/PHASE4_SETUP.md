# Phase 4 Setup Instructions

This document outlines manual steps that must be completed in the Godot editor to finish Phase 4.

## 🎨 1. Apply Theme to All Scenes

**Status:** ⚠️ Requires Godot Editor

### Scenes Needing Theme Application:
1. MainMenu.tscn
2. GameSession.tscn
3. ProgressScreen.tscn
4. ResultsScreen.tscn
5. activities/Flashcard.tscn
6. activities/MultipleChoice.tscn
7. activities/Spelling.tscn
8. activities/FillBlank.tscn
9. activities/SynonymAntonym.tscn

### Steps for Each Scene:
1. Open scene in Godot editor
2. Select root Control node
3. In Inspector → Theme section
4. Set Theme property to: `res://themes/kid_friendly_theme.tres`
5. Review any `theme_override_*` properties
   - Keep intentional overrides (e.g., title font sizes)
   - Remove redundant overrides that match theme defaults
6. Save scene
7. Test visually to ensure no regressions

---

## 🔊 2. Configure AudioManager AutoLoad

**Status:** ⚠️ Requires Godot Editor

### Steps:
1. Open Project → Project Settings
2. Navigate to AutoLoad tab
3. Click "Add" button
4. Set Path: `res://scripts/AudioManager.gd`
5. Set Node Name: `AudioManager`
6. Check "Enable" checkbox
7. Click "Add" button
8. Close Project Settings

### Verification:
- AudioManager should appear in AutoLoad list
- Load Order should be appropriate (after PlaycademySdk if it exists)

---

## 🎹 3. Configure Input Action Map

**Status:** ⚠️ Requires Godot Editor

### Steps:
1. Open Project → Project Settings
2. Navigate to Input Map tab
3. Add the following actions:

| Action Name | Key Bindings |
|-------------|--------------|
| `ui_submit` | Enter, Space |
| `ui_back` | Escape |
| `ui_help` | F1 |
| `ui_mute` | M |
| `ui_retry` | Ctrl+R (Cmd+R on Mac) |
| `select_option_1` | 1, Keypad 1 |
| `select_option_2` | 2, Keypad 2 |
| `select_option_3` | 3, Keypad 3 |
| `select_option_4` | 4, Keypad 4 |

### For Each Action:
1. Click "Add New Action" button
2. Type action name (e.g., `ui_submit`)
3. Press Enter
4. Click "+" next to action name
5. Press the key you want to bind
6. Click "OK"
7. Repeat for additional bindings

### Notes:
- `ui_submit` and some `ui_*` actions may already exist - just add missing keys
- Ensure no conflicts with existing Godot shortcuts
- Test shortcuts in-game after configuration

---

## 🎵 4. Source Sound Effect Files

**Status:** 🚧 Requires Audio Assets

### Required Files:
All files should be placed in `vocab_game/audio/` directory:

1. `button_click.wav` - Button press sound
2. `correct.wav` - Correct answer celebration
3. `incorrect.wav` - Incorrect answer feedback
4. `activity_complete.wav` - Activity completion
5. `session_start.wav` - Session start
6. `session_complete.wav` - Session completion

### Sourcing Options:

#### Option A: Download from Free Libraries
**Recommended Sites:**
- [freesound.org](https://freesound.org) - Filter by Creative Commons
- [OpenGameArt.org](https://opengameart.org) - Game-specific sounds
- [Zapsplat.com](https://www.zapsplat.com) - Free with attribution
- [Mixkit.co](https://mixkit.co/free-sound-effects/game/) - Game sounds

#### Option B: Generate in Godot
Use Godot's AudioStreamGenerator for simple tones:
```gdscript
# Example: Generate a simple beep
var generator = AudioStreamGenerator.new()
generator.mix_rate = 44100
# Configure and use...
```

#### Option C: Record Custom Sounds
- Use Audacity (free audio editor)
- Record simple sounds (clicks, dings, etc.)
- Export as WAV 44.1kHz, 16-bit

### Audio Specifications:
- Format: WAV or OGG Vorbis
- Sample Rate: 44.1kHz
- Bit Depth: 16-bit
- Max Duration: 2.5 seconds
- Max File Size: 100KB per file
- Kid-friendly, non-harsh sounds

---

## 🎬 5. Create Error Dialog Scene

**Status:** ⚠️ Requires Godot Editor

### Steps:
1. Create new scene: Scene → New Scene
2. Add root node: Control (name: "ErrorDialog")
3. Build structure:
```
Control (ErrorDialog)
├── Panel (BackgroundPanel - semi-transparent)
│   └── CenterContainer
│       └── Panel (ErrorCard)
│           └── MarginContainer (margins: 20px all sides)
│               └── VBoxContainer (separation: 15px)
│                   ├── Label (IconLabel - font size: 48)
│                   ├── Label (TitleLabel - font size: 32, bold)
│                   ├── Label (MessageLabel - font size: 20, autowrap)
│                   └── HBoxContainer (separation: 10px)
│                       ├── Button (ActionButton - "Retry")
│                       └── Button (CancelButton - "Cancel")
```

4. Configure BackgroundPanel:
   - Anchor preset: Full Rect
   - Modulate alpha: 0.7 (semi-transparent)
   - Color: Black

5. Configure ErrorCard:
   - Min size: 400x300
   - Theme: Apply kid_friendly_theme

6. Attach script: `res://scripts/ErrorDialog.gd`
7. Save scene as: `res://scenes/ErrorDialog.tscn`

---

## 📚 6. Create Help Overlay Scene

**Status:** ⚠️ Requires Godot Editor

### Steps:
1. Create new scene: Scene → New Scene
2. Add root node: CanvasLayer (name: "HelpOverlay")
3. Set layer: 100 (renders on top)
4. Build structure:
```
CanvasLayer (HelpOverlay)
└── Panel (BackgroundPanel)
    └── CenterContainer
        └── Panel (HelpCard)
            └── MarginContainer
                └── VBoxContainer
                    ├── Label (Title - "Keyboard Shortcuts ⌨️")
                    ├── ScrollContainer
                    │   └── VBoxContainer (ShortcutList)
                    │       ├── Label ("Enter - Submit Answer")
                    │       ├── Label ("1-4 - Select Option")
                    │       ├── Label ("Space - Continue")
                    │       ├── Label ("Escape - Return to Menu")
                    │       ├── Label ("F1 - Toggle Help")
                    │       └── Label ("M - Mute Audio")
                    └── Label (CloseLabel - "Press F1 or ESC to close")
```

5. Configure BackgroundPanel:
   - Anchor preset: Full Rect
   - Modulate alpha: 0.8
   - Color: Black

6. Configure HelpCard:
   - Min size: 500x400
   - Theme: Apply kid_friendly_theme

7. Create script: `res://scripts/HelpOverlay.gd`
8. Save scene as: `res://scenes/HelpOverlay.tscn`

---

## ✅ Verification Checklist

After completing setup:

### Theme Application:
- [ ] All 9 scenes have theme applied
- [ ] Visual consistency across all screens
- [ ] No layout regressions
- [ ] Buttons styled uniformly
- [ ] Text readable at all sizes

### Audio System:
- [ ] AudioManager in AutoLoad list
- [ ] 6 sound effect files in audio/ directory
- [ ] Sounds play when triggered
- [ ] No audio clipping or distortion
- [ ] Mute functionality works

### Keyboard Shortcuts:
- [ ] All input actions configured
- [ ] Enter submits in all activities
- [ ] Number keys work in choice activities
- [ ] Escape returns to menu
- [ ] F1 shows help overlay
- [ ] M toggles mute

### Error Handling:
- [ ] ErrorDialog scene created
- [ ] ErrorDialog script attached
- [ ] Error messages are kid-friendly
- [ ] Retry functionality works

### Help Overlay:
- [ ] HelpOverlay scene created
- [ ] All shortcuts documented
- [ ] F1 toggles help
- [ ] ESC closes help

---

## 🐛 Troubleshooting

### AudioManager Not Found
**Problem:** Script error: "AudioManager is not a valid Node"
**Solution:** Ensure AudioManager is added to AutoLoad in Project Settings

### Sounds Not Playing
**Problem:** No audio when triggered
**Solutions:**
1. Check audio files exist in `audio/` directory
2. Verify file paths in AudioManager.gd
3. Check audio bus settings (Master bus should exist)
4. Confirm audio is enabled: `AudioManager.is_enabled()`

### Keyboard Shortcuts Not Working
**Problem:** Key presses don't trigger actions
**Solutions:**
1. Verify Input Map configuration in Project Settings
2. Check for input event consumption by UI elements
3. Ensure `_input()` or `_unhandled_input()` is implemented
4. Test in both editor and exported build

### Theme Not Applying
**Problem:** Scenes don't use theme styling
**Solutions:**
1. Verify theme file path is correct: `res://themes/kid_friendly_theme.tres`
2. Check theme is set on root Control node
3. Remove conflicting theme_override properties
4. Reload scene after applying theme

---

## 📊 Phase 4 Progress Tracker

| Feature | Status | Completion |
|---------|--------|------------|
| Theme Application | 🚧 Manual | 0% |
| AudioManager | ✅ Complete | 100% |
| Sound Effects | 🚧 Assets Needed | 0% |
| ErrorMessages | ✅ Complete | 100% |
| ErrorDialog | 🚧 Scene Needed | 50% |
| Input Actions | 🚧 Manual | 0% |
| Keyboard Shortcuts | ✅ Code Ready | 75% |
| HelpOverlay | 🚧 Scene Needed | 25% |
| GlobalInput | ✅ Complete | 100% |

**Overall Phase 4 Progress: ~60%** (code complete, manual setup pending)

---

## 🎯 Next Steps

1. **Complete Godot Editor tasks** (sections 1, 2, 3, 5, 6)
2. **Source sound effect files** (section 4)
3. **Test all features** in-game
4. **Fix any bugs** discovered during testing
5. **Update documentation** with findings
6. **Create PR** for Phase 4 completion

---

## 📞 Support

If you encounter issues during setup:
1. Check Godot console for error messages
2. Review this document's troubleshooting section
3. Consult Godot documentation for editor-specific questions
4. Test in a fresh Godot project if problems persist

Good luck with Phase 4! 🚀
