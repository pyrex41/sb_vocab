# PRD: Sound Design & Visual Polish

**Features:** Audio Integration & Visual Consistency
**Status:** Planning
**Priority:** P0 (Sound), P1 (Visual)
**Owner:** TBD
**Effort:** 14-18 hours total
**Target Release:** Phase 5, Week 2

---

# Part A: Sound Design

## Overview

Complete the audio system by sourcing professional sound effects and background music, integrating throughout the game, and ensuring kid-friendly, non-distracting audio experience.

## Problem Statement

AudioManager system is implemented with hooks everywhere, but:
- No actual audio files exist (placeholder paths)
- No background music
- No ambient sounds
- Missing hover sounds for buttons
- No volume normalization or mixing

Sound represents **50% of game feel** - visuals alone aren't enough.

## Sound Assets Required

### 1. Core Sound Effects (Already Hooked)

| Sound | File | Duration | Style | Status | Usage |
|-------|------|----------|-------|---------|-------|
| button_click.wav | 0.1-0.2s | Short, pleasant click | ⚠️ NEEDED | All button presses |
| correct.wav | 0.5-1.0s | Upbeat, celebratory | ⚠️ NEEDED | Correct answers |
| incorrect.wav | 0.3-0.5s | Gentle, non-harsh | ⚠️ NEEDED | Incorrect answers |
| activity_complete.wav | 0.8-1.2s | Satisfying completion | ⚠️ NEEDED | Activity finished |
| session_start.wav | 0.8-1.5s | Welcoming, energetic | ⚠️ NEEDED | Session begins |
| session_complete.wav | 1.5-2.5s | Triumphant, big celebration | ⚠️ NEEDED | Results screen |

### 2. Additional Sound Effects (New)

| Sound | File | Duration | Style | Usage |
|-------|------|----------|-------|-------|
| button_hover.wav | 0.05-0.1s | Subtle woosh/tone | Button mouse enter |
| transition_whoosh.wav | 0.3-0.5s | Smooth swoosh | Scene transitions |
| error_dialog.wav | 0.4-0.6s | Alert but not scary | Error dialogs |
| help_open.wav | 0.2-0.3s | Friendly chime | Help overlay opens |
| word_appear.wav | 0.2-0.3s | Pop/ting | New word/question appears |
| typing.wav | 0.05s | Soft keyboard click | Each letter typed (spelling) |

### 3. Background Music

| Track | Duration | BPM | Style | Usage |
|-------|----------|-----|-------|-------|
| gameplay_loop.ogg | 2-3 min | 100-120 | Upbeat, non-distracting, loopable | During activities |
| results_fanfare.ogg | 30-45s | Any | Celebratory, triumphant | Results screen |

**Requirements:**
- Kid-friendly instrumentation
- No lyrics (distracting)
- Moderate tempo (not too fast/slow)
- Seamless loop points
- Volume balanced for background

## Audio Sourcing Strategy

### Option A: Free CC0 Libraries (Recommended for MVP)

**Sites:**
- [freesound.org](https://freesound.org) - Huge library, CC0/CC-BY
- [OpenGameArt.org](https://opengameart.org) - Game-focused
- [Mixkit.co](https://mixkit.co/free-sound-effects/) - High quality, free
- [ZapSplat.com](https://www.zapsplat.com) - Free with attribution

**Search Terms:**
- button: "ui click", "button press", "menu select"
- correct: "success", "win", "positive", "achievement"
- incorrect: "error gentle", "wrong soft", "negative friendly"
- music: "upbeat loop", "educational game music", "happy background"

### Option B: Generate with Tools

**Bfxr / Sfxr:**
- Free web tool for retro game sounds
- Good for UI sounds (clicks, pops)
- Export as WAV

**LMMS / GarageBand:**
- Create simple music loops
- Educational/playful presets
- Export as OGG Vorbis

### Option C: Commission Custom (Future)

- Hire sound designer on Fiverr ($50-150)
- Consistent style across all sounds
- Exclusive, professional quality

## Technical Implementation

### AudioManager Enhancements

**Add music system:**
```gdscript
# AudioManager.gd additions

const MUSIC_GAMEPLAY = "gameplay_loop"
const MUSIC_RESULTS = "results_fanfare"

var music_player: AudioStreamPlayer
var music_tracks: Dictionary = {}
var current_music: String = ""
var music_enabled: bool = true

func _ready():
    # ... existing code ...
    _setup_music_player()
    _load_music()

func _setup_music_player():
    music_player = AudioStreamPlayer.new()
    music_player.bus = "Music"  # Separate bus for music
    add_child(music_player)

func play_music(track_name: String, fade_in: float = 1.0):
    if not music_enabled or track_name == current_music:
        return

    # Fade out current
    if music_player.playing:
        var fade_out = create_tween()
        fade_out.tween_property(music_player, "volume_db", -80, 0.5)
        await fade_out.finished

    # Play new track
    music_player.stream = music_tracks.get(track_name)
    music_player.volume_db = -80
    music_player.play()

    # Fade in
    var fade_in_tween = create_tween()
    fade_in_tween.tween_property(music_player, "volume_db", -10, fade_in)

    current_music = track_name

func stop_music(fade_out: float = 0.5):
    if not music_player.playing:
        return

    var tween = create_tween()
    tween.tween_property(music_player, "volume_db", -80, fade_out)
    await tween.finished
    music_player.stop()
    current_music = ""

func set_music_enabled(enabled: bool):
    music_enabled = enabled
    if not enabled:
        stop_music(0.5)
```

### Audio Bus Configuration

**Add in Godot Project Settings → Audio:**
```
Master (0 dB)
├── SFX (0 dB)  # Sound effects
└── Music (-10 dB)  # Background music, quieter
```

### Integration Points

**GameSession.gd:**
```gdscript
func _ready():
    # ... existing code ...
    AudioManager.play_music(AudioManager.MUSIC_GAMEPLAY, 1.5)

func _on_session_ended(summary):
    AudioManager.stop_music(0.5)
    # Scene change...
```

**ResultsScreen.gd:**
```gdscript
func _ready():
    # ... existing code ...
    AudioManager.play_music(AudioManager.MUSIC_RESULTS, 0.5)
```

**MainMenu.gd:**
```gdscript
func _ready():
    # Stop any playing music
    AudioManager.stop_music(0.3)
```

## Asset Specifications

### File Format
- **Sound Effects:** WAV (44.1kHz, 16-bit, mono preferred)
- **Music:** OGG Vorbis (compressed, loopable)

### Volume Normalization
- All SFX normalized to -6 dB peak
- Music normalized to -12 dB peak
- No clipping (peaks should not hit 0 dB)

### Naming Convention
```
audio/sfx/button_click.wav
audio/sfx/correct_answer.wav
audio/sfx/incorrect_answer.wav
audio/music/gameplay_loop.ogg
audio/music/results_fanfare.ogg
```

## Testing Strategy

### Audio QA Checklist
- [ ] All sounds play at appropriate times
- [ ] No audio clipping or distortion
- [ ] Volume levels balanced across all sounds
- [ ] Music loops seamlessly without gap/click
- [ ] Concurrent sounds don't overload (pooling works)
- [ ] Mute toggle works for both SFX and music
- [ ] Audio plays correctly on web export
- [ ] No delay/latency in audio triggering

### Kid Testing
- Play with volume at normal levels
- Ask: "Are any sounds annoying or too loud?"
- Observe: Do they mute the game? (indicates issue)

---

# Part B: Visual Polish

## Overview

Apply consistent theming, polish typography, add depth/shadows, and ensure visual cohesion across all screens.

## Problem Statement

Visual inconsistencies hurt perceived quality:
- Theme file exists but not applied to any scenes
- Inconsistent spacing, padding, alignment
- Flat UI with no depth or hierarchy
- Typography sizes/weights are ad-hoc
- Colors vary across screens

## Theme Application

**kid_friendly_theme.tres exists with:**
- Primary colors (blue, green, yellow)
- Button styles (normal, hover, pressed, disabled)
- Label styles (title, body, hint)
- Panel styles (rounded, shadowed)

**Apply to all 9 scenes:**

### Manual Tasks (Godot Editor Required)

For each scene file:
1. Open in Godot editor
2. Select root Control node
3. Inspector → Theme section
4. Set Theme: `res://themes/kid_friendly_theme.tres`
5. Review `theme_override_*` properties
6. Remove redundant overrides
7. Save scene
8. Test visual regression

**Scene List:**
- MainMenu.tscn
- GameSession.tscn
- ProgressScreen.tscn
- ResultsScreen.tscn
- activities/Flashcard.tscn
- activities/MultipleChoice.tscn
- activities/Spelling.tscn
- activities/FillBlank.tscn
- activities/SynonymAntonym.tscn

## Visual Polish Tasks

### 1. Depth & Shadows

**Add shadows to cards/panels:**
```gdscript
# In theme or per-panel
var panel_stylebox = StyleBoxFlat.new()
panel_stylebox.bg_color = Color.WHITE
panel_stylebox.shadow_color = Color(0, 0, 0, 0.2)
panel_stylebox.shadow_size = 8
panel_stylebox.shadow_offset = Vector2(0, 4)
panel_stylebox.corner_radius_top_left = 12
panel_stylebox.corner_radius_top_right = 12
panel_stylebox.corner_radius_bottom_left = 12
panel_stylebox.corner_radius_bottom_right = 12
```

**Result:** Panels "float" above background

### 2. Typography Hierarchy

**Establish clear sizes:**
- **H1 (Titles):** 48px, bold - Main screen titles
- **H2 (Subtitles):** 32px, semi-bold - Section headers
- **Body:** 24px, regular - Activity text, questions
- **Hint:** 18px, italic - Helper text
- **Button:** 24px, bold - Call to action

**Implementation:**
Update theme with font size variations, apply via theme overrides sparingly.

### 3. Spacing & Alignment

**Establish consistent spacing:**
- **Margins:** 20px standard, 40px large (screen edges)
- **Padding:** 15px inside panels
- **Separation:** 10px between related elements, 20px between sections

**Tools:**
- Use VBoxContainer/HBoxContainer with `separation` property
- Use MarginContainer for consistent padding
- Align to 8px grid for precision

### 4. Color Consistency

**Verify color palette usage:**
- **Primary Blue:** Buttons, headers
- **Success Green:** Correct answers, progress
- **Warning Yellow:** Highlights, attention
- **Error Red:** Incorrect (muted, not harsh)
- **Neutral Gray:** Disabled states, backgrounds

**Check:**
- No random color variations
- Accessible contrast ratios (WCAG AA)

### 5. Button States

**Ensure all buttons have:**
- **Normal:** Clear, inviting
- **Hover:** Slight scale up (1.05x), lighter color
- **Pressed:** Slight scale down (0.95x), darker color
- **Disabled:** Grayscale, lower opacity

Already implemented via juice, verify theme supports.

### 6. Loading States

**Replace "Loading..." text:**
```gdscript
# Create animated spinner or progress bar
# Add friendly loading messages:
var loading_messages = [
    "Gathering words... 📚",
    "Preparing activities... ✏️",
    "Getting ready to learn! 🎓",
    "Loading vocabulary... 📖"
]
```

## Implementation Checklist

### Week 2, Day 1-2: Theme & Depth
- [ ] Apply kid_friendly_theme.tres to all 9 scenes
- [ ] Add shadows to all panels
- [ ] Test visual consistency across screens
- [ ] Document any custom overrides needed

### Week 2, Day 3: Typography
- [ ] Establish font size hierarchy
- [ ] Update all text elements to use standard sizes
- [ ] Ensure readability on all screens
- [ ] Test on different resolutions

### Week 2, Day 4: Spacing & Colors
- [ ] Standardize margins/padding
- [ ] Verify color palette consistency
- [ ] Improve button visual states
- [ ] Polish loading indicators

### Week 2, Day 5: QA
- [ ] Cross-screen visual consistency check
- [ ] Accessibility: Check contrast ratios
- [ ] Responsive design: Test different resolutions
- [ ] Final polish pass

## Success Criteria

### Sound Design
- [ ] All 6 core sound effects sourced and integrated
- [ ] Background music plays during gameplay
- [ ] Hover sounds on all buttons
- [ ] No audio clipping or volume issues
- [ ] M key toggles mute successfully

### Visual Polish
- [ ] Theme applied to all scenes
- [ ] Consistent shadows and depth
- [ ] Typography hierarchy clear and readable
- [ ] Spacing/alignment consistent
- [ ] No visual regressions from theme application

## Timeline

**Total: 14-18 hours**

**Sound Design:** 6-8 hours
- Sourcing: 2-3 hours
- Integration: 2-3 hours
- Music system: 1-2 hours
- Testing/polish: 1 hour

**Visual Polish:** 8-10 hours
- Theme application: 3-4 hours (manual in editor)
- Depth/shadows: 1-2 hours
- Typography: 1-2 hours
- Spacing/colors: 2 hours
- QA/testing: 1-2 hours

---

**Related Documents:**
- [Phase 5 Overview](./phase5-overview.md)
- [Juice & Effects PRD](./phase5-juice-and-effects.md)
- [Audio README](/vocab_game/audio/README.md)
- [Theme File](/vocab_game/themes/kid_friendly_theme.tres)
