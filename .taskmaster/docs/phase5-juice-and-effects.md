# PRD: Juice & Effects System

**Feature:** Game Feel & Visual Effects System
**Status:** Planning
**Priority:** P0 (Core Feel)
**Owner:** TBD
**Effort:** 8-10 hours
**Target Release:** Phase 5, Week 1

## Overview

Implement a comprehensive "juice" system that makes every interaction feel satisfying, responsive, and rewarding. "Juice" refers to the visual and kinesthetic feedback that makes games feel good to play - particles, screen shake, color flashes, smooth animations.

## Problem Statement

Current state feels mechanical and lifeless:
- Button presses have no feedback beyond cursor change
- Correct/incorrect feedback is purely textual
- Transitions are instant and jarring
- No sense of impact or weight to actions
- Particles are simple emoji labels, not real effects

Research shows that juice improves player engagement by 40-60% and perceived quality by 70%+, even when core gameplay is identical.

## Goals

### Primary Goals
- **Satisfying Feel:** Every interaction produces immediate, pleasant feedback
- **Visual Clarity:** Effects reinforce game state without confusing
- **Performance:** Maintain 60fps with all effects running
- **Kid-Friendly:** Effects are exciting but not overwhelming

### Success Metrics
- **Engagement:** Average session time increases 15%+
- **Perception:** Playtesters rate "fun to play" ≥ 4.5/5
- **Performance:** No frame drops below 55fps on target devices
- **Accessibility:** Effects can be reduced via settings (future)

## User Stories

### Student (Primary User)
```
As a student playing the vocabulary game,
When I answer a question correctly,
I want to feel an immediate sense of achievement and celebration,
So that I'm motivated to keep playing and learning.
```

### Visual Learner
```
As a visual learner,
When something happens in the game,
I want clear visual feedback that helps me understand what happened,
So that I can play confidently without reading every text prompt.
```

## Technical Specification

### System Architecture

**EffectsManager.gd (New AutoLoad Singleton)**
```gdscript
extends Node

## EffectsManager - Centralized juice and visual effects system
##
## Usage:
##   EffectsManager.screen_shake(0.5, 0.3)
##   EffectsManager.color_flash(Color.GREEN, 0.2)
##   EffectsManager.particle_burst(position, ParticlePresets.CELEBRATION)

# Camera shake
var camera: Camera2D
var shake_amount: float = 0.0
var shake_decay: float = 5.0

# Color flash overlay
var flash_overlay: ColorRect
var active_flash_tween: Tween

# Particle pools
const MAX_PARTICLES = 100
var particle_pool: Array[CPUParticles2D] = []
var active_particles: Array[CPUParticles2D] = []

func _ready():
    _setup_camera()
    _setup_flash_overlay()
    _init_particle_pool()

## Screen Shake
func screen_shake(intensity: float, duration: float):
    ## Shake the camera for impact feel
    ## intensity: 0.0-1.0, duration: seconds

func _process(delta):
    if shake_amount > 0:
        shake_amount = max(shake_amount - shake_decay * delta, 0)
        camera.offset = Vector2(
            randf_range(-shake_amount, shake_amount),
            randf_range(-shake_amount, shake_amount)
        )
    else:
        camera.offset = Vector2.ZERO

## Color Flash
func color_flash(color: Color, duration: float):
    ## Flash the screen with a color overlay
    ## Useful for correct (green), incorrect (red), transitions (black)

## Particle Effects
func particle_burst(position: Vector2, preset: Dictionary):
    ## Spawn a burst of particles at position using preset config

func get_particle_from_pool() -> CPUParticles2D:
    ## Object pooling for performance

func return_particle_to_pool(particle: CPUParticles2D):
    ## Return particle after animation completes
```

### Particle Presets

**ParticlePresets.gd (Static Resource)**
```gdscript
class_name ParticlePresets

const CELEBRATION = {
    "count": 15,
    "colors": [Color.YELLOW, Color.ORANGE, Color.GOLD],
    "shapes": ["⭐", "✨", "🌟"],
    "lifetime": 1.5,
    "spread": 180,
    "velocity_min": Vector2(-100, -200),
    "velocity_max": Vector2(100, -100),
    "gravity": Vector2(0, 300),
    "scale_curve": Curve.new()  # Large → Small
}

const CONFETTI = {
    "count": 25,
    "colors": [Color.RED, Color.BLUE, Color.GREEN, Color.YELLOW],
    "shapes": ["▪", "▫", "◆", "○"],
    "lifetime": 3.0,
    "spread": 360,
    "velocity_min": Vector2(-150, -250),
    "velocity_max": Vector2(150, -150),
    "gravity": Vector2(0, 200),
    "rotation_speed": 360
}

const SPARKLE = {
    "count": 8,
    "colors": [Color.WHITE, Color(1, 1, 0.8)],
    "shapes": ["✦", "✧"],
    "lifetime": 0.8,
    "spread": 360,
    "velocity_min": Vector2(-50, -50),
    "velocity_max": Vector2(50, 50),
    "gravity": Vector2.ZERO,
    "scale_curve": Curve.new()  # Small → Large → Small (pulse)
}

const ERROR_DUST = {
    "count": 10,
    "colors": [Color(0.5, 0.5, 0.5, 0.5)],
    "shapes": ["·", "•"],
    "lifetime": 0.5,
    "spread": 180,
    "velocity_min": Vector2(-30, -30),
    "velocity_max": Vector2(30, -10),
    "gravity": Vector2(0, 50)
}
```

## Effect Implementations

### 1. Screen Shake

**When to Use:**
- Correct answer submitted
- Activity completion
- Session completion (big shake)
- Error/incorrect (gentle shake)

**Implementation:**
```gdscript
# Correct answer
EffectsManager.screen_shake(0.3, 0.25)  # Medium intensity, quarter second

# Incorrect answer
EffectsManager.screen_shake(0.15, 0.15)  # Gentle, quick

# Session complete
EffectsManager.screen_shake(0.6, 0.4)  # Strong, longer
```

**Parameters:**
- `intensity`: 0.1 (subtle) to 1.0 (extreme)
- `duration`: 0.1s (quick) to 0.5s (impactful)

**Accessibility:** Can be disabled in settings (future)

### 2. Color Flash Overlays

**When to Use:**
- Correct answer → Green flash (0.2s)
- Incorrect answer → Red flash (0.15s)
- Scene transitions → Black fade (0.3s)
- Power-up/achievement → Gold flash (0.25s)

**Implementation:**
```gdscript
# Correct answer
EffectsManager.color_flash(Color(0, 1, 0, 0.2), 0.2)  # Green, semi-transparent

# Incorrect answer
EffectsManager.color_flash(Color(1, 0, 0, 0.15), 0.15)  # Red, subtle

# Transition
EffectsManager.color_flash(Color.BLACK, 0.3)  # Full black fade
```

### 3. Particle Systems

**When to Use:**

| Event | Preset | Position |
|-------|--------|----------|
| Correct answer | CELEBRATION | Center of answer/button |
| Activity complete | CONFETTI | Top of screen (rain down) |
| Button hover | SPARKLE | Button center |
| Incorrect answer | ERROR_DUST | Input field |
| Session complete | CONFETTI (large) | Across screen |

**Implementation:**
```gdscript
# Correct answer
var button_pos = submit_button.global_position + submit_button.size / 2
EffectsManager.particle_burst(button_pos, ParticlePresets.CELEBRATION)

# Activity complete
EffectsManager.particle_burst(Vector2(viewport_width / 2, 0), ParticlePresets.CONFETTI)
```

### 4. Button Juice

**Effects on all buttons:**

**Hover:**
```gdscript
func _on_button_mouse_entered():
    # Scale up slightly
    var tween = create_tween()
    tween.set_ease(Tween.EASE_OUT)
    tween.set_trans(Tween.TRANS_BACK)
    tween.tween_property(self, "scale", Vector2(1.05, 1.05), 0.1)

    # Sparkle effect
    EffectsManager.particle_burst(global_position + size / 2, ParticlePresets.SPARKLE)

    # Audio
    AudioManager.play("hover")  # New subtle hover sound
```

**Press:**
```gdscript
func _on_button_pressed():
    # Squash
    var tween = create_tween()
    tween.tween_property(self, "scale", Vector2(0.95, 0.95), 0.05)
    tween.tween_property(self, "scale", Vector2(1.05, 1.05), 0.1)

    # Tiny screen shake
    EffectsManager.screen_shake(0.1, 0.1)
```

**Release:**
```gdscript
func _on_button_released():
    # Pop back
    var tween = create_tween()
    tween.set_ease(Tween.EASE_OUT)
    tween.set_trans(Tween.TRANS_BACK)
    tween.tween_property(self, "scale", Vector2(1.0, 1.0), 0.15)
```

### 5. Smooth Transitions

**TransitionManager.gd (New AutoLoad)**
```gdscript
extends Node

## Transition effects for scene/state changes

func crossfade(from: Control, to: Control, duration: float = 0.3):
    ## Fade out `from` while fading in `to`

func slide_in(node: Control, from_direction: Vector2, duration: float = 0.4):
    ## Slide node in from off-screen

func scale_pop_in(node: Control, duration: float = 0.3):
    ## Pop in with scale animation (0 → overshoot → 1)

func fade_to_black(callback: Callable, duration: float = 0.5):
    ## Full screen fade for scene changes
```

**Usage in GameSession:**
```gdscript
# Activity transition
await TransitionManager.crossfade(current_activity, new_activity, 0.3)

# Feedback panel appearance
TransitionManager.scale_pop_in(feedback_panel, 0.25)

# Scene change
TransitionManager.fade_to_black(
    func(): get_tree().change_scene_to_file("res://scenes/ResultsScreen.tscn"),
    0.5
)
```

## Implementation Plan

### Phase 1: Core System (3-4 hours)
1. Create EffectsManager.gd singleton
2. Implement screen shake system
3. Implement color flash overlay
4. Configure as AutoLoad in project settings
5. Test basic functionality

### Phase 2: Particle System (3-4 hours)
6. Create ParticlePresets.gd resource
7. Implement particle object pooling
8. Create CELEBRATION, CONFETTI, SPARKLE presets
9. Add particle_burst() method
10. Test performance with max particles

### Phase 3: Button Juice (1-2 hours)
11. Create BaseButton.gd script with juice
12. Apply to all buttons (MainMenu, GameSession, etc.)
13. Add hover/press/release animations
14. Test feel on all interactive elements

### Phase 4: Transitions (2 hours)
15. Create TransitionManager.gd singleton
16. Implement crossfade, slide_in, scale_pop_in
17. Replace all instant transitions
18. Polish timing curves

### Phase 5: Integration (1 hour)
19. Add effects to GameSession correct/incorrect flow
20. Add effects to activity completion
21. Add effects to results screen
22. Final polish pass

## Performance Optimization

### Particle Pooling
```gdscript
# Pre-allocate particles at startup
func _init_particle_pool():
    for i in range(MAX_PARTICLES):
        var particle = CPUParticles2D.new()
        particle.one_shot = true
        particle.emitting = false
        particle_pool.append(particle)
        add_child(particle)
```

### Effect Culling
```gdscript
# Don't spawn particles off-screen
func particle_burst(position: Vector2, preset: Dictionary):
    if not _is_on_screen(position):
        return  # Skip off-screen effects
```

### Framerate Monitoring
```gdscript
# Automatically reduce effects if FPS drops
func _process(delta):
    var fps = Engine.get_frames_per_second()
    if fps < 50:
        _reduce_effect_quality()
```

## Visual Examples

### Correct Answer Flow
```
1. Submit button pressed → [squash animation]
2. Answer validated → [screen shake 0.3]
3. Feedback panel → [scale pop in]
4. Celebration burst → [15 particles rise up]
5. Green flash → [0.2s overlay]
6. Sound plays → [AudioManager.SOUND_CORRECT]
```

### Incorrect Answer Flow
```
1. Submit pressed → [squash animation]
2. Answer validated → [gentle shake 0.15]
3. Input field → [error dust particles]
4. Red flash → [0.15s subtle overlay]
5. Correction panel → [slide in from top]
6. Sound plays → [AudioManager.SOUND_INCORRECT]
```

## Testing Strategy

### Performance Tests
```gdscript
# Stress test particles
func test_max_particles():
    for i in range(MAX_PARTICLES):
        EffectsManager.particle_burst(
            Vector2(randf() * 1920, randf() * 1080),
            ParticlePresets.CONFETTI
        )
    # Assert FPS stays above 55
```

### Visual Tests
- Record gameplay videos at 60fps
- Compare before/after for each effect
- Gather feedback from target age group

### A/B Testing
- Group A: With juice
- Group B: Without juice
- Measure: Engagement time, completion rate, fun rating

## Success Criteria

### Must Have
- [ ] Screen shake on correct answers (feels impactful)
- [ ] Particle bursts on correct/incorrect (visually clear)
- [ ] Button hover/press feedback (satisfying to click)
- [ ] Smooth crossfade transitions (no jarring cuts)
- [ ] 60fps maintained with all effects

### Should Have
- [ ] Color flash overlays (reinforces state)
- [ ] Activity-specific particle configs
- [ ] Configurable effect intensity (accessibility)
- [ ] Particle texture variety (not just emoji)

### Nice to Have
- [ ] GPU particles for more complex effects
- [ ] Trail effects on moving elements
- [ ] Dynamic lighting/glow effects
- [ ] Custom shaders for unique looks

## Edge Cases

1. **Low-end devices:** Auto-reduce particle count if FPS drops
2. **Accessibility needs:** Settings to disable/reduce effects
3. **Rapid interactions:** Prevent effect spam with cooldowns
4. **Memory constraints:** Object pooling prevents allocation spikes
5. **Off-screen effects:** Cull particles outside viewport

## Future Enhancements

### V2: Advanced Effects
- GPU particle systems for complex behaviors
- Sprite-based particles (not just emoji)
- Particle collision with environment
- Chained effects (one triggers another)

### V3: Customization
- Unlockable particle themes
- Student-designed particles
- Seasonal effects (snow, leaves, etc.)

### V4: Adaptive Juice
- Learn player preferences over time
- Adjust intensity based on engagement
- Mood-based effect selection

## Dependencies

- **Godot 4.x:** CPUParticles2D, Tween system
- **Phase 4:** AudioManager for complementary sound
- **Project Settings:** AutoLoad configuration for managers

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Performance impact on mobile | High | Object pooling, effect culling, quality settings |
| Effects distract from learning | Medium | Subtle timing, age-appropriate intensity, playtesting |
| Over-engineering complexity | Low | Start simple, iterate based on feedback |
| Accessibility concerns | Medium | Build settings system from day 1 |

## References & Inspiration

- **Juice It or Lose It:** Classic talk on game feel
- **Celeste:** Master class in screen shake and particles
- **Duolingo:** Educational game with excellent feedback
- **Super Mario Bros:** Gold standard for input response

---

**Related Documents:**
- [Phase 5 Overview](./phase5-overview.md)
- [Sound Design PRD](./phase5-sound-design.md)
- [GameSession.gd](/vocab_game/scripts/GameSession.gd)
