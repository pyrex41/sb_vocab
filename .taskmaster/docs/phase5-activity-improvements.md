# PRD: Activity-Specific Improvements

**Feature:** Custom Animations & Interactions per Activity Type
**Status:** Planning
**Priority:** P2 (Nice-to-have)
**Owner:** TBD
**Effort:** 10-12 hours
**Target Release:** Phase 5, Week 3

## Overview

Add activity-specific polish that makes each of the 5 activity types feel unique and engaging with custom animations, transitions, and interactions tailored to the activity's learning goals.

## Problem Statement

Currently all activities feel generic:
- Same fade-in entrance for every activity
- No activity-specific animations
- Interactive elements lack unique feel
- Missed opportunities for engaging, activity-appropriate effects

Each activity type deserves custom polish that reinforces its educational purpose and makes it memorable.

## Goals

### Primary Goals
- **Unique Identity:** Each activity type feels distinct
- **Educational Reinforcement:** Animations support learning objectives
- **Engagement:** Activity-specific polish increases interest
- **Polish Level:** Matches commercial educational games

### Success Metrics
- **Variety Perception:** Students notice different activity types (survey)
- **Engagement:** No drop-off in activity completion rates
- **Preference:** Students develop favorites (indicates distinct experiences)

## Activity-by-Activity Specifications

### 1. Flashcard (Flip & Reveal)

**Current:** Simple text display with reveal button

**Enhanced:**

**Entrance Animation:**
```gdscript
func _animate_entrance():
    # Fly in from off-screen with rotation
    position.x = -get_viewport_rect().size.x
    rotation = deg_to_rad(-15)

    var tween = create_tween()
    tween.set_parallel(true)
    tween.set_ease(Tween.EASE_OUT)
    tween.set_trans(Tween.TRANS_BACK)
    tween.tween_property(self, "position:x", original_position.x, 0.6)
    tween.tween_property(self, "rotation", 0, 0.6)
```

**Card Flip Effect:**
```gdscript
func _on_reveal_pressed():
    # 3D card flip animation
    var half_flip = create_tween()
    half_flip.tween_property(card_front, "scale:x", 0, 0.15)
    await half_flip.finished

    # Swap front/back
    card_front.hide()
    card_back.show()
    card_back.scale.x = 0

    var full_flip = create_tween()
    full_flip.tween_property(card_back, "scale:x", 1, 0.15)
```

**Particle Effect:**
- On flip: Small sparkle burst around card edges
- On correct: Celebration burst from card center

**Effort:** 2 hours

---

### 2. Multiple Choice (Sequential Reveal)

**Current:** All options appear instantly

**Enhanced:**

**Sequential Option Appearance:**
```gdscript
func _show_options(options: Array):
    for i in range(options.size()):
        var option_button = option_buttons[i]
        option_button.modulate.a = 0
        option_button.position.x -= 50  # Start offset left

        # Stagger each option by 0.1s
        await get_tree().create_timer(0.1 * i).timeout

        var tween = create_tween()
        tween.set_parallel(true)
        tween.set_ease(Tween.EASE_OUT)
        tween.tween_property(option_button, "modulate:a", 1, 0.2)
        tween.tween_property(option_button, "position:x", original_pos.x, 0.2)

        # Play soft "pop" sound
        AudioManager.play("option_appear")
```

**Selection Feedback:**
```gdscript
func _on_option_selected(button: Button):
    # Highlight selected
    var tween = create_tween()
    tween.tween_property(button, "scale", Vector2(1.1, 1.1), 0.1)
    tween.tween_property(button, "scale", Vector2(1.0, 1.0), 0.1)

    # Dim others
    for other_button in option_buttons:
        if other_button != button:
            other_button.modulate.a = 0.5
```

**Incorrect Option Shake:**
```gdscript
func _show_incorrect_feedback():
    # Selected option shakes out
    var selected = _get_selected_button()
    var shake_tween = create_tween()
    shake_tween.tween_property(selected, "rotation", deg_to_rad(5), 0.05)
    shake_tween.tween_property(selected, "rotation", deg_to_rad(-5), 0.05)
    shake_tween.tween_property(selected, "rotation", 0, 0.05)

    # Fade out wrong options
    for button in option_buttons:
        if button != correct_button:
            var fade = create_tween()
            fade.tween_property(button, "modulate:a", 0.3, 0.3)
```

**Effort:** 2-3 hours

---

### 3. Spelling (Letter-by-Letter)

**Current:** Empty input, student types

**Enhanced:**

**Word Appearance (Listening Mode):**
```gdscript
func _show_word_to_spell(word: String):
    # Word appears letter-by-letter from center
    word_label.text = ""
    for letter in word:
        word_label.text += letter

        # Bounce in each letter
        var tween = create_tween()
        tween.tween_property(word_label, "scale", Vector2(1.2, 1.2), 0.05)
        tween.tween_property(word_label, "scale", Vector2(1.0, 1.0), 0.05)

        # Soft type sound
        AudioManager.play("letter_type")

        await get_tree().create_timer(0.1).timeout

    # Brief pause, then hide for typing
    await get_tree().create_timer(1.0).timeout
    word_label.modulate.a = 0  # Fade out
```

**Typing Feedback:**
```gdscript
func _on_input_text_changed(new_text: String):
    # Each letter typed causes input to pulse
    var tween = create_tween()
    tween.tween_property(input_field, "scale", Vector2(1.05, 1.05), 0.05)
    tween.tween_property(input_field, "scale", Vector2(1.0, 1.0), 0.05)

    # Type sound
    AudioManager.play("letter_type")
```

**Correct Letter Celebration:**
```gdscript
func _on_correct_submission():
    # Reveal correct word letter-by-letter in green
    var correct_label = Label.new()
    correct_label.text = ""
    correct_label.modulate = Color.GREEN
    add_child(correct_label)

    for letter in correct_word:
        correct_label.text += letter
        EffectsManager.particle_burst(
            correct_label.get_global_position(),
            ParticlePresets.SPARKLE
        )
        await get_tree().create_timer(0.08).timeout
```

**Effort:** 2-3 hours

---

### 4. Fill in the Blank (Word Slot Animation)

**Current:** Sentence with blank, input field

**Enhanced:**

**Sentence Build-In:**
```gdscript
func _show_sentence(sentence_parts: Array):
    # Parts before blank
    for part in sentence_parts[0]:
        sentence_label.text += part + " "
        await get_tree().create_timer(0.05).timeout

    # Blank appears as pulsing slot
    blank_indicator.show()
    var pulse = create_tween()
    pulse.set_loops()
    pulse.tween_property(blank_indicator, "modulate:a", 0.5, 0.5)
    pulse.tween_property(blank_indicator, "modulate:a", 1.0, 0.5)

    # Parts after blank
    for part in sentence_parts[1]:
        sentence_label.text += part + " "
        await get_tree().create_timer(0.05).timeout
```

**Word Insertion Animation:**
```gdscript
func _on_correct_answer(word: String):
    # Zoom word into blank
    var word_node = Label.new()
    word_node.text = word
    word_node.scale = Vector2(0, 0)
    word_node.global_position = input_field.global_position
    add_child(word_node)

    var zoom_tween = create_tween()
    zoom_tween.set_parallel(true)
    zoom_tween.set_ease(Tween.EASE_OUT)
    zoom_tween.set_trans(Tween.TRANS_BACK)
    zoom_tween.tween_property(word_node, "scale", Vector2(1, 1), 0.4)
    zoom_tween.tween_property(word_node, "global_position", blank_indicator.global_position, 0.4)

    await zoom_tween.finished

    # Replace blank indicator
    blank_indicator.hide()
    # Word settles into sentence
```

**Effort:** 2 hours

---

### 5. Synonym/Antonym (Relationship Visualization)

**Current:** Word + buttons for Synonym/Antonym selection

**Enhanced:**

**Word Pair Entrance:**
```gdscript
func _show_word_pair(word1: String, word2: String):
    # Words fly in from opposite sides
    word1_label.position.x = -200
    word2_label.position.x = get_viewport_rect().size.x + 200

    var tween1 = create_tween()
    tween1.set_ease(Tween.EASE_OUT)
    tween1.set_trans(Tween.TRANS_BACK)
    tween1.tween_property(word1_label, "position:x", word1_original_pos.x, 0.5)

    var tween2 = create_tween()
    tween2.set_ease(Tween.EASE_OUT)
    tween2.set_trans(Tween.TRANS_BACK)
    tween2.tween_property(word2_label, "position:x", word2_original_pos.x, 0.5)
```

**Relationship Arrow:**
```gdscript
func _on_synonym_selected():
    # Draw animated arrow between words (pointing right →)
    var arrow = Line2D.new()
    arrow.default_color = Color.GREEN
    arrow.width = 5
    add_child(arrow)

    # Animate arrow growing
    var start = word1_label.position + Vector2(word1_label.size.x, word1_label.size.y/2)
    var end = word2_label.position + Vector2(0, word2_label.size.y/2)

    var tween = create_tween()
    tween.tween_method(_draw_arrow.bind(arrow, start), 0.0, 1.0, 0.4)

func _on_antonym_selected():
    # Draw animated X or opposing arrows (← →)
```

**Effort:** 2-3 hours

---

## Common Activity Improvements

### 1. Activity Title Cards

**Add title card before each activity:**
```gdscript
func _show_title_card(activity_type: String):
    var title_overlay = CanvasLayer.new()
    var title_label = Label.new()
    title_label.text = _get_activity_title(activity_type)
    title_label.modulate = Color(1, 1, 1, 0)

    # Fade in
    var fade_in = create_tween()
    fade_in.tween_property(title_label, "modulate:a", 1, 0.3)
    await fade_in.finished

    # Hold
    await get_tree().create_timer(0.8).timeout

    # Zoom out and fade
    var exit_tween = create_tween()
    exit_tween.set_parallel(true)
    exit_tween.tween_property(title_label, "modulate:a", 0, 0.3)
    exit_tween.tween_property(title_label, "scale", Vector2(1.5, 1.5), 0.3)
    await exit_tween.finished

    title_overlay.queue_free()

func _get_activity_title(type: String) -> String:
    match type:
        "flashcard": return "📇 Flashcard Challenge"
        "multiple_choice": return "🤔 Choose the Answer"
        "spelling": return "✍️ Spelling Bee"
        "fill_blank": return "📝 Complete the Sentence"
        "synonym_antonym": return "🔄 Word Relationships"
```

**Effort:** 1 hour

### 2. Activity Icons

**Consistent iconography:**
- Flashcard: 📇
- Multiple Choice: 🤔 or ABCD
- Spelling: ✍️ or ✏️
- Fill Blank: 📝 or __
- Synonym/Antonym: 🔄 or ↔

Display in top-right during activity.

---

## Implementation Plan

### Week 3, Day 1: Flashcard & Spelling
- Implement flashcard flip animation
- Implement spelling letter-by-letter reveal
- Test both activities

### Week 3, Day 2: Multiple Choice & Fill Blank
- Sequential option reveal for multiple choice
- Selection feedback
- Fill blank word insertion
- Test both

### Week 3, Day 3: Synonym/Antonym & Title Cards
- Word pair entrance
- Relationship visualization
- Activity title cards (all activities)
- Test all 5 activities

### Week 3, Day 4: Polish & Refinement
- Tune animation timings
- Add activity icons
- Cross-activity consistency check
- Performance testing

### Week 3, Day 5: Buffer/QA
- Bug fixes
- Additional polish as time permits
- Documentation updates

## Success Criteria

### Must Have
- [ ] Each activity has unique entrance animation
- [ ] Flashcard flip effect works smoothly
- [ ] Multiple choice options appear sequentially
- [ ] Spelling has letter-by-letter feedback

### Should Have
- [ ] Activity title cards before each activity
- [ ] Incorrect answer animations per activity type
- [ ] Activity icons displayed consistently
- [ ] All animations maintain 60fps

### Nice to Have
- [ ] Synonym/antonym relationship arrows
- [ ] Fill blank word insertion zoom
- [ ] Activity-specific particle presets
- [ ] Sound effects per activity type

## Technical Considerations

### Performance
- Reuse tween objects where possible
- Don't create/destroy nodes frequently
- Test with all 5 activities running back-to-back

### Consistency
- Use same ease/transition curves across activities
- Timing should feel similar (0.3-0.5s standard)
- Don't make any activity feel slow

### Accessibility
- Animations don't obscure important text
- Can be skipped with any input
- Consider "reduced motion" setting

## Future Enhancements

### V2: Advanced Interactions
- Drag-and-drop for multiple choice
- Voice input for spelling
- Gesture-based synonym/antonym selection

### V3: Mini-Game Variants
- Flashcard memory matching game
- Multiple choice with time pressure
- Spelling race against clock

---

**Related Documents:**
- [Phase 5 Overview](./phase5-overview.md)
- [Juice & Effects PRD](./phase5-juice-and-effects.md)
- Activity scripts: `/vocab_game/scripts/activities/*.gd`
