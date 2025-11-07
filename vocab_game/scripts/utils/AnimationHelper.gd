extends Node
class_name AnimationHelper

# Animation constants for validation errors
const VALIDATION_FLASH_DURATION = 0.3
const VALIDATION_FLASH_COLOR = Color(1.0, 0.7, 0.7)  # Light red tint
const VALIDATION_SHAKE_DISTANCE = 5
const VALIDATION_SHAKE_STEP_DURATION = 0.05
const VALIDATION_SHAKE_ITERATIONS = 3

## Shows validation error feedback with flash and shake animation
## Works with any Control node
static func show_validation_error(node: Control) -> void:
	if not node or not node.is_inside_tree():
		return

	# Flash animation
	var original_color = node.modulate
	node.modulate = VALIDATION_FLASH_COLOR
	await node.get_tree().create_timer(VALIDATION_FLASH_DURATION).timeout

	if not node or not node.is_inside_tree():
		return
	node.modulate = original_color

	# Shake animation
	var original_pos = node.position
	for i in range(VALIDATION_SHAKE_ITERATIONS):
		if not node or not node.is_inside_tree():
			break
		node.position.x = original_pos.x - VALIDATION_SHAKE_DISTANCE
		await node.get_tree().create_timer(VALIDATION_SHAKE_STEP_DURATION).timeout
		if not node or not node.is_inside_tree():
			break
		node.position.x = original_pos.x + VALIDATION_SHAKE_DISTANCE
		await node.get_tree().create_timer(VALIDATION_SHAKE_STEP_DURATION).timeout

	if node and node.is_inside_tree():
		node.position = original_pos
