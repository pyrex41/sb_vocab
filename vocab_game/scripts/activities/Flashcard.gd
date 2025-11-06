extends Control

signal answer_submitted(answer)

@onready var word_label = $VBoxContainer/WordLabel
@onready var definition_label = $VBoxContainer/DefinitionLabel
@onready var example_label = $VBoxContainer/ExampleLabel
@onready var continue_button = $VBoxContainer/ContinueButton

func _ready():
	continue_button.pressed.connect(_on_continue_pressed)

func setup(activity_data: Dictionary):
	var word_data = activity_data.word
	word_label.text = word_data.word
	definition_label.text = word_data.definition
	example_label.text = '"' + word_data.example + '"'

func _on_continue_pressed():
	# Flashcards don't require grading, just acknowledgment
	answer_submitted.emit("continue")
