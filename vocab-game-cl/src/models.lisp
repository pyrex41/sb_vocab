(in-package #:vocab-game)

;;; Data structures matching Playcademy API

(defstruct word-data
  "Vocabulary word with metadata"
  id
  word
  definition
  example
  synonyms      ; list of strings
  antonyms      ; list of strings
  audio-url)

(defstruct activity
  "Single activity in a session"
  type          ; :flashcard :multiple-choice :spelling :fill-blank :synonym-antonym
  word          ; word-data struct
  options)      ; list of strings (for multiple choice)

(defstruct game-session
  "Active game session state"
  session-id
  grade
  activities    ; list of activity structs
  current-index
  score
  total-activities
  started-at
  user-id)

(defstruct attempt-result
  "Result of an answer submission"
  correct
  feedback
  correct-answer) ; only if incorrect

(defstruct session-summary
  "End-of-session statistics"
  total-activities
  correct-count
  accuracy
  words-practiced
  session-duration)

(defstruct progress-data
  "User progress tracking"
  total-words-practiced
  overall-accuracy
  sessions-completed
  mastery-levels) ; hash-table {:new 15 :learning 20 ...}
