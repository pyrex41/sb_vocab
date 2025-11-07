;;;; activities.lisp
;;;; Activity rendering and HTML generation for different activity types

(in-package #:vocab-game-cl)

(defun render-word-info (word-data)
  "Render word information (used in various activities)"
  (with-html-output-to-string (s)
    (let ((word (extract-json-value word-data :|word|))
          (definition (extract-json-value word-data :|definition|))
          (example (extract-json-value word-data :|example|)))
      (htm
       (:div :class "word-info"
             (:h2 :class "word-display"
                  (str (or word "Unknown"))))
       (when definition
         (htm (:p :class "definition"
                  (:strong "Definition: ")
                  (str definition))))
       (when example
         (htm (:p :class "example"
                  (:em "Example: ")
                  (str example))))))))

(defun render-flashcard (activity-data)
  "Render flashcard activity (introduction to a word)"
  (with-html-output-to-string (s)
    (let* ((word (extract-json-value activity-data :|word|))
           (word-text (extract-json-value word :|word|))
           (definition (extract-json-value word :|definition|))
           (example (extract-json-value word :|example|)))
      (htm
       (:div :class "activity flashcard-activity"
             (:div :class "activity-header"
                   (:h1 "New Word!"))

             (:div :class "flashcard-content"
                   (:div :class "word-card"
                         (:h2 :class "word-display"
                              (str (or word-text "Unknown")))

                         (when definition
                           (htm (:div :class "definition-section"
                                      (:strong "Definition:")
                                      (:p (str definition)))))

                         (when example
                           (htm (:div :class "example-section"
                                      (:strong "Example:")
                                      (:p (:em (str example))))))))

             (:div :class "activity-actions"
                   (:button :type "button"
                            :class "btn btn-primary btn-large"
                            :onclick "submitFlashcardAnswer()"
                            "Got it!")))))))

(defun render-multiple-choice (activity-data)
  "Render multiple choice activity"
  (with-html-output-to-string (s)
    (let* ((word (extract-json-value activity-data :|word|))
           (word-text (extract-json-value word :|word|))
           (options (extract-json-value activity-data :|options|)))
      (htm
       (:div :class "activity multiple-choice-activity"
             (:div :class "activity-header"
                   (:h1 "Choose the Correct Definition"))

             (:div :class "question-section"
                   (:h2 :class "word-display"
                        (str (or word-text "Unknown")))
                   (:p :class "prompt"
                       "What does this word mean?"))

             (:div :class "options-container"
                   (loop for option in options
                         for i from 0
                         do (htm
                             (:button :type "button"
                                      :class "option-btn"
                                      :data-answer (str option)
                                      :onclick (format nil "submitMultipleChoiceAnswer('~A')"
                                                     (sanitize-string option))
                                      (str option))))))))))

(defun render-spelling (activity-data)
  "Render spelling activity"
  (with-html-output-to-string (s)
    (let* ((word (extract-json-value activity-data :|word|))
           (word-text (extract-json-value word :|word|)))
      (htm
       (:div :class "activity spelling-activity"
             (:div :class "activity-header"
                   (:h1 "Spell the Word"))

             (:div :class "spelling-content"
                   (:div :class "audio-section"
                         (:p :class "instruction"
                             "Listen carefully and type the word you hear:")
                         (:button :type "button"
                                  :class "btn btn-secondary play-audio-btn"
                                  :onclick "playWordAudio()"
                                  (:i :class "icon-sound")
                                  " Play Word"))

                   (:div :class "input-section"
                         (:input :type "text"
                                 :id "spelling-input"
                                 :class "spelling-input"
                                 :placeholder "Type the word here..."
                                 :autocomplete "off"
                                 :autofocus "autofocus"
                                 :onkeypress "if(event.key==='Enter') submitSpellingAnswer()")))

             (:div :class "activity-actions"
                   (:button :type "button"
                            :class "btn btn-primary"
                            :onclick "submitSpellingAnswer()"
                            "Submit")))))))

(defun render-fill-blank (activity-data)
  "Render fill-in-the-blank activity"
  (with-html-output-to-string (s)
    (let* ((word (extract-json-value activity-data :|word|))
           (word-text (extract-json-value word :|word|))
           (example (extract-json-value word :|example|))
           ;; Create sentence with blank
           (sentence-with-blank (if example
                                   (cl-ppcre:regex-replace-all
                                    (format nil "\\b~A\\b" word-text)
                                    example
                                    "___"
                                    :preserve-case t)
                                   "Complete the sentence with the correct word: ___")))
      (htm
       (:div :class "activity fill-blank-activity"
             (:div :class "activity-header"
                   (:h1 "Fill in the Blank"))

             (:div :class "fill-blank-content"
                   (:div :class "sentence-section"
                         (:p :class "sentence-with-blank"
                             (str sentence-with-blank)))

                   (:div :class "input-section"
                         (:label :for "fill-blank-input"
                                 "Your answer:")
                         (:input :type "text"
                                 :id "fill-blank-input"
                                 :class "fill-blank-input"
                                 :placeholder "Type the missing word..."
                                 :autocomplete "off"
                                 :autofocus "autofocus"
                                 :onkeypress "if(event.key==='Enter') submitFillBlankAnswer()")))

             (:div :class "activity-actions"
                   (:button :type "button"
                            :class "btn btn-primary"
                            :onclick "submitFillBlankAnswer()"
                            "Submit")))))))

(defun render-synonym-antonym (activity-data)
  "Render synonym/antonym selection activity"
  (with-html-output-to-string (s)
    (let* ((word (extract-json-value activity-data :|word|))
           (word-text (extract-json-value word :|word|))
           (options (extract-json-value activity-data :|options|))
           ;; Determine if this is synonym or antonym (could be in activity type or derived)
           (is-synonym t) ; Default to synonym, could be extracted from activity metadata
           (prompt-text (if is-synonym
                           (format nil "Select a SYNONYM for: ~A" word-text)
                           (format nil "Select an ANTONYM for: ~A" word-text))))
      (htm
       (:div :class "activity synonym-antonym-activity"
             (:div :class "activity-header"
                   (:h1 (str (if is-synonym "Find the Synonym" "Find the Antonym"))))

             (:div :class "question-section"
                   (:p :class "prompt"
                       (str prompt-text)))

             (:div :class "options-container"
                   (loop for option in options
                         for i from 0
                         do (htm
                             (:button :type "button"
                                      :class "option-btn"
                                      :data-answer (str option)
                                      :onclick (format nil "submitSynonymAntonymAnswer('~A')"
                                                     (sanitize-string option))
                                      (str option))))))))))

(defun render-activity (activity-data)
  "Render an activity based on its type"
  (let ((activity-type (extract-json-value activity-data :|activityType|)))
    (cond
      ((or (string-equal activity-type "flashcard")
           (string-equal activity-type "introduction"))
       (render-flashcard activity-data))

      ((or (string-equal activity-type "multiple_choice")
           (string-equal activity-type "definition_mc")
           (string-equal activity-type "multiple-choice"))
       (render-multiple-choice activity-data))

      ((or (string-equal activity-type "spelling")
           (string-equal activity-type "spell_typed"))
       (render-spelling activity-data))

      ((or (string-equal activity-type "fill_blank")
           (string-equal activity-type "cloze_typed")
           (string-equal activity-type "fill-blank"))
       (render-fill-blank activity-data))

      ((or (string-equal activity-type "synonym_antonym")
           (string-equal activity-type "synonym-antonym"))
       (render-synonym-antonym activity-data))

      (t
       ;; Unknown activity type
       (with-html-output-to-string (s)
         (htm
          (:div :class "activity unknown-activity"
                (:div :class "error-message"
                      (:h2 "Unknown Activity Type")
                      (:p (str (format nil "Activity type '~A' is not recognized." activity-type)))))))))))
