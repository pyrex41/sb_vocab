// Playcademy Vocabulary Game - Client-side JavaScript

// Global state
let currentSessionId = null;
let currentActivityIndex = 0;
let totalActivities = 0;
let activityStartTime = null;

// ============================================
// UTILITY FUNCTIONS
// ============================================

function showLoading(show = true) {
    const overlay = document.getElementById('loading-overlay');
    if (overlay) {
        if (show) {
            overlay.classList.remove('hidden');
        } else {
            overlay.classList.add('hidden');
        }
    }
}

function showFeedback(correct, message, detail = '') {
    const overlay = document.getElementById('feedback-overlay');
    const content = overlay.querySelector('.feedback-content');
    const icon = document.getElementById('feedback-icon');
    const messageEl = document.getElementById('feedback-message');
    const detailEl = document.getElementById('feedback-detail');

    // Update content
    icon.textContent = correct ? '✓' : '✗';
    messageEl.textContent = message;
    detailEl.textContent = detail;

    // Update styling
    content.className = 'feedback-content ' + (correct ? 'correct' : 'incorrect');

    // Show overlay
    overlay.classList.remove('hidden');

    // Auto-hide after 1.5 seconds
    setTimeout(() => {
        overlay.classList.add('hidden');
    }, 1500);
}

function updateProgressBar(current, total) {
    const progressBar = document.getElementById('progress-bar');
    const counter = document.getElementById('activity-counter');

    if (progressBar) {
        const percentage = (current / total) * 100;
        progressBar.style.width = percentage + '%';
    }

    if (counter) {
        counter.textContent = `Activity ${current} of ${total}`;
    }
}

async function apiCall(method, endpoint, body = null) {
    try {
        const options = {
            method: method,
            headers: {
                'Content-Type': 'application/json',
            },
        };

        if (body) {
            options.body = JSON.stringify(body);
        }

        const response = await fetch(endpoint, options);
        const data = await response.json();

        if (data.error) {
            throw new Error(data.error.message || 'An error occurred');
        }

        return data;
    } catch (error) {
        console.error('API Error:', error);
        throw error;
    }
}

// ============================================
// MAIN MENU FUNCTIONS
// ============================================

async function startSession(grade) {
    try {
        showLoading(true);

        // Authenticate first
        await apiCall('GET', '/api/auth');

        // Get available courses
        const coursesData = await apiCall('GET', '/api/courses');
        const courses = coursesData.courses || [];

        if (courses.length === 0) {
            throw new Error('No courses available');
        }

        // Use the first course (or filter by grade if needed)
        const courseId = courses[0].courseId;

        // Start session
        const sessionData = await apiCall('POST', '/api/session/start', {
            courseId: courseId
        });

        currentSessionId = sessionData.sessionId;
        totalActivities = sessionData.itemCount || 10;

        // Redirect to game page
        window.location.href = '/game';

    } catch (error) {
        showLoading(false);
        alert('Failed to start session: ' + error.message);
    }
}

function viewProgress() {
    window.location.href = '/progress';
}

// ============================================
// GAME SESSION FUNCTIONS
// ============================================

async function loadNextActivity() {
    try {
        showLoading(true);

        const activityData = await apiCall('GET', '/api/activity/next');

        if (activityData.completed) {
            // Session completed
            await endSession();
            return;
        }

        // Update UI
        currentActivityIndex = activityData.index || (currentActivityIndex + 1);
        totalActivities = activityData.total || totalActivities;

        updateProgressBar(currentActivityIndex, totalActivities);

        // Record start time for latency calculation
        activityStartTime = Date.now();

        // Render activity
        await renderActivity(activityData);

        showLoading(false);

    } catch (error) {
        showLoading(false);
        alert('Failed to load activity: ' + error.message);
    }
}

async function renderActivity(activityData) {
    const container = document.getElementById('activity-container');
    const activityType = activityData.activityType;
    const word = activityData.word;
    const options = activityData.options;

    let html = '';

    switch (activityType) {
        case 'flashcard':
        case 'introduction':
            html = renderFlashcard(word);
            break;

        case 'multiple_choice':
        case 'definition_mc':
        case 'multiple-choice':
            html = renderMultipleChoice(word, options);
            break;

        case 'spelling':
        case 'spell_typed':
            html = renderSpelling(word);
            break;

        case 'fill_blank':
        case 'cloze_typed':
        case 'fill-blank':
            html = renderFillBlank(word);
            break;

        case 'synonym_antonym':
        case 'synonym-antonym':
            html = renderSynonymAntonym(word, options);
            break;

        default:
            html = '<div class="error-message">Unknown activity type: ' + activityType + '</div>';
    }

    container.innerHTML = html;

    // Focus on input if present
    const input = container.querySelector('input[type="text"]');
    if (input) {
        input.focus();
    }
}

// ============================================
// ACTIVITY RENDERING
// ============================================

function renderFlashcard(word) {
    return `
        <div class="activity flashcard-activity">
            <div class="activity-header">
                <h1>New Word!</h1>
            </div>
            <div class="flashcard-content">
                <div class="word-card">
                    <h2 class="word-display">${word.word}</h2>
                    <div class="definition-section">
                        <strong>Definition:</strong>
                        <p>${word.definition}</p>
                    </div>
                    ${word.example ? `
                        <div class="example-section">
                            <strong>Example:</strong>
                            <p><em>${word.example}</em></p>
                        </div>
                    ` : ''}
                </div>
            </div>
            <div class="activity-actions">
                <button type="button" class="btn btn-primary btn-large" onclick="submitFlashcardAnswer()">
                    Got it!
                </button>
            </div>
        </div>
    `;
}

function renderMultipleChoice(word, options) {
    const optionButtons = options.map(option => `
        <button type="button" class="option-btn" onclick="submitMultipleChoiceAnswer('${escapeHtml(option)}')">
            ${option}
        </button>
    `).join('');

    return `
        <div class="activity multiple-choice-activity">
            <div class="activity-header">
                <h1>Choose the Correct Definition</h1>
            </div>
            <div class="question-section">
                <h2 class="word-display">${word.word}</h2>
                <p class="prompt">What does this word mean?</p>
            </div>
            <div class="options-container">
                ${optionButtons}
            </div>
        </div>
    `;
}

function renderSpelling(word) {
    return `
        <div class="activity spelling-activity">
            <div class="activity-header">
                <h1>Spell the Word</h1>
            </div>
            <div class="spelling-content">
                <div class="audio-section">
                    <p class="instruction">Listen carefully and type the word you hear:</p>
                    <button type="button" class="btn btn-secondary play-audio-btn" onclick="playWordAudio()">
                        🔊 Play Word
                    </button>
                </div>
                <div class="input-section">
                    <input type="text" id="spelling-input" class="spelling-input"
                           placeholder="Type the word here..." autocomplete="off" autofocus
                           onkeypress="if(event.key==='Enter') submitSpellingAnswer()">
                </div>
            </div>
            <div class="activity-actions">
                <button type="button" class="btn btn-primary" onclick="submitSpellingAnswer()">
                    Submit
                </button>
            </div>
        </div>
    `;
}

function renderFillBlank(word) {
    // Create sentence with blank
    const sentenceWithBlank = word.example
        ? word.example.replace(new RegExp('\\b' + word.word + '\\b', 'gi'), '___')
        : 'Complete the sentence with the correct word: ___';

    return `
        <div class="activity fill-blank-activity">
            <div class="activity-header">
                <h1>Fill in the Blank</h1>
            </div>
            <div class="fill-blank-content">
                <div class="sentence-section">
                    <p class="sentence-with-blank">${sentenceWithBlank}</p>
                </div>
                <div class="input-section">
                    <label for="fill-blank-input">Your answer:</label>
                    <input type="text" id="fill-blank-input" class="fill-blank-input"
                           placeholder="Type the missing word..." autocomplete="off" autofocus
                           onkeypress="if(event.key==='Enter') submitFillBlankAnswer()">
                </div>
            </div>
            <div class="activity-actions">
                <button type="button" class="btn btn-primary" onclick="submitFillBlankAnswer()">
                    Submit
                </button>
            </div>
        </div>
    `;
}

function renderSynonymAntonym(word, options) {
    const optionButtons = options.map(option => `
        <button type="button" class="option-btn" onclick="submitSynonymAntonymAnswer('${escapeHtml(option)}')">
            ${option}
        </button>
    `).join('');

    return `
        <div class="activity synonym-antonym-activity">
            <div class="activity-header">
                <h1>Find the Related Word</h1>
            </div>
            <div class="question-section">
                <p class="prompt">Select a word related to: <strong>${word.word}</strong></p>
            </div>
            <div class="options-container">
                ${optionButtons}
            </div>
        </div>
    `;
}

// ============================================
// ANSWER SUBMISSION
// ============================================

async function submitAnswer(answer) {
    try {
        showLoading(true);

        const result = await apiCall('POST', '/api/activity/submit', {
            answer: answer
        });

        showLoading(false);

        // Show feedback
        const message = result.correct ? 'Correct!' : 'Not quite';
        const detail = result.feedback || '';
        showFeedback(result.correct, message, detail);

        // Wait for feedback, then load next activity
        setTimeout(() => {
            loadNextActivity();
        }, 1600);

    } catch (error) {
        showLoading(false);
        alert('Failed to submit answer: ' + error.message);
    }
}

function submitFlashcardAnswer() {
    submitAnswer('acknowledged');
}

function submitMultipleChoiceAnswer(answer) {
    submitAnswer(answer);
}

function submitSpellingAnswer() {
    const input = document.getElementById('spelling-input');
    const answer = input ? input.value.trim() : '';

    if (!answer) {
        alert('Please type your answer');
        return;
    }

    submitAnswer(answer);
}

function submitFillBlankAnswer() {
    const input = document.getElementById('fill-blank-input');
    const answer = input ? input.value.trim() : '';

    if (!answer) {
        alert('Please type your answer');
        return;
    }

    submitAnswer(answer);
}

function submitSynonymAntonymAnswer(answer) {
    submitAnswer(answer);
}

// ============================================
// SESSION MANAGEMENT
// ============================================

async function endSession() {
    try {
        showLoading(true);

        await apiCall('POST', '/api/session/end', {});

        // Redirect to results page
        window.location.href = '/results';

    } catch (error) {
        showLoading(false);
        alert('Failed to end session: ' + error.message);
    }
}

function quitSession() {
    if (confirm('Are you sure you want to quit? Your progress will be saved.')) {
        endSession();
    }
}

// ============================================
// AUDIO PLAYBACK
// ============================================

function playWordAudio() {
    // In a real implementation, this would play the word's audio
    // For now, we'll show a message
    alert('Audio playback would play here. Feature requires backend audio URL support.');
}

// ============================================
// HELPER FUNCTIONS
// ============================================

function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

// ============================================
// AUTO-START ON GAME PAGE
// ============================================

window.addEventListener('DOMContentLoaded', () => {
    // If we're on the game page and there's an activity container, load the first activity
    if (window.location.pathname === '/game') {
        const container = document.getElementById('activity-container');
        if (container) {
            loadNextActivity();
        }
    }
});
