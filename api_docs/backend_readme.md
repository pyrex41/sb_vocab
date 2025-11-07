# Playcademy Vocabulary — Backend Readme (MVP)

A concise, high-signal guide for backend devs: auth, data model, learning flow, activities, config, and GenAI backend.

> **Stack:** Bun + Hono (HTTP), PostgreSQL + Drizzle ORM.  
> **Base URL:** `http://localhost:${API_PORT || 3000}/v1` (cookies required).  
> **Auth cookie:** `vocab_auth` (HTTP‑only JWT). CORS allows one configured origin with credentials.

---

## 1) Auth, Roles, CORS/CSRF, Errors, Rate Limiting

### Roles

- **student** – sessions + progress
- **admin** – full content + roster + ops

### Authentication (MVP)

- **POST** `/v1/auth/mock-login` — body `{ "email": "<existing user>" }`. Sets `vocab_auth` cookie. _No password._
- **POST** `/v1/auth/logout` — clears cookie.
- **GET** `/v1/auth/me` — returns current user:
  ```json
  {
    "user": {
      "userId": "...",
      "email": "...",
      "displayName": "...",
      "role": "student|admin",
      "orgId": "..."
    }
  }
  ```

### Cookies/CORS

- Frontend must send credentials (include cookies). One allowed origin (`CORS_ORIGIN` env).

### CSRF (cookie-auth)

- For `POST`/`PATCH`/`DELETE`, `Origin` header must match allowed origin (enforced unless `NODE_ENV=test`).

### Rate Limiting

- **Default:** 100 requests/min/IP
- **Headers:** `X-RateLimit-Limit`, `X-RateLimit-Remaining`, `X-RateLimit-Reset`
- **When exceeded:** 429 + `Retry-After` header
- **Disabled** in tests (`NODE_ENV=test`)

### Errors (standard shape)

```json
{
  "error": {
    "code": "UNAUTHORIZED|FORBIDDEN|NOT_FOUND|BAD_REQUEST|VALIDATION_ERROR|INTERNAL_ERROR|TOO_MANY_REQUESTS",
    "message": "...",
    "details": {}
  }
}
```

### IDs, Time, Pagination

- **IDs:** UUIDv4
- **Timestamps:** ISO strings
- **Pagination:** `limit` & `offset`; responses include `pagination` or `total`

---

## 2) API Map (what's available)

- `/v1/auth`: login/logout/me
- `/v1/session`:
  - `POST /start`
  - `GET /active`
  - `POST /:id/next`
  - `POST /:id/attempt`
  - `POST /:id/hint`
  - `POST /:id/finalize`
- `/v1/me` (student self):
  - `GET /progress/course/:id`
  - `GET /progress/course/:id/daily-plan`
  - `GET /progress/course/:id/lessons`
  - `GET /progress/course/:id/words`
  - `GET /xp/ledger`
- `/v1/content` (admin):
  - Words: CRUD + search, enriched read, senses, examples, relations, variants, activity demo
  - Lessons: CRUD + search + reorder + add/remove words
  - Courses: CRUD + list/read + reorder + copy lesson
  - Imports/Exports
- `/v1/admin`:
  - Students + Classes CRUD/assignments
  - Analytics exports
  - Ops (ingest, seed progress)
- `/v1/admin/genai`: GenAI sprint workflow (see section 9)
- **Infra/hosting:**
  - `GET /health` and `/v1/health`
  - Static media: `GET /static/*` (no auth)
  - In production: `GET /assets/*` (built client), SPA fallback

---

## 3) Key Enums (wire format)

- **pos**: `"noun"|"verb"|"adjective"|"adverb"|"preposition"|"conjunction"|"interjection"|"pronoun"|"proper_noun"|"determiner"|"particle"`
- **activityType**: `"flashcard_usage"|"connect_def"|"context_cloze"|"select_usage"|"synonym_mcq"|"spell_typed"|"definition_typed"|"sentence_typed_gen"|"paraphrase_typed_gen"`
- **media.kind**: `"audio"|"image"`
- **media.role**: `"word_pronunciation"|"alt_pronunciation"|"sentence_audio"|"illustration"`

---

## 4) Data Model (client-visible essentials)

### Roster

- **org** `{ orgId, name }`
- **user** `{ userId, orgId, email, displayName, role, timezone?, reviewRolloverHour? }`
- **class** `{ classId, orgId, name, schoolYear }`
- **enrollment** `{ classId, userId, role }`
- **user_course** `{ userId, courseId }`
- **class_course** `{ classId, courseId }`

### Curriculum

- **course** `{ courseId, grade(unique), title, status, defaultNewWordsPerSession, min/maxNewPerSession, maxWordsPerSession, maxReviewWordsPerSession, sessionTimeBudgetS }`
- **lesson** `{ lessonId, courseId, title, orderNo, targetNewPerSession, maxWordsPerSession?, maxReviewWordsPerSession? }`
- **lesson_word** `{ lessonId, wordId, orderNo }`

### Lexicon

- **word** `{ wordId, headword, lang="en", pos, definition, notes?, status }`
- **word_sense** `{ senseId, wordId, definition, orderNo, isPrimary }`
- **example** `{ exampleId, wordId, senseId?, kind: "correct_usage"|"incorrect_usage", text, orderNo, status, source?, notes? }`
- **example_word_option** `{ exampleId, headword, isCorrect, orderNo }` — curated options for `context_cloze`
- **media** `{ mediaId, kind, url, ... }`, **word_media** `{ wordId, mediaId, role, orderNo, senseId? }`
- **lexical_relation** `{ relationId, relationType, fromWordId, toWordId, ... }`
- **orth_variant** `{ variantId, wordId, form, region?, isPreferred }`
- **word_meaning_distractor** `{ wordId, distractorText, orderNo }` — curated distractors for `connect_def`
- **word_synonym** `{ wordId, synonymText, orderNo }` — curated synonyms for `synonym_mcq`
- **word_synonym_distractor** `{ wordId, distractorText, orderNo }` — curated distractors for `synonym_mcq`
- **word_cue_token** `{ wordId, token, pos?, orderNo }` — cue tokens for `sentence_typed_gen`

### Session & FSRS

- **session** `{ sessionId, userId, courseId, lessonId?, startTs, endTs?, state, plannedDurationS }`
- **session_item** `{ itemId, sessionId, activityType, wordId, orderNo, paramsJson?, state, section: "new"|"review" }`
- **attempt** `{ attemptId, itemId, answerJson, correct, score, hintsUsed, retriesUsed, latencyMs, startedTs, endedTs }`
- **session_word** `{ sessionId, wordId, spellingDone, meaningDone, spellingCorrect, meaningCorrect, hintsTotal, retriesTotal, slowFlag, fsrsCommitted }`
- **srs_state** `{ userId, wordId, courseId, stability, difficulty, dueTs, reps, lapses, lastReviewTs, lastRating, updatedTs }`
- **srs_event** `{ srsEventId, userId, wordId, courseId, sessionId?, rating, prevStability, newStability, prevDifficulty, newDifficulty, prevDueTs, newDueTs, elapsedDays, hasError, eventTs }`

### Progress & XP

- **progress_snapshot** `{ userId, courseId, newCnt, learningCnt, reviewingCnt, masteredCnt, updatedTs }`
- **session_summary** `{ sessionId, itemsAnswered, accuracy, xpAwarded, finalizedTs }`

---

## 5) Activities & Params (server contract)

Activity types and current param shapes from handlers in `api/services/activities/`. Server is authoritative for scoring and correctness; client never sends correctness flags.

### flashcard_usage

- **Params sent:**
  ```json
  {
    "options": [
      { "exampleId": "uuid", "text": "..." },
      { "exampleId": "uuid", "text": "..." }
    ]
  }
  ```

  - 1 `correct_usage` + 3 `incorrect_usage`, deterministically selected per `(sessionId, wordId)`.
- **Answer:** `"exampleId"` (string, UUID)
- **Scoring:** Requires `timeSpentS ≥ 6s` (MIN_FLASHCARD_TIME_S)

### select_usage

- **Params:** Same structure as `flashcard_usage`; no definition shown
- **Answer:** `"exampleId"` (string, UUID)

### connect_def

- **Params sent:**
  ```json
  {
    "options": ["conspire", "inspire", "..."]
  }
  ```

  - Options are headword strings; correct is target headword. Distractors come from `word_meaning_distractor` (3 required).
- **Answer:** `"headword"` (string)

### context_cloze (Connect Sentence)

- **Params sent:**
  ```json
  {
    "sentence": "The team began to ____ against the manager.",
    "options": ["conspire", "inspire", "..."]
  }
  ```

  - `sentence` is a `correct_usage` example with headword blanked
  - `options` come from curated `example_word_option` (4 per example including target headword)
- **Answer:** `"headword"` (string)

### synonym_mcq

- **Params sent:**
  ```json
  {
    "targetWord": { "wordId": "uuid", "headword": "conspire" },
    "options": [{ "headword": "plot" }, { "headword": "..." }]
  }
  ```

  - Correct answer is one curated synonym from `word_synonym`
  - Distractors pulled from `word_synonym_distractor` (3 required)
- **Answer:** `"headword"` (string)

### spell_typed

- **Params:** `null`
- **Answer:** User typed string; normalized (lowercase, removes diacritics/punctuation); orthographic variants accepted
- **Hints:** 2 levels available via `/hint`

### definition_typed

- **Params:** `null`
- **Answer:** User typed string; uses same normalization as spelling
- **Hints:** 2 levels available via `/hint`

### sentence_typed_gen

- **Params sent:**
  ```json
  {
    "cueWord": "...",
    "cuePos": "noun|verb|..."
  }
  ```

  - Cue is chosen deterministically from `word_cue_token` (≥1 required)
- **Answer:** User typed sentence
- **Scoring:** GenAI graded (grammar ≥ 0.7, meaning ≥ 0.7). Slow responses (≥ 45s) downgrade.

### paraphrase_typed_gen

- **Params:** `null`
- **Answer:** User typed paraphrase
- **Scoring:** GenAI graded (grammar ≥ 0.7, meaning ≥ 0.7). Slow responses (≥ 45s) downgrade.

### Server-side scoring highlights

- **Flashcard** requires min time; others ignore timing except slow flags affecting FSRS rating.
- **Typed activities:** server computes correctness; hints/retries/slow mark as "Hard".
- **GenAI-graded activities:**
  - `sentence_typed_gen`: label is `good|hard|again` based on headword presence, cue used, and thresholds.
  - `paraphrase_typed_gen`: label `good|hard|again` based on grammar/meaning thresholds.

### Attempt limits (per item)

- **Default 1 attempt:** MCQs and GenAI-graded tasks
- **2 attempts (initial + 1 retry):** `spell_typed` and `definition_typed`

### Determinism

- Example/cue selection uses seeded randomness `(sessionId, wordId)` for reproducibility within a session.

### Hints

- **POST** `/v1/session/:id/hint { itemId, currentHints }` for `spell_typed` and `definition_typed`
- **Strategies:**
  - **Spelling:** level 1 first letter; level 2 morphology+length (fallback: length only)
  - **Definition-typed:** level 1 sentence cloze from `correct_usage` example (blanked headword); level 2 first letter + length
- **Max:** 2 hints per item

---

## 6) Session Lifecycle

### Start / Resume

**POST** `/v1/session/start`

- Body (optional): `{ "courseId": "uuid", "lessonId": "uuid", "timeBudgetS": number }`
- If recent active session exists (< 24h), returns it (`resuming=true`)
- **One active session** per student at a time

**GET** `/v1/session/active` → `{ hasActiveSession, sessionId? }`

Response on start:

```json
{
  "sessionId": "...",
  "itemCount": 12,
  "newWordActivityCount": 9,
  "plannedDurationS": 600,
  "resuming": false,
  "completedItems": 0
}
```

**Session resumption:** If student disconnects, calling `/session/start` again returns the same session (if < 24h old). Delivered items are reset to pending. Older sessions are auto-abandoned.

### Next item

**POST** `/v1/session/:id/next`

Returns:

```json
{
  "itemId":"...",
  "activityType":"spell_typed|connect_def|...",
  "phase":"new|review",
  "phaseProgress": { "current": 3, "total": 12 },
  "word": {
    "wordId":"...",
    "headword": "...",    // omitted for spell_typed, definition_typed, paraphrase_typed_gen
    "definition":"...",
    "pos":"adjective",
    "media":[ { "mediaId":"...", "kind":"audio|image", "url":"...", "mimeType":"...", "role":"word_pronunciation", "orderNo":1 } ]
  },
  "params": { } | null
}
```

### Submit attempt

**POST** `/v1/session/:id/attempt`

Body:

```json
{
  "itemId": "uuid",
  "answer": "string|object", // see per-activity above
  "latencyMs": 2300,
  "hintsUsed": 0,
  "retriesUsed": 0,
  "timeSpentS": 12, // include for flashcard timing
  "attemptId": "uuid" // idempotency token
}
```

Response:

```json
{
  "attemptId": "...",
  "correct": true,
  "score": 1,
  "feedback": "optional",
  "recycled": false,
  "recycleItemId": null,
  "maxRecyclesReached": false
}
```

**Recycling:**

- Incorrect, slow, or hinted successes recycle the item to the tail of the appropriate section (new vs review).
- **Min gap K:** 4 items between original and recycle
- **Per-word cap:** 3 recycles max; when reached, item is marked answered.

**FSRS commits (during session, not at finalize):**

- FSRS updates happen **immediately** when a word reaches its "boundary":
  - **Boundary** = spelling+meaning both done, OR spelling hit max attempts
  - Applies to both NEW and REVIEW words
- **Composite rating** considers both spelling AND meaning performance:
  - If spelling OR meaning failed → Rating 1 (Again)
  - If correct but hints/retries/slow → Rating 2 (Hard)
  - If perfect first-try → Rating 3 (Good)
- Uses actual review timestamp for FSRS calculations; `finalize` only writes summary (XP, accuracy)

### Finalize

**POST** `/v1/session/:id/finalize`

- Marks delivered-but-unanswered items as skipped, closes session, writes `session_summary`, updates progress snapshot.

Response:

```json
{
  "sessionId": "...",
  "itemsAnswered": 20,
  "accuracy": 0.9,
  "xpAwarded": 10,
  "summary": { "correct": 18, "total": 20 }
}
```

**Idempotent attempts:**

- Client generates `attemptId` (UUIDv4) before submitting
- If network retry happens, server returns cached result (same `attemptId` → same response)
- Prevents double-recording on flaky connections

---

## 7) Learning Logic (FSRS + Session Planning)

### FSRS (Spaced Repetition)

**Algorithm:** ts-fsrs v4 with 90% target retention, continuous time (fractional days).

**Stability buckets:**

- **NEW**: no `srs_state` yet (never introduced)
- **LEARNING**: stability < 7 days (early acquisition)
- **REVIEWING**: 7 ≤ stability < 14 days (consolidation)
- **MASTERED**: stability ≥ 14 days (long-term retention)

**Ratings (composite, considers both spelling + meaning):**

- **1 (Again)**: spelling OR meaning failed
- **2 (Hard)**: correct but with hints/retries/slow
- **3 (Good)**: correct on first try, not slow
- **4 (Easy)**: reserved (infrastructure ready but disabled; requires latency < 6s + stability ≥ 21d)

**Commit timing:** Immediate at word boundary (when spelling+meaning done, OR spelling hit max attempts). Uses actual review timestamp for FSRS. No commits happen at finalize.

**Due scheduling:**

- `dueTs ≤ now` sorted oldest first
- Due timestamps snapped to user's `reviewRolloverHour` in their timezone (default 4 AM) — ensures "today's" reviews align to class schedules
- **Durability** = current retention probability (0-1), computed as \( 0.9^{\text{days_since_review} / \text{stability}} \)

**Error-guarding:**

- `srs_event` table logs every FSRS update with deltas (prev/new stability/difficulty/due), `elapsedDays`, and `hasError` flag when fallback path used
- Sanity guards: max elapsed days (3 years), baseline stability (0.15), baseline difficulty (5.0)

### Session Planning (how items are chosen)

**Input:** `{ userId, courseId, lessonId?, timeBudgetS? }`

**NEW words first** (strict per-word sequence):

- **Lesson selection:** requested → first with unintroduced words → first lesson
- **Eligible:** words with no `srs_state` (never introduced)
- **Target count:** `lesson.targetNewPerSession` (fallback `course.defaultNewWordsPerSession`), clamped to min 2, max 6
- **Per-word sequence:** flashcard → Select Usage (meaning) → Spelling
- **Why NEW first?** Foundation building; avoids mixing interference patterns with reviews

**REVIEW words next** (due items):

- **Eligible:** `dueTs ≤ now`, ordered by `dueTs` (oldest first)
- **Interference control:** avoid scheduling two review words sharing the same root morpheme
- **Per-word sequence:** both Spelling AND one meaning activity (matrix-selected)
- **Plan order:** all review spellings first, then all review meanings (meanings unlock after spelling boundary reached)

**Caps enforced:**

- `maxWordsPerSession` (default 20): total words per session
- `maxReviewWordsPerSession` (default 40): review words only
- `sessionTimeBudgetS` (default 600): estimated using per-activity durations

**Capability checks:**

- Words probed for content requirements (examples, options, relations, cue tokens)
- Automatic fallback to safe alternatives if content missing

### Activity Plan Matrix (stability × grade band)

**Grade bands:**

- **G3_5**: grades ≤ 5
- **G6_7**: grades 6-7
- **G8P**: grades 8+

**Matrix:**

| Stability     | G3_5                                                | G6_7                                                                         | G8P                                                                                   |
| ------------- | --------------------------------------------------- | ---------------------------------------------------------------------------- | ------------------------------------------------------------------------------------- |
| **NEW**       | Spelling: `spell_typed`<br>Meaning: `select_usage`  | Spelling: `spell_typed`<br>Meaning: `select_usage`                           | Spelling: `spell_typed`<br>Meaning: `select_usage`                                    |
| **LEARNING**  | Spelling: `spell_typed`<br>Meaning: base pool¹      | Spelling: `spell_typed`<br>Meaning: base pool¹                               | Spelling: `spell_typed`<br>Meaning: base pool¹                                        |
| **REVIEWING** | Spelling: `spell_typed`<br>Meaning: base pool¹      | Spelling: `spell_typed`<br>Meaning: `synonym_mcq`, `sentence_typed_gen`      | Spelling: `spell_typed`<br>Meaning: `synonym_mcq`, `sentence_typed_gen`               |
| **MASTERED**  | Spelling: `definition_typed`<br>Meaning: base pool¹ | Spelling: `definition_typed`<br>Meaning: `synonym_mcq`, `sentence_typed_gen` | Spelling: `definition_typed`<br>Meaning: `sentence_typed_gen`, `paraphrase_typed_gen` |

¹ **Base meaning pool:** `connect_def`, `select_usage`, `context_cloze` (chosen uniformly at random)

---

## 8) Progress & XP

**Student self endpoints:**

- **GET** `/v1/me/progress/course/:id`

  ```json
  { "progress": { "new": 42, "learning": 18, "reviewing": 27, "mastered": 9, "updatedAt": "..." } }
  ```

- **GET** `/v1/me/progress/course/:id/daily-plan`

  ```json
  {
    "plan": {
      "newWords": 3,
      "reviewWords": 12,
      "estimatedMinutes": 9,
      "maxWordsPerSession": 20,
      "totalCap": 20
    }
  }
  ```

  - `totalCap` is a backward-compatible alias for `maxWordsPerSession`

- **GET** `/v1/me/progress/course/:id/lessons`

  ```json
  {
    "lessons": [
      {
        "lessonId": "...",
        "lessonTitle": "...",
        "orderNo": 1,
        "totalWords": 12,
        "introducedWords": 9
      }
    ]
  }
  ```

- **GET** `/v1/me/progress/course/:id/words?lessonId=&bucket=new|learning|reviewing|mastered&stabilityMin=&stabilityMax=&status=due|upcoming&includeStats=true&limit=&offset=`

  Returns `{ words: [...], total }`. Example word:

  ```json
  {
    "wordId": "...",
    "headword": "conspire",
    "pos": "verb",
    "stability": 24.1,
    "durability": 0.86,
    "nextDue": "2025-10-12T00:00:00Z",
    "bucket": "reviewing",
    "lessonId": "...",
    "lessonTitle": "Lesson 3",
    "stats": {
      // only if includeStats=true
      "totalAttempts": 8,
      "successRate": 0.75,
      "avgLatencyMs": 1234,
      "totalHintsUsed": 2,
      "timesReviewed": 5,
      "lastAttemptDate": "2025-10-10T14:30:00Z"
    }
  }
  ```

- **GET** `/v1/me/xp/ledger` → per-session XP ledger + total:
  ```json
  {
    "entries": [
      {
        "sessionId": "...",
        "itemsAnswered": 20,
        "accuracy": 0.9,
        "xpAwarded": 10,
        "finalizedTs": "..."
      }
    ],
    "totalXp": 480
  }
  ```

**XP formula:**

- Accuracy ≥ 80%: **1 XP/min**
- 65–79%: **0.5 XP/min**
- < 65%: **0 XP**

---

## 9) GenAI Backend (Sprint + Worker + Commit)

### What it does

Creates generation batches/jobs for headwords, generates text + media, stores curated artifacts required by activities, and can auto-map to lessons.

### Data model (core tables)

- **prompt**, **prompt_version** (not used in current sprint flow — prompts are code-based)
- **generation_batch** `{ batchId, promptVersionId (placeholder), scopeJson: { grade, courseTitle, lessonTitle }, status, ... }`
- **generation_job** `{ jobId, batchId, targetKey=headword, inputJson?, outputJsonRaw?, outputJson?, status, attempts, retryAfterTs, ... }`

### Endpoints (admin-only)

- **POST** `/v1/admin/genai/sprint`
  - Body: `{ words: string[], grade: 1..12, courseTitle?: string, lessonTitle?: string }`
  - Creates batch + queued jobs for each headword
  - Response: `{ batchId, jobsCreated, jobIds }`
- **GET** `/v1/admin/genai/sprint/:batchId/status`
  - Returns counts by job status: `{ total, completed, inProgress, counts, done }`
- **GET** `/v1/admin/genai/sprint/:batchId/results`
  - Returns `{ succeeded: [ { jobId, headword, bundle } ], failed: [ { jobId, headword, error } ] }`
- **GET** `/v1/admin/genai/sprint/:batchId/failures`
  - Returns failed jobs with metadata for manual retry UIs
- **POST** `/v1/admin/genai/retry-failed`
  - Body: `{ batchId?: "uuid" }`
  - Resets matching failed jobs to `queued`

### Worker (run off-process)

**Entry:** `api/services/genai/worker.ts`

**Two pools:**

- **Text workers:** claim `queued` → run `generateTextOnly` → write `outputJsonRaw` → `status: text_complete`
- **Media workers:** claim `text_complete` → run `generateMediaOnly` → assemble `WordBundle` → upload bundle JSON to S3 → `status: succeeded`

**Concurrency defaults:**

- `TEXT_POOL=80` (env override via `TEXT_POOL`)
- `MEDIA_IMAGE=20`, `MEDIA_TTS=20` (env override via `MEDIA_POOL`)

**Run:** `bun run api/services/genai/worker.ts`

### Generation details

**Text generation:**

- System prompt built in code: `buildSystemPrompt({ grade })`
- Strict JSON Schema enforced via OpenAI Responses API; auto-regenerates examples if counts insufficient
- **Curated artifacts required for activities:**
  - `meaningDistractors`: 3 distractors for `connect_def`
  - `synonyms`: ≥1 synonym + 3 distractors for `synonym_mcq`
  - `cueTokens`: ≥1 cue for `sentence_typed_gen`
  - `sentenceWordOptions`: 4 options per `correct_usage` example for `context_cloze` (includes target headword)
- **Content counts enforced:**
  - `correct_usage`: ≥2 examples
  - `incorrect_usage`: ≥6 examples
  - Total: ≥8 examples
- Sanitization: `sanitizeRawVocab` trims and normalizes arrays/fields

**Media generation:**

- **Image:** `gpt-image-1` (configurable via `MODEL_IMAGE`), uploaded to S3-like storage
- **TTS:** two pronunciations via `tts-1`-family (configurable via `MODEL_TTS`), voices: `alloy` and `verse`
- All artifacts saved with stable keys under `media/gen/{batchId}/{jobId}/...`

**Commit (server-side, transactional):**

- Validates `WordBundle`; rejects if headword already exists
- Inserts Word + senses/examples/variants/morphology/media
- **Curated artifacts:**
  - `word_meaning_distractor`: stores text (no inter-word dependency)
  - `word_synonym`: correct options for `synonym_mcq`
  - `word_synonym_distractor`: distractors for `synonym_mcq`
  - `word_cue_token`: sentence generation cues
  - `example_word_option`: 4 options per `correct_usage`, must include target headword
- **Optional lesson mapping:**
  - If batch scope includes `lessonId` or resolvable `(courseTitle, lessonTitle)`, maps word into lesson

**GenAI grading (runtime):**

- `sentence_typed_gen` and `paraphrase_typed_gen` use Responses API with strict JSON schema
- **Thresholds:** grammar ≥ 0.7, meaning ≥ 0.7
- **Slow flag:** ≥ 45s
- **Timeouts:** 5s for grading (configurable)

**Required environment (generation/worker):**

- `OPENAI_API_KEY`
- **S3-compatible storage:**
  - `BUCKET`, `ENDPOINT`, `ACCESS_KEY_ID`, `SECRET_ACCESS_KEY`
  - Optional: `BUCKET_PUBLIC_BASE_URL`, `S3_REGION`
- **Optional model overrides:** `MODEL_RESPONSES`, `MODEL_RESPONSES_SAFE`, `MODEL_IMAGE`, `MODEL_TTS`

**Retry policy:**

- Max attempts: 3 (text and media)
- Base delay: 20s with exponential backoff + 5s jitter
- Stale running jobs can be requeued via `/retry-failed`

---

## 10) Backend Config (single source of truth)

**Location:** `api/config/constants.ts` (re-exported via `api/config/index.ts` for legacy)

**Key settings:**

### Timing

- `SLOW_THRESHOLD_MS = 30000` (standard activities)
- `GENAI_SLOW_THRESHOLD_MS = 45000` (generative tasks)
- `MIN_FLASHCARD_TIME_S = 6`
- `EASY_THRESHOLD_MS = 6000` (Rating 4, currently disabled)

### FSRS

- `requestRetention = 0.9`
- **Stability buckets:** learning < 7d, reviewing < 14d, mastered ≥ 14d
- **Sanity guards:** max interval 365d, max elapsed 3 years, baseline stability 0.15, baseline difficulty 5.0

### Session defaults

- `DURATION_S = 600` (10 min)
- `MAX_WORDS_PER_SESSION = 20`
- `MAX_REVIEW_WORDS_PER_SESSION = 40`
- `DAILY_REVIEW_CAP = 40`
- `DEFAULT_NEW_WORDS = 3`
- `MIN_NEW_PER_SESSION = 2`
- `MAX_NEW_PER_SESSION = 6`
- `ACTIVITIES_PER_NEW_WORD = 3` (flashcard + meaning + spelling)
- `RESUME_WINDOW_MS = 24h`

### Activity time estimates (seconds)

- `flashcard: 15`, `connect_def: 10`, `context_cloze: 10`, `select_usage: 15`, `spell_typed: 15`, `synonym_mcq: 10`, `definition_typed: 20`, `sentence_typed_gen: 45`, `paraphrase_typed_gen: 45`

### Activity attempt limits

- **1 attempt:** `flashcard_usage`, `connect_def`, `context_cloze`, `select_usage`, `synonym_mcq`, `sentence_typed_gen`, `paraphrase_typed_gen`
- **2 attempts:** `spell_typed`, `definition_typed`

### Recycling

- `PER_WORD_CAP = 3`
- `MIN_GAP_K = 4`
- `RECYCLE_ORDER.NEW = 1000` (base for new word recycles)
- `RECYCLE_ORDER.REVIEW = 2000` (base for review recycles)

### Interaction

- `MAX_HINTS = 2`
- `MAX_RETRIES = 1`

### GenAI

- **Concurrency:** `TEXT = 80`, `MEDIA_IMAGE = 20`, `MEDIA_TTS = 20`
- **Retry:** max 3 attempts, base delay 20s, jitter 5s
- **Grading thresholds:** grammar ≥ 0.7, meaning ≥ 0.7
- **Grader model:** `gpt-4.1-mini` (timeout 5s)
- **Generator models:** `gpt-4.1` (text), `gpt-image-1` (image), `gpt-4o-mini-tts` (TTS)
- **TTS voices:** `alloy`, `verse`
- **Image size:** `1024x1024`
- **Storage prefix:** `media/gen`

### XP

- High (≥80%): 1 XP/min
- Medium (65-79%): 0.5 XP/min
- Low (<65%): 0 XP

### Rate Limit

- `WINDOW_MS = 60000` (1 min)
- `MAX_REQUESTS = 100`

### Content validation counts

- `CORRECT_USAGE = 2`, `INCORRECT_USAGE = 6`, `MIN_TOTAL = 8`
- `MEANING_DISTRACTORS = 3`, `SYNONYM_DISTRACTORS = 3`
- `SENTENCE_WORD_OPTIONS = 4`, `CUE_TOKENS_MIN = 1`

**Policy:** Activity plan matrix in `api/config/policy/activity-matrix.ts`

**Content contract:** Activity content requirements in `api/config/policy/content-contract.ts`

---

## 11) Admin APIs (high-level)

> See [API_ROUTES.md](../API_ROUTES.md) for detailed schemas, validation, and output shapes.

### Content Management (admin only)

**Words:**

- **POST** `/v1/content/word` — create word `{ headword, pos, definition, lang?, notes?, status? }`
- **PATCH**/**GET**/**DELETE** `/v1/content/word/:id` (delete blocked if word is in any lessons)
- **GET** `/v1/content/word` — search with `?query=&lessonId=&status=&limit=&offset=`
- **GET** `/v1/content/word/:id/enriched` — full word (senses, morphology, media, examples, relations, variants)

**Enrichment** (senses/media/examples/relations/variants):

- **Senses:** POST/PATCH/DELETE under `/v1/content/word/:id/sense`
- **Morphology:** POST/DELETE `/word/:id/morphology/:morphId`; GET `/morphemes` to browse reusable elements
- **Media:** POST/DELETE `/word/:id/media/:mediaId`
- **Examples:** POST/PATCH/DELETE `/word/:id/example/:exampleId`
- **Relations:** POST/DELETE `/word/:id/relation/:relationId`
- **Variants:** POST/PATCH/DELETE `/word/:id/variant/:variantId`

**Courses & Lessons:**

- **Courses:** POST/GET/PATCH/DELETE `/v1/content/course/:id`; reorder lessons; copy lesson
- **Lessons:** POST/GET/PATCH/DELETE `/v1/content/lesson/:id`; add/remove words; reorder words; search by title
- Read endpoints (GET course/lesson) return effective caps computed from lesson → course defaults

**Imports:**

- **POST** `/v1/content/import/minimal` — CSV structure import `{ data: [{ grade, course_title, lesson_number, lesson_title, headword }] }`
  - Missing headwords auto-created as `draft` words; use GenAI to generate content
- **POST** `/v1/content/import/full` — JSON complete import `{ courses: [{ ...lessons: [{ ...words: [enrichedWord] }] }] }`
  - Imports senses, examples, media, morphology, relations, variants

**Exports:**

- `/v1/content/export/courses-minimal.csv?courseId=` — curriculum structure
- `/v1/content/export/courses-full.json?courseId=` — complete enriched content
- Other CSV exports: words, senses, morphology, examples, media, relations, variants (see API_ROUTES.md)

### Roster Management (admin only)

**Students:**

- POST/GET/PATCH/DELETE `/v1/admin/students/:id`
- GET `/v1/admin/students` — list with per-course progress
- GET `/v1/admin/students/:id/progress` — detailed progress
- POST/DELETE `/v1/admin/students/:id/assign-course/:courseId`

**Classes:**

- POST/GET/PATCH/DELETE `/v1/admin/classes/:id`
- POST/DELETE `/v1/admin/classes/:id/enrollments` — bulk add/remove students
- POST/DELETE `/v1/admin/classes/:id/assign-course/:courseId` — assigns to all enrolled students

**Analytics Exports:**

- `/v1/admin/export/exposures.csv` — per-word exposure stats
- `/v1/admin/export/time-to-mastery.csv` — course mastery progress
- `/v1/admin/export/time.csv` — daily session time
- `/v1/admin/export/progress.csv` — daily progress aggregations
- `/v1/admin/export/lesson-progress.csv` — per-lesson mastery

**Ops:**

- **POST** `/v1/admin/ingest` — legacy content ingestion
- **POST** `/v1/admin/seed-progress` — seed SRS progress for demos

**Utilities:**

- **GET** `/v1/content/word/:id/activity-demo/:activityType` — preview params for a word (admin QC tool)
- **POST** `/v1/content/word/:id/hint` — preview spelling hints (admin QC tool)

---

## 12) Media & Static Files

- Static assets under `/static/*` served without auth (use for images/audio referenced by `media.url`)
- In production: built client assets under `/assets/*`; SPA index fallback for all other routes

---

## 13) Typical Workflow (client integration)

### Student learning loop

1. **Login:** `POST /v1/auth/mock-login { email }` → sets cookie
2. **Check for active session:** `GET /v1/session/active` (optional; or just call `/session/start`)
3. **Start session:** `POST /v1/session/start { courseId?, lessonId? }` → `{ sessionId, itemCount, ... }`
4. **Loop until no more items:**
   - `POST /v1/session/:id/next` → `{ itemId, activityType, word, params }`
   - Render UI based on `activityType` and `params`
   - User answers → `POST /v1/session/:id/attempt { itemId, answer, latencyMs, attemptId, ... }`
   - For `spell_typed`/`definition_typed`: optionally `POST /v1/session/:id/hint` before attempting
5. **Finalize:** `POST /v1/session/:id/finalize` → `{ xpAwarded, accuracy, summary }`
6. **Refresh dashboards:**
   - `GET /v1/me/progress/course/:id`
   - `GET /v1/me/progress/course/:id/daily-plan`

### Idempotent attempts

- Client generates `attemptId` (UUIDv4) before submitting
- If network retry happens, server returns cached result (same `attemptId` → same response)
- Prevents double-recording on flaky connections

### CORS setup

- Frontend must use `credentials: "include"` (fetch) or `withCredentials: true` (axios)
- Backend allows one origin via `CORS_ORIGIN` env
- Origin header must match for state-changing requests (POST/PATCH/DELETE)

---

## 14) FAQ / Common Gotchas

- **Why is `headword` hidden sometimes?** In `spell_typed`, `definition_typed`, and `paraphrase_typed_gen`, the server omits `headword` to avoid leaking the answer.
- **Why is a correct item recycled?** If it was slow or used a hint/retry, the system treats it as weak and recycles it.
- **Why can't I delete a word?** Remove it from **all lessons** first; deletion is blocked when referenced.
- **What if student disconnects mid-session?** Call `/session/start` again — backend returns the existing session if < 24h. They can continue where they left off.
- **Can student have multiple sessions?** No — only one active session per student. Starting a new session either returns the existing one (if fresh) or abandons it (if stale).
- **What happens to abandoned sessions?** Attempts are saved but never committed to SRS. Progress is discarded (finalize is atomic).

---

## 15) Environment & Boot

**Environment variables:**

- `PORT` or `API_PORT` (default 3000)
- `DATABASE_URL` (default `postgresql://postgres:postgres@localhost:5433/vocab_mvp`)
- `JWT_SECRET` (for auth cookie)
- `CORS_ORIGIN` (frontend origin, default `http://localhost:5173`)
- `NODE_ENV` (when `production`, cookie is `secure: true`)

**Health checks:** `/health` and `/v1/health`

**Dev notes:** Rate limiting/logging disabled in tests (`NODE_ENV=test`)

**Database views** (auto-created in migrations):

- `srs_state_with_durability` — adds computed durability column
- `srs_word_stats` — per-word exposure aggregations
- `course_mastery_progress` — time-to-mastery calculations
- `session_time_daily` — daily session time rollups
- `progress_daily` — daily progress aggregations
- `lesson_progress` — per-lesson mastery tracking

---

## See also

- **Route catalog:** [API_ROUTES.md](../API_ROUTES.md)
- **Executable tests:** [tests/](../tests/)
- **Activity handlers:** `api/services/activities/`
- **GenAI services:** `api/services/genai/`
- **Config source:** `api/config/constants.ts`
