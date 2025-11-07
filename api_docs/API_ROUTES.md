# Playcademy Vocabulary API — Route Reference

> **Scope.** This file is a **thin, accurate route catalog**: paths, auth, validation, inputs/queries, and output shapes.  
> For end‑to‑end flows, operational practices, middleware, and security deep dives, see **[docs/backend_readme.md](./docs/backend_readme.md)** and the executable specs in **[./tests](./tests)**.

_Last updated: 2025‑11‑02_

---

## Conventions

- **Base URL:** All paths below are relative to `/v1`.
- **Auth:** Cookie-based session (HttpOnly). The frontend must send requests with `credentials: 'include'` (CORS).
- **Roles:** `admin`, `student`. “Public read” means no auth required.
- **Validation:** All POST/PATCH routes accept JSON bodies and use **Zod** schemas named below.
- **Errors:** Standard JSON error envelope:
  ```json
  {
    "error": {
      "code": "ERROR_CODE",
      "message": "Human-readable message",
      "details": {}
    }
  }
  ```

Common codes: `UNAUTHORIZED` (401), `FORBIDDEN` (403), `NOT_FOUND` (404), `BAD_REQUEST` (400), `VALIDATION_ERROR` (400), `INTERNAL_ERROR` (500).

- **Pagination:** Unless stated otherwise, list endpoints support `limit` and `offset`.
- **IDs:** UUIDs unless noted.
- **SRS terms:**
  _stability_ ≈ recall half-life in days; _durability_ = current retention probability (0–1); _bucket_ ∈ {`new`,`learning`,`reviewing`,`mastered`}.

---

# Authentication (`/auth`)

### POST `/auth/mock-login`

- **Auth:** None
- **Validation:** `mockLoginSchema`
- **Input:** `{ email: string }`
- **Output:** `{ success: true, user: {...} }`
- **Notes:** Sets HttpOnly cookie.

### GET `/auth/me`

- **Auth:** Optional
- **Validation:** —
- **Output:** `{ user: {...} }` or 401

### POST `/auth/logout`

- **Auth:** Optional
- **Validation:** —
- **Output:** `{ success: true }`
- **Notes:** Clears auth cookie.

---

# Content — Words (`/content/word`)

### POST `/content/word`

- **Auth:** Admin only
- **Validation:** `createWordSchema`
- **Input:** `{ headword, pos, definition, ... }`
- **Output:** `{ word: {...} }`

### PATCH `/content/word/:id`

- **Auth:** Admin only
- **Validation:** `updateWordSchema`
- **Input:** Partial word fields
- **Output:** `{ word: {...} }`

### GET `/content/word/:id`

- **Auth:** Public read
- **Validation:** —
- **Output:** `{ word: {...} }`

### GET `/content/word`

- **Auth:** Public read
- **Validation:** `wordSearchSchema` (query)
- **Query:** `?query=...&lessonId=...&status=...&limit=...&offset=...`
- **Output:** `{ words: [...], pagination: {...} }`

### DELETE `/content/word/:id`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true, word: {...} }`
- **Notes:** Fails if the word is used in any lessons.

---

## Word enrichment

### GET `/content/word/:id/enriched`

- **Auth:** Admin only
- **Validation:** —
- **Output:** Word with senses, morphology, media, examples, relations, orthographic variants.

### POST `/content/word/:id/sense`

- **Auth:** Admin only
- **Validation:** `createWordSenseSchema`
- **Input:** `{ definition: string, orderNo?: number, isPrimary?: boolean }`
- **Output:** `{ senseId, wordId, definition, orderNo, isPrimary }`
- **Notes:** Max 1 primary sense per word (DB-enforced).

### PATCH `/content/word/:wordId/sense/:senseId`

- **Auth:** Admin only
- **Validation:** —
- **Input:** `{ definition?, isPrimary?, orderNo? }`
- **Output:** `{ success: true, sense: {...} }`

### DELETE `/content/word/:wordId/sense/:senseId`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true, sense: {...} }`

### POST `/content/word/:id/morphology`

- **Auth:** Admin only
- **Validation:** `createMorphologySchema`
- **Input:** `{ type: "prefix"|"root"|"suffix", value: string, gloss?: string, lang?: string, orderNo?: number }`
- **Output:** `{ success: true, morphElement: {...} }`
- **Notes:** Reuses existing morph element if matching on `(type,value,lang)`.

### DELETE `/content/word/:wordId/morphology/:morphId`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true }`

### POST `/content/word/:id/media`

- **Auth:** Admin only
- **Validation:** `createMediaSchema`
- **Input (required):** `{ kind: "audio"|"image", url: string, role: "word_pronunciation"|"alt_pronunciation"|"sentence_audio"|"illustration" }`
  **Optional metadata:** `mimeType, width, height, durationMs, lang, accent, speaker, altText, license, attribution, sourceUrl, hash, storageKey, senseId, orderNo`
- **Output:** `{ success: true, media: {...} }`

### DELETE `/content/word/:wordId/media/:mediaId`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true }`

### POST `/content/word/:id/example`

- **Auth:** Admin only
- **Validation:** `createExampleSchema`
- **Input:** `{ kind: "correct_usage"|"incorrect_usage", text: string, senseId?, orderNo?, status?, source?, notes? }`
- **Output:** `{ success: true, example: {...} }`

### PATCH `/content/word/:wordId/example/:exampleId`

- **Auth:** Admin only
- **Validation:** —
- **Input:** `{ text?, kind?, status?, orderNo? }`
- **Output:** `{ success: true, example: {...} }`

### DELETE `/content/word/:wordId/example/:exampleId`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true, example: {...} }`

### POST `/content/word/:id/relation`

- **Auth:** Admin only
- **Validation:** `createLexicalRelationSchema`
- **Input:** `{ relationType: "synonym"|"antonym"|"related"|"derivation"|"homophone"|"confusable", toWordId: string, fromSenseId?: string, toSenseId?: string }`
- **Output:** `{ success: true, relation: {...} }`
- **Notes:** Symmetric relations enforce ordered ids to prevent duplicates.

### DELETE `/content/word/:wordId/relation/:relationId`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true, relation: {...} }`

### POST `/content/word/:id/variant`

- **Auth:** Admin only
- **Validation:** `createOrthVariantSchema`
- **Input:** `{ form: string, region?: string, isPreferred?: boolean }`
- **Output:** `{ success: true, variant: {...} }`

### PATCH `/content/word/:wordId/variant/:variantId`

- **Auth:** Admin only
- **Validation:** —
- **Input:** `{ form?, region?, isPreferred? }`
- **Output:** `{ success: true, variant: {...} }`

### DELETE `/content/word/:wordId/variant/:variantId`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true, variant: {...} }`

### GET `/content/word/:id/activity-demo/:activityType`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ word: {...}, params: object|null }`
- **Notes:** Utility endpoint to preview per-activity params for a given word.

### POST `/content/word/:id/hint`

- **Auth:** Admin only
- **Validation:** —
- **Input:** `{ hintsUsed: number }`
- **Output:** `{ hint: { type, text }, hintsUsed }`

### GET `/content/morphemes`

- **Auth:** Admin only
- **Validation:** —
- **Query:** `?query=...&type=prefix|root|suffix`
- **Output:** `{ morphemes: [...] }`

---

# Curriculum — Courses & Lessons (`/content`)

## Courses (`/content/course`)

### POST `/content/course`

- **Auth:** Admin only
- **Validation:** `createCourseSchema`
- **Input:** `{ grade, title, status?, defaultNewWordsPerSession?, maxWordsPerSession?, maxReviewWordsPerSession?, sessionTimeBudgetS? }`
- **Output:** `{ course: {...} }`
- **Notes:** `grade` unique.

### GET `/content/course`

- **Auth:** Public read
- **Validation:** —
- **Output:** `{ courses: [...] }`

### GET `/content/course/:id`

- **Auth:** Public read
- **Validation:** —
- **Output:** `{ course: {..., lessons: [...] } }`

### PATCH `/content/course/:id`

- **Auth:** Admin only
- **Validation:** `updateCourseSchema`
- **Input:** Partial fields; `updateAllLessons?: boolean` to cascade defaults to all lessons.
- **Output:** `{ course: {...} }`

### DELETE `/content/course/:id`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true, course: {...} }`
- **Notes:** Cascades to lessons and mappings.

## Lessons (`/content/lesson`)

### POST `/content/lesson`

- **Auth:** Admin only
- **Validation:** `createLessonSchema`
- **Input:** `{ courseId, title, orderNo?, targetNewPerSession?, maxWordsPerSession?, maxReviewWordsPerSession? }`
- **Output:** `{ lesson: {...} }`

### GET `/content/lesson` (search)

- **Auth:** Admin only
- **Validation:** `lessonSearchSchema` (query)
- **Query:** `?query=...&courseId=...&limit=...&offset=...`
- **Output:** `{ lessons: [...], pagination: {...} }`
- **Notes:** Trigram fuzzy match on title.

### GET `/content/lesson/:id`

- **Auth:** Public read
- **Validation:** —
- **Output:** `{ lesson: {..., words: [...] } }`

### PATCH `/content/lesson/:id`

- **Auth:** Admin only
- **Validation:** `updateLessonSchema`
- **Input:** Partial lesson fields
- **Output:** `{ lesson: {...} }`

### DELETE `/content/lesson/:id`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true, lesson: {...} }`

## Lesson–Word mapping

### POST `/content/lesson/:id/words`

- **Auth:** Admin only
- **Validation:** `addWordToLessonSchema`
- **Input:** `{ wordId, orderNo? }`
- **Output:** `{ success: true, mapping: { lessonId, wordId, orderNo } }`

### DELETE `/content/lesson/:id/words/:wordId`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true }`

### PATCH `/content/lesson/:id/words/reorder`

- **Auth:** Admin only
- **Validation:** `reorderWordsSchema`
- **Input:** `{ wordIds: uuid[] }`
- **Output:** `{ success: true, count }`

### PATCH `/content/course/:id/lessons/reorder`

- **Auth:** Admin only
- **Validation:** `reorderLessonsSchema`
- **Input:** `{ lessonIds: uuid[] }`
- **Output:** `{ success: true, count }`

---

# CSV Exports (`/content/export`)

_All Admin only._

- **GET** `/content/export/courses-minimal.csv` (`?courseId=...`) → CSV format matching minimal import: `grade, course_title, lesson_number, lesson_title, headword`
- **GET** `/content/export/courses-full.json` (`?courseId=...`) → JSON format with complete course/lesson/word structure including all enriched content (senses, examples, media, morphology, relations, variants)

_The following exports are available but not exposed in the admin UI:_

- **GET** `/content/export/courses.csv` → columns: `courseId, grade, title, status, defaultNewWordsPerSession, maxWordsPerSession, maxReviewWordsPerSession, sessionTimeBudgetS, lessonCount, createdTs`
- **GET** `/content/export/lessons.csv` (`?courseId=...`) → `lessonId, courseId, courseTitle, courseGrade, title, orderNo, targetNewPerSession, maxWordsPerSession, maxReviewWordsPerSession, wordCount`
- **GET** `/content/export/lesson-words.csv` (`?courseId=...&lessonId=...`) → `lessonId, lessonTitle, wordId, headword, pos, definition, orderNo`
- **GET** `/content/export/words.csv` → `wordId, headword, lang, pos, definition, status, notes, createdTs, updatedTs`
- **GET** `/content/export/word-senses.csv` → `senseId, wordId, headword, definition, orderNo, isPrimary`
- **GET** `/content/export/morphology.csv` → `wordId, headword, morphId, type, value, gloss, lang, orderNo`
- **GET** `/content/export/examples.csv` → `exampleId, wordId, headword, senseId, kind, text, orderNo, status, source, notes`
<!-- Cloze options export removed (table dropped) -->
- **GET** `/content/export/media.csv` → `mediaId, kind, url, mimeType, width, height, durationMs, lang, accent, speaker, altText, license, attribution, sourceUrl, hash, storageKey, createdTs`
- **GET** `/content/export/relations.csv` → `relationId, relationType, fromWordId, toWordId, fromSenseId, toSenseId`
- **GET** `/content/export/orth-variants.csv` → `variantId, wordId, headword, form, region, isPreferred`

---

# Sessions (`/session`)

### GET `/session/active`

- **Auth:** Student only
- **Validation:** —
- **Output:** `{ hasActiveSession: boolean, sessionId?: string }`
- **Notes:** Returns `hasActiveSession: false` if any active session is past the resume window.

### POST `/session/start`

- **Auth:** Student only
- **Validation:** `startSessionSchema`
- **Input:** `{ courseId?, lessonId?, timeBudgetS? }`
- **Output:** `{ sessionId, itemCount, newWordActivityCount, plannedDurationS, resuming, completedItems? }`

### POST `/session/:id/next`

- **Auth:** Student only
- **Validation:** —
- **Output:** `{ itemId, activityType, phase?, phaseProgress?, word: {...}, params?: {...} }`
- **Security:** **Headword is hidden** for typed activities (`SPELL_TYPED`, `DEFINITION_TYPED`, `PARAPHRASE_TYPED_GEN`); visible for others.

### POST `/session/:id/attempt`

- **Auth:** Student only
- **Validation:** `submitAttemptSchema`
- **Input:** `{ itemId, answer, latencyMs, hintsUsed, retriesUsed, timeSpentS?, attemptId }`
- **Output:** `{ attemptId, correct, score, feedback?, recycled, recycleItemId?, cached }`
- **Notes:** Client supplies **`attemptId`** (UUID v4) for idempotency; server is authoritative for scoring. Flashcards require `timeSpentS` ≥ 10s.

### POST `/session/:id/hint`

- **Auth:** Student only
- **Validation:** `requestHintSchema`
- **Input:** `{ itemId, currentHints }`
- **Output:** `{ hint: { type, text }, hintsUsed, maxHints }`
- **Notes:** Hints exist for spelling and definition‑typed activities; using hints marks the item weak (eligible for recycle).

### POST `/session/:id/finalize`

- **Auth:** Student only
- **Validation:** —
- **Output:** `{ sessionId, itemsAnswered, accuracy, xpAwarded, summary }`

---

# Progress (`/me`)

### GET `/me/progress/course/:id`

- **Auth:** Required
- **Validation:** —
- **Output:** `{ progress: { new, learning, reviewing, mastered, updatedAt } }`

### GET `/me/progress/course/:id/daily-plan`

- **Auth:** Required
- **Validation:** —
- **Output:** `{ plan: { newWords, reviewWords, estimatedMinutes, maxWordsPerSession } }`
- **Notes:** `totalCap` is deprecated in favor of `maxWordsPerSession`.

### GET `/me/progress/course/:id/lessons`

- **Auth:** Required
- **Validation:** —
- **Output:** `{ lessons: [{ lessonId, lessonTitle, orderNo, totalWords, introducedWords }, ...] }`

### **GET** `/me/progress/course/:id/words`

- **Auth:** Required
- **Validation:** Query validated
- **Query params (updated):**
  - `lessonId` — filter by lesson
  - `bucket` — one of `new|learning|reviewing|mastered`
  - `status` — `due|upcoming` (SRS due state)
  - `stabilityMin` — number (days)
  - `stabilityMax` — number (days)
  - `includeStats` — `true|false` (per-word performance; slower)
  - `limit`, `offset`

- **Output:**

```json
{
  "words": [
    {
      "wordId": "uuid",
      "headword": "conspire",
      "pos": "verb",
      "stability": 24.1,
      "durability": 0.86,
      "nextDue": "2025-10-12T00:00:00Z",
      "bucket": "reviewing",
      "lessonId": "lesson-uuid",
      "lessonTitle": "Lesson 1",
      "stats": {
        "totalAttempts": 8,
        "successRate": 0.75,
        "avgLatencyMs": 1234,
        "totalHintsUsed": 2,
        "timesReviewed": 5,
        "lastAttemptDate": "2025-10-10T14:30:00Z"
      }
    }
  ],
  "total": 42
}
```

- **Notes:** `includeStats=true` performs additional lookups (slower).

### GET `/me/xp/ledger`

- **Auth:** Required
- **Validation:** —
- **Output:** `{ entries: [...], totalXp }`

---

# Admin — Roster & Classes (`/admin`)

## Students

### POST `/admin/students`

- **Auth:** Admin only
- **Validation:** `createUserSchema`
- **Input:** `{ email, displayName, orgId, role?, timezone?, reviewRolloverHour? }`
- **Output:** `{ student: {...} }`

### GET `/admin/students`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ students: [{ userId, email, displayName, courses: [{ courseId, courseTitle, grade, progress: {...} }] }] }`

### GET `/admin/students/:id/progress`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ student: {...}, courses: [{ courseId, courseTitle, grade, progress: {...} }] }`

### PATCH `/admin/students/:id`

- **Auth:** Admin only
- **Validation:** `updateUserSchema`
- **Input:** `{ email?, displayName?, timezone?, reviewRolloverHour? }`
- **Output:** `{ student: {...} }`

### DELETE `/admin/students/:id`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true, student: {...} }`

### POST `/admin/students/:id/assign-course`

- **Auth:** Admin only
- **Validation:** `assignCourseSchema`
- **Input:** `{ courseId }`
- **Output:** `{ success: true, userId, courseId }`

### DELETE `/admin/students/:id/assign-course/:courseId`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true, userId, courseId }`

## Classes

### POST `/admin/classes`

- **Auth:** Admin only
- **Validation:** `createClassSchema`
- **Input:** `{ orgId, name, schoolYear }`
- **Output:** `{ class: {...} }`

### GET `/admin/classes` (`?orgId=...`)

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ classes: [...] }`

### GET `/admin/classes/:id`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ class: {..., students: [{ userId, email, displayName, role }] } }`

### PATCH `/admin/classes/:id`

- **Auth:** Admin only
- **Validation:** `updateClassSchema`
- **Input:** `{ name?, schoolYear? }`
- **Output:** `{ class: {...} }`

### DELETE `/admin/classes/:id`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true, class: {...} }`

### POST `/admin/classes/:id/enrollments`

- **Auth:** Admin only
- **Validation:** `addStudentsToClassSchema`
- **Input:** `{ userIds: uuid[] }`
- **Output:** `{ success: true, classId, userIds }`

### DELETE `/admin/classes/:id/enrollments`

- **Auth:** Admin only
- **Validation:** `addStudentsToClassSchema`
- **Input:** `{ userIds: uuid[] }`
- **Output:** `{ success: true, classId, removedCount }`

### POST `/admin/classes/:id/assign-course`

- **Auth:** Admin only
- **Validation:** `assignCourseSchema`
- **Input:** `{ courseId }`
- **Output:** `{ success: true, classId, courseId, studentsAssigned }`

### DELETE `/admin/classes/:id/assign-course/:courseId`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: true, classId, courseId }`

---

# Admin — Analytics Exports (`/admin/export`)

_All Admin only._

- **GET** `/admin/export/exposures.csv` (`?userId=...&courseId=...&wordId=...`) → `userId, wordId, courseId, firstExposureTs, lastExposureTs, totalExposures, correctCount, incorrectCount, slowCount, totalReps, totalLapses`
- **GET** `/admin/export/time-to-mastery.csv` (`?userId=...&courseId=...`) → `userId, courseId, totalWords, masteredWords, firstWordMasteredTs, lastWordMasteredTs, timeToMasterDays, masteryPercent`
- **GET** `/admin/export/time.csv` (`?userId=...&courseId=...&from=...&to=...`) → `userId, courseId, date, sessionCount, totalSeconds, totalMinutes, avgSessionSeconds`
- **GET** `/admin/export/progress.csv` (`?userId=...&courseId=...&classId=...`) → `userId, email, displayName, courseId, courseTitle, grade, date, sessionCount, totalSeconds, totalMinutes, itemsAnswered, avgAccuracy, newCnt, learningCnt, reviewingCnt, masteredCnt, dueCnt`
- **GET** `/admin/export/lesson-progress.csv` (`?userId=...&courseId=...&classId=...`) → `userId, email, displayName, courseId, courseTitle, grade, lessonId, lessonTitle, orderNo, totalWords, introducedWords, learningWords, reviewingWords, masteredWords, masteryPercent`

---

# Admin — Ops (`/admin`)

### POST `/admin/ingest`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: boolean, message?: string, error?: string }`
- **Notes:** Runs one-time content ingestion from legacy source.

### POST `/admin/seed-progress`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ success: boolean, message?: string, error?: string }`
- **Notes:** Seeds SRS progress data for testing/demo environments.

---

# Admin — Generative AI (`/admin/genai`)

_Admin only. Simplified sprint workflow for generation jobs._

### POST `/admin/genai/sprint`

- **Auth:** Admin only
- **Validation:** `{ words: string[], grade: number, courseTitle?, lessonTitle? }`
- **Output:** `{ batchId, jobsCreated, jobIds }`

### GET `/admin/genai/sprint/:batchId/status`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ batchId, total, completed, inProgress, counts, done }`

### GET `/admin/genai/sprint/:batchId/results`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ batchId, total, succeeded: [{ jobId, headword, bundle }], failed: [{ jobId, headword, error }] }`

### GET `/admin/genai/sprint/:batchId/failures`

- **Auth:** Admin only
- **Validation:** —
- **Output:** `{ batchId, count, failures: [{ jobId, headword, attempts, stage, error, updatedAt }] }`

### POST `/admin/genai/retry-failed`

- **Auth:** Admin only
- **Validation:** `{ batchId? }`
- **Output:** `{ retriedCount, jobIds }`

---

# Admin — Content Import

### POST `/content/import/minimal`

- **Auth:** Admin only
- **Validation:** CSV rows validated
- **Input:** `{ data: [{ grade, course_title, lesson_number, lesson_title, headword }] }`
- **Output:** `{ success: true, summary }`
- **Notes:** Rows with extra columns are rejected. Missing headwords are auto-created as `draft` words linked to lessons.

### POST `/content/import/full`

- **Auth:** Admin only
- **Validation:** `jsonFullImportSchema`
- **Input:** Full content JSON `{ courses: [{ ...lessons: [{ ...words: [enrichedWord] }]}] }`
- **Output:** `{ success: true, summary }`
- **Notes:** New words created with `status: draft`. Imports senses, examples, media, morphology, relations, variants.

---

# Configuration (`/config`)

### GET `/config`

- **Auth:** Public read
- **Validation:** —
- **Output:**
  ```json
  {
    "interaction": {
      "maxHints": number,
      "maxRetries": number,
      "minFlashcardTimeS": number
    },
    "session": {
      "defaultDurationS": number,
      "maxWordsPerSession": number,
      "maxReviewWordsPerSession": number
    },
    "fsrs": {
      "slowThresholdMs": number,
      "recycleMinGapK": number,
      "recyclePerWordCap": number,
      "stabilityBuckets": number[]
    }
  }
  ```

---

# Health

- **GET** `/` → 200
- **GET** `/health` → 200
- **GET** `/v1/health` → 200

---

## Security & Activity Rules (stable, non-brittle)

- **Never reveal canonical spellings before answer** (spelling activities hide `headword` and never leak via hints/errors).
- **Hints are capped** and **never disclose the full word**; morphology/first‑letter/cloze style hints only.
- **Server‑side scoring**; client cannot override correctness.
- **Role enforcement** on all write endpoints; public read only where specified.

---

## See also

- **Flows, diagrams, and middleware:** [docs/backend_readme.md](./docs/backend_readme.md)
- **Executable examples & edge cases:** [./tests](./tests)
- **Schemas:** `api/data/**/schemas.ts` and route handlers in `api/routes/**`
