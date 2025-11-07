# Playcademy Vocabulary API — Detailed Specification

> **Comprehensive API specification** with request/response examples, error handling, validation rules, and implementation details.
> For a quick reference catalog, see **[API_ROUTES.md](../API_ROUTES.md)**.
> For backend architecture and flows, see **[backend_readme.md](./backend_readme.md)**.

_Version: 1.0.0_
_Last updated: 2025-11-06_

---

## Table of Contents

1. [Overview](#overview)
2. [Authentication & Authorization](#authentication--authorization)
3. [Request/Response Format](#requestresponse-format)
4. [Error Handling](#error-handling)
5. [API Endpoints](#api-endpoints)
   - [Authentication](#authentication-endpoints)
   - [Content Management](#content-management)
   - [Session Management](#session-management)
   - [Progress & Analytics](#progress--analytics)
   - [Admin Operations](#admin-operations)
   - [GenAI Pipeline](#genai-pipeline)
6. [Data Models](#data-models)
7. [Rate Limiting & Performance](#rate-limiting--performance)
8. [Testing & Development](#testing--development)

---

## Overview

The Playcademy Vocabulary API is a RESTful API built on Cloudflare Workers using the Playcademy platform. It provides:

- **Content Management**: CRUD operations for words, courses, lessons, and enriched vocabulary data
- **Session Management**: Spaced repetition practice sessions with FSRS v4 scheduling
- **Progress Tracking**: Real-time student progress, analytics, and XP system
- **GenAI Pipeline**: Automated content generation using OpenAI (GPT-4, TTS, DALL-E)
- **Admin Tools**: Student management, class rosters, bulk operations

### Technology Stack

- **Runtime**: Cloudflare Workers (Miniflare for local dev)
- **Database**: SQLite (D1) with Drizzle ORM
- **Storage**: Cloudflare R2 for media files
- **Auth**: Better Auth (email/password, session cookies)
- **Validation**: Zod schemas for runtime type safety

### Base URL

- **Local Development**: `http://localhost:8000/api`
- **Production**: `https://your-domain.com/api`

All endpoints below are relative to `/api`.

---

## Authentication & Authorization

### Authentication Method

The API uses **session-based authentication** via Better Auth with HttpOnly cookies.

**Login Flow:**
1. Client sends credentials to `/auth/sign-in/email`
2. Server validates and sets HttpOnly session cookie
3. Client includes cookie in subsequent requests with `credentials: 'include'`

**Cookie Details:**
- Name: `better-auth.session_token`
- HttpOnly: `true`
- Secure: `true` (production)
- SameSite: `Lax`
- Max-Age: 30 days

### Roles

- **`student`**: Can access own progress, sessions, and public content
- **`admin`**: Full access to all endpoints including content management and user administration

### Authorization Patterns

```typescript
// Public endpoints (no auth required)
GET /api/content/course
GET /api/content/lesson/:id
GET /api/config

// Authenticated endpoints (any logged-in user)
GET /api/me/progress/course/:id
GET /api/me/xp/ledger

// Student-only endpoints
POST /api/session/start
POST /api/session/:id/next

// Admin-only endpoints
POST /api/content/word
DELETE /api/content/course/:id
POST /api/admin/genai/sprint
```

---

## Request/Response Format

### Content Types

- **Request Body**: `application/json` (unless otherwise specified)
- **Response Body**: `application/json`
- **CSV Exports**: `text/csv`

### Common Headers

**Request:**
```http
Content-Type: application/json
Cookie: better-auth.session_token=...
```

**Response:**
```http
Content-Type: application/json
Cache-Control: no-store
```

### Pagination

List endpoints support query parameters:

```typescript
{
  limit: number;   // Max items per page (default: 50, max: 100)
  offset: number;  // Skip N items (default: 0)
}
```

**Example:**
```http
GET /api/content/word?limit=20&offset=40
```

**Response includes pagination metadata:**
```json
{
  "words": [...],
  "pagination": {
    "limit": 20,
    "offset": 40,
    "total": 156
  }
}
```

---

## Error Handling

### Error Response Format

All errors return a consistent JSON structure:

```json
{
  "error": {
    "code": "ERROR_CODE",
    "message": "Human-readable error message",
    "details": {}
  }
}
```

### HTTP Status Codes

| Status | Code | Description |
|--------|------|-------------|
| 400 | `BAD_REQUEST` | Invalid request format or parameters |
| 400 | `VALIDATION_ERROR` | Zod validation failed (includes field details) |
| 401 | `UNAUTHORIZED` | Missing or invalid authentication |
| 403 | `FORBIDDEN` | Insufficient permissions |
| 404 | `NOT_FOUND` | Resource not found |
| 409 | `CONFLICT` | Resource conflict (e.g., duplicate headword) |
| 429 | `RATE_LIMIT_EXCEEDED` | Too many requests |
| 500 | `INTERNAL_ERROR` | Server error |

### Validation Errors

Validation errors include detailed field-level information:

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Invalid input",
    "details": [
      {
        "path": ["headword"],
        "message": "Headword is required",
        "code": "too_small"
      },
      {
        "path": ["pos"],
        "message": "Invalid enum value. Expected 'noun' | 'verb' | 'adjective' | 'adverb'",
        "code": "invalid_enum_value"
      }
    ]
  }
}
```

---

## API Endpoints

### Authentication Endpoints

#### Better Auth Integration

All Better Auth endpoints are available via:

```http
POST /api/auth/[...all]
```

**Key endpoints:**

##### Sign In with Email

```http
POST /api/auth/sign-in/email
```

**Request:**
```json
{
  "email": "student@demo.playcademy.com",
  "password": "password"
}
```

**Response (200):**
```json
{
  "user": {
    "id": "usr_123abc",
    "email": "student@demo.playcademy.com",
    "name": "Jane Student",
    "role": "student",
    "orgId": "org_456def"
  },
  "session": {
    "token": "ses_789ghi",
    "expiresAt": "2025-12-06T12:00:00Z"
  }
}
```

**Sets Cookie:**
```http
Set-Cookie: better-auth.session_token=ses_789ghi; HttpOnly; Secure; SameSite=Lax; Max-Age=2592000
```

##### Get Current Session

```http
GET /api/auth/session
```

**Response (200):**
```json
{
  "user": {
    "id": "usr_123abc",
    "email": "student@demo.playcademy.com",
    "name": "Jane Student",
    "role": "student"
  },
  "session": {
    "token": "ses_789ghi",
    "expiresAt": "2025-12-06T12:00:00Z"
  }
}
```

**Response (401):**
```json
{
  "error": {
    "code": "UNAUTHORIZED",
    "message": "No active session"
  }
}
```

##### Sign Out

```http
POST /api/auth/sign-out
```

**Response (200):**
```json
{
  "success": true
}
```

**Clears Cookie:**
```http
Set-Cookie: better-auth.session_token=; Max-Age=0
```

---

### Content Management

#### Words

##### Create Word

```http
POST /api/content/word
```

**Auth:** Admin only

**Request Body:**
```json
{
  "headword": "conspire",
  "lang": "en",
  "pos": "verb",
  "definition": "To secretly plan with others to do something harmful or illegal",
  "notes": "From Latin con- (together) + spirare (breathe)",
  "status": "live"
}
```

**Validation Schema:**
```typescript
{
  headword: string (min 1);
  lang: string (default "en");
  pos: "noun" | "verb" | "adjective" | "adverb" | "pronoun" | "preposition" | "conjunction" | "interjection";
  definition: string (min 1);
  notes?: string;
  status?: "draft" | "live" | "archived" (default "live");
}
```

**Response (200):**
```json
{
  "word": {
    "wordId": "wrd_abc123",
    "headword": "conspire",
    "lang": "en",
    "pos": "verb",
    "definition": "To secretly plan with others to do something harmful or illegal",
    "notes": "From Latin con- (together) + spirare (breathe)",
    "status": "live",
    "createdTs": "2025-11-06T10:00:00Z",
    "updatedTs": "2025-11-06T10:00:00Z"
  }
}
```

**Error (409):**
```json
{
  "error": {
    "code": "CONFLICT",
    "message": "Word with headword 'conspire' already exists"
  }
}
```

##### Get Word by ID

```http
GET /api/content/word/:id
```

**Auth:** Public read

**Response (200):**
```json
{
  "word": {
    "wordId": "wrd_abc123",
    "headword": "conspire",
    "lang": "en",
    "pos": "verb",
    "definition": "To secretly plan with others to do something harmful or illegal",
    "status": "live",
    "createdTs": "2025-11-06T10:00:00Z",
    "updatedTs": "2025-11-06T10:00:00Z"
  }
}
```

##### Search Words

```http
GET /api/content/word?query=con&status=live&limit=10&offset=0
```

**Auth:** Public read

**Query Parameters:**
```typescript
{
  query?: string;        // Trigram fuzzy search on headword
  lessonId?: string;     // Filter by lesson (UUID)
  status?: "draft" | "live" | "archived";
  limit?: number;        // Default 50, max 100
  offset?: number;       // Default 0
}
```

**Response (200):**
```json
{
  "words": [
    {
      "wordId": "wrd_abc123",
      "headword": "conspire",
      "pos": "verb",
      "definition": "To secretly plan with others...",
      "status": "live"
    },
    {
      "wordId": "wrd_def456",
      "headword": "consume",
      "pos": "verb",
      "definition": "To eat, drink, or ingest...",
      "status": "live"
    }
  ],
  "pagination": {
    "limit": 10,
    "offset": 0,
    "total": 24
  }
}
```

##### Update Word

```http
PATCH /api/content/word/:id
```

**Auth:** Admin only

**Request Body (all fields optional):**
```json
{
  "headword": "conspire",
  "definition": "Updated definition...",
  "status": "live"
}
```

**Response (200):**
```json
{
  "word": {
    "wordId": "wrd_abc123",
    "headword": "conspire",
    "definition": "Updated definition...",
    "status": "live",
    "updatedTs": "2025-11-06T11:00:00Z"
  }
}
```

##### Delete Word

```http
DELETE /api/content/word/:id
```

**Auth:** Admin only

**Response (200):**
```json
{
  "success": true,
  "word": {
    "wordId": "wrd_abc123",
    "headword": "conspire"
  }
}
```

**Error (409):**
```json
{
  "error": {
    "code": "CONFLICT",
    "message": "Cannot delete word: used in 3 lessons"
  }
}
```

##### Get Enriched Word

```http
GET /api/content/word/:id/enriched
```

**Auth:** Admin only

**Response (200):**
```json
{
  "word": {
    "wordId": "wrd_abc123",
    "headword": "conspire",
    "pos": "verb",
    "definition": "To secretly plan with others to do something harmful or illegal",
    "senses": [
      {
        "senseId": "sns_001",
        "definition": "To secretly plan with others to do something harmful or illegal",
        "orderNo": 1,
        "isPrimary": true
      },
      {
        "senseId": "sns_002",
        "definition": "To act together toward a common goal",
        "orderNo": 2,
        "isPrimary": false
      }
    ],
    "morphology": [
      {
        "morphId": "mrp_001",
        "type": "prefix",
        "value": "con-",
        "gloss": "together, with",
        "lang": "lat",
        "orderNo": 1
      },
      {
        "morphId": "mrp_002",
        "type": "root",
        "value": "spir",
        "gloss": "breathe",
        "lang": "lat",
        "orderNo": 2
      }
    ],
    "media": [
      {
        "mediaId": "med_001",
        "kind": "audio",
        "url": "https://r2.example.com/audio/conspire-us.mp3",
        "role": "word_pronunciation",
        "mimeType": "audio/mpeg",
        "durationMs": 800,
        "lang": "en",
        "accent": "US",
        "speaker": "alloy",
        "license": "proprietary"
      },
      {
        "mediaId": "med_002",
        "kind": "image",
        "url": "https://r2.example.com/images/conspire.png",
        "role": "illustration",
        "mimeType": "image/png",
        "width": 512,
        "height": 512,
        "altText": "Two figures whispering in shadows",
        "license": "proprietary"
      }
    ],
    "examples": [
      {
        "exampleId": "exm_001",
        "kind": "correct_usage",
        "text": "The rivals conspired to overthrow the government.",
        "senseId": "sns_001",
        "orderNo": 1,
        "status": "live"
      }
    ],
    "relations": [
      {
        "relationId": "rel_001",
        "relationType": "synonym",
        "toWordId": "wrd_xyz789",
        "toWord": {
          "wordId": "wrd_xyz789",
          "headword": "plot",
          "pos": "verb"
        }
      }
    ],
    "variants": [
      {
        "variantId": "var_001",
        "form": "conspire",
        "region": "US",
        "isPreferred": true
      }
    ]
  }
}
```

#### Word Enrichment

##### Add Word Sense

```http
POST /api/content/word/:id/sense
```

**Auth:** Admin only

**Request Body:**
```json
{
  "definition": "To act together toward a common goal",
  "orderNo": 2,
  "isPrimary": false
}
```

**Response (200):**
```json
{
  "senseId": "sns_002",
  "wordId": "wrd_abc123",
  "definition": "To act together toward a common goal",
  "orderNo": 2,
  "isPrimary": false
}
```

##### Add Morphology

```http
POST /api/content/word/:id/morphology
```

**Auth:** Admin only

**Request Body:**
```json
{
  "type": "prefix",
  "value": "con-",
  "gloss": "together, with",
  "lang": "lat",
  "orderNo": 1
}
```

**Response (200):**
```json
{
  "success": true,
  "morphElement": {
    "morphId": "mrp_001",
    "type": "prefix",
    "value": "con-",
    "gloss": "together, with",
    "lang": "lat"
  }
}
```

##### Add Media

```http
POST /api/content/word/:id/media
```

**Auth:** Admin only

**Request Body:**
```json
{
  "kind": "audio",
  "url": "https://r2.example.com/audio/conspire-us.mp3",
  "role": "word_pronunciation",
  "mimeType": "audio/mpeg",
  "durationMs": 800,
  "lang": "en",
  "accent": "US",
  "speaker": "alloy",
  "license": "proprietary",
  "storageKey": "audio/conspire-us.mp3"
}
```

**Validation Schema:**
```typescript
{
  kind: "audio" | "image";
  url: string (URL);
  role: "word_pronunciation" | "alt_pronunciation" | "sentence_audio" | "illustration";
  mimeType?: string;
  width?: number;        // Images only
  height?: number;       // Images only
  durationMs?: number;   // Audio only
  lang?: string;
  accent?: string;       // Audio only (US, UK, AU, CA, etc.)
  speaker?: string;      // Audio only
  altText?: string;      // Images only
  license?: "cc0" | "cc_by" | "cc_by_sa" | "proprietary" | "unknown";
  attribution?: string;
  sourceUrl?: string (URL);
  hash?: string;
  storageKey?: string;
  senseId?: string (UUID);
  orderNo?: number;
}
```

**Response (200):**
```json
{
  "success": true,
  "media": {
    "mediaId": "med_001",
    "kind": "audio",
    "url": "https://r2.example.com/audio/conspire-us.mp3",
    "role": "word_pronunciation",
    "durationMs": 800
  }
}
```

##### Add Example

```http
POST /api/content/word/:id/example
```

**Auth:** Admin only

**Request Body:**
```json
{
  "kind": "correct_usage",
  "text": "The rivals conspired to overthrow the government.",
  "senseId": "sns_001",
  "orderNo": 1,
  "status": "live",
  "source": "OpenAI GPT-4"
}
```

**Response (200):**
```json
{
  "success": true,
  "example": {
    "exampleId": "exm_001",
    "kind": "correct_usage",
    "text": "The rivals conspired to overthrow the government.",
    "senseId": "sns_001",
    "orderNo": 1,
    "status": "live"
  }
}
```

##### Add Lexical Relation

```http
POST /api/content/word/:id/relation
```

**Auth:** Admin only

**Request Body:**
```json
{
  "relationType": "synonym",
  "toWordId": "wrd_xyz789",
  "fromSenseId": "sns_001",
  "toSenseId": "sns_003"
}
```

**Validation Schema:**
```typescript
{
  relationType: "synonym" | "antonym" | "related" | "derivation" | "homophone" | "confusable";
  toWordId: string (UUID);
  fromSenseId?: string (UUID);
  toSenseId?: string (UUID);
}
```

**Response (200):**
```json
{
  "success": true,
  "relation": {
    "relationId": "rel_001",
    "relationType": "synonym",
    "fromWordId": "wrd_abc123",
    "toWordId": "wrd_xyz789",
    "fromSenseId": "sns_001",
    "toSenseId": "sns_003"
  }
}
```

**Note:** Symmetric relations (synonym, antonym, homophone, confusable) enforce ordered word IDs to prevent duplicates.

##### Add Orthographic Variant

```http
POST /api/content/word/:id/variant
```

**Auth:** Admin only

**Request Body:**
```json
{
  "form": "colour",
  "region": "UK",
  "isPreferred": true
}
```

**Response (200):**
```json
{
  "success": true,
  "variant": {
    "variantId": "var_001",
    "wordId": "wrd_abc123",
    "form": "colour",
    "region": "UK",
    "isPreferred": true
  }
}
```

#### Courses

##### Create Course

```http
POST /api/content/course
```

**Auth:** Admin only

**Request Body:**
```json
{
  "grade": 3,
  "title": "Grade 3 Vocabulary",
  "status": "active",
  "defaultNewWordsPerSession": 5,
  "maxWordsPerSession": 15,
  "maxReviewWordsPerSession": 25,
  "sessionTimeBudgetS": 600
}
```

**Validation Schema:**
```typescript
{
  grade: number (1-12, unique);
  title: string (min 1);
  status?: "active" | "archived" (default "active");
  defaultNewWordsPerSession?: number (min 1);
  maxWordsPerSession?: number (min 1);
  maxReviewWordsPerSession?: number (min 1);
  sessionTimeBudgetS?: number (min 60);
}
```

**Response (200):**
```json
{
  "course": {
    "courseId": "crs_abc123",
    "grade": 3,
    "title": "Grade 3 Vocabulary",
    "status": "active",
    "defaultNewWordsPerSession": 5,
    "maxWordsPerSession": 15,
    "maxReviewWordsPerSession": 25,
    "sessionTimeBudgetS": 600,
    "lessonCount": 0,
    "createdTs": "2025-11-06T10:00:00Z"
  }
}
```

##### List Courses

```http
GET /api/content/course
```

**Auth:** Public read

**Response (200):**
```json
{
  "courses": [
    {
      "courseId": "crs_abc123",
      "grade": 3,
      "title": "Grade 3 Vocabulary",
      "status": "active",
      "lessonCount": 12,
      "createdTs": "2025-11-06T10:00:00Z"
    },
    {
      "courseId": "crs_def456",
      "grade": 4,
      "title": "Grade 4 Vocabulary",
      "status": "active",
      "lessonCount": 15,
      "createdTs": "2025-11-05T09:00:00Z"
    }
  ]
}
```

##### Get Course with Lessons

```http
GET /api/content/course/:id
```

**Auth:** Public read

**Response (200):**
```json
{
  "course": {
    "courseId": "crs_abc123",
    "grade": 3,
    "title": "Grade 3 Vocabulary",
    "status": "active",
    "defaultNewWordsPerSession": 5,
    "lessons": [
      {
        "lessonId": "lsn_001",
        "courseId": "crs_abc123",
        "title": "Lesson 1: Action Words",
        "orderNo": 1,
        "wordCount": 10,
        "targetNewPerSession": 5
      },
      {
        "lessonId": "lsn_002",
        "courseId": "crs_abc123",
        "title": "Lesson 2: Describing Things",
        "orderNo": 2,
        "wordCount": 12,
        "targetNewPerSession": 5
      }
    ]
  }
}
```

##### Update Course

```http
PATCH /api/content/course/:id
```

**Auth:** Admin only

**Request Body:**
```json
{
  "title": "Grade 3 Advanced Vocabulary",
  "defaultNewWordsPerSession": 6,
  "updateAllLessons": true
}
```

**Response (200):**
```json
{
  "course": {
    "courseId": "crs_abc123",
    "grade": 3,
    "title": "Grade 3 Advanced Vocabulary",
    "defaultNewWordsPerSession": 6,
    "updatedTs": "2025-11-06T12:00:00Z"
  }
}
```

**Note:** `updateAllLessons: true` cascades defaults to all lessons in the course.

#### Lessons

##### Create Lesson

```http
POST /api/content/lesson
```

**Auth:** Admin only

**Request Body:**
```json
{
  "courseId": "crs_abc123",
  "title": "Lesson 1: Action Words",
  "orderNo": 1,
  "targetNewPerSession": 5,
  "maxWordsPerSession": 15,
  "maxReviewWordsPerSession": 25
}
```

**Response (200):**
```json
{
  "lesson": {
    "lessonId": "lsn_001",
    "courseId": "crs_abc123",
    "title": "Lesson 1: Action Words",
    "orderNo": 1,
    "targetNewPerSession": 5,
    "maxWordsPerSession": 15,
    "maxReviewWordsPerSession": 25,
    "wordCount": 0,
    "createdTs": "2025-11-06T10:00:00Z"
  }
}
```

##### Add Word to Lesson

```http
POST /api/content/lesson/:id/words
```

**Auth:** Admin only

**Request Body:**
```json
{
  "wordId": "wrd_abc123",
  "orderNo": 1
}
```

**Response (200):**
```json
{
  "success": true,
  "mapping": {
    "lessonId": "lsn_001",
    "wordId": "wrd_abc123",
    "orderNo": 1
  }
}
```

##### Reorder Lesson Words

```http
PATCH /api/content/lesson/:id/words/reorder
```

**Auth:** Admin only

**Request Body:**
```json
{
  "wordIds": [
    "wrd_abc123",
    "wrd_def456",
    "wrd_ghi789"
  ]
}
```

**Response (200):**
```json
{
  "success": true,
  "count": 3
}
```

**Note:** `wordIds` array defines the new order. All words must belong to the lesson.

---

### Session Management

#### Start Session

```http
POST /api/session/start
```

**Auth:** Student only

**Request Body:**
```json
{
  "courseId": "crs_abc123",
  "lessonId": "lsn_001",
  "timeBudgetS": 600
}
```

**Validation Schema:**
```typescript
{
  courseId?: string (UUID);        // Auto-selects active course if omitted
  lessonId?: string (UUID);        // Optional: focus on specific lesson
  timeBudgetS?: number (1-3600);   // Default from course config
}
```

**Response (200) - New Session:**
```json
{
  "sessionId": "ses_abc123",
  "itemCount": 18,
  "newWordActivityCount": 15,
  "plannedDurationS": 600,
  "resuming": false,
  "completedItems": 0
}
```

**Response (200) - Resuming Session:**
```json
{
  "sessionId": "ses_xyz789",
  "itemCount": 18,
  "newWordActivityCount": 15,
  "plannedDurationS": 600,
  "resuming": true,
  "completedItems": 7
}
```

**Session Logic:**
- If active session exists and age < 2 hours: resume
- If active session exists and age ≥ 2 hours: abandon old, create new
- Otherwise: create new session
- Delivered items are reset to pending on resume

**Error (404):**
```json
{
  "error": {
    "code": "NOT_FOUND",
    "message": "No active course found"
  }
}
```

#### Get Next Item

```http
POST /api/session/:id/next
```

**Auth:** Student only (session owner)

**Response (200):**
```json
{
  "itemId": "itm_001",
  "activityType": "flashcard_usage",
  "phase": "new",
  "phaseProgress": {
    "current": 1,
    "total": 5
  },
  "word": {
    "wordId": "wrd_abc123",
    "headword": "conspire",
    "definition": "To secretly plan with others to do something harmful or illegal",
    "pos": "verb",
    "media": [
      {
        "mediaId": "med_001",
        "kind": "audio",
        "url": "https://r2.example.com/audio/conspire-us.mp3",
        "role": "word_pronunciation"
      }
    ]
  },
  "params": {
    "sentence": "The rivals _______ to overthrow the government.",
    "altSentence": null
  }
}
```

**Response (200) - Spelling Activity (headword hidden):**
```json
{
  "itemId": "itm_003",
  "activityType": "spell_typed",
  "phase": "new",
  "phaseProgress": {
    "current": 3,
    "total": 5
  },
  "word": {
    "wordId": "wrd_abc123",
    "headword": null,
    "definition": "To secretly plan with others to do something harmful or illegal",
    "pos": "verb",
    "media": [
      {
        "mediaId": "med_001",
        "kind": "audio",
        "url": "https://r2.example.com/audio/conspire-us.mp3",
        "role": "word_pronunciation"
      }
    ]
  },
  "params": null
}
```

**Activity Types:**
- `flashcard_usage`: Show word with example sentence
- `connect_sentence`: Multiple choice - select correct sentence
- `spell_typed`: Type the word (headword hidden)
- `meaning_mcq`: Multiple choice - select correct definition

**Security:** Headword is hidden for typed activities (`spell_typed`, `definition_typed`, `paraphrase_typed_gen`)

**Error (404):**
```json
{
  "error": {
    "code": "NOT_FOUND",
    "message": "No more items in session"
  }
}
```

#### Submit Attempt

```http
POST /api/session/:id/attempt
```

**Auth:** Student only (session owner)

**Request Body:**
```json
{
  "itemId": "itm_001",
  "answer": "conspire",
  "latencyMs": 1200,
  "hintsUsed": 0,
  "retriesUsed": 0,
  "timeSpentS": 15,
  "attemptId": "att_uuid_from_client"
}
```

**Validation Schema:**
```typescript
{
  itemId: string (UUID);
  answer: string | number;      // String for typed, number for MCQ (option index)
  latencyMs: number;            // Time to first keystroke/click
  hintsUsed: number;
  retriesUsed: number;
  timeSpentS?: number;          // Required for flashcards (min 10s)
  attemptId: string (UUID);     // Client-generated for idempotency
}
```

**Response (200) - Correct:**
```json
{
  "attemptId": "att_uuid_from_client",
  "correct": true,
  "score": 1.0,
  "feedback": null,
  "recycled": false,
  "recycleItemId": null,
  "cached": false
}
```

**Response (200) - Incorrect (Recycled):**
```json
{
  "attemptId": "att_uuid_from_client",
  "correct": false,
  "score": 0.0,
  "feedback": "The correct answer is 'conspire'.",
  "recycled": true,
  "recycleItemId": "itm_015",
  "cached": false
}
```

**Response (200) - Cached (Duplicate attemptId):**
```json
{
  "attemptId": "att_uuid_from_client",
  "correct": true,
  "score": 1.0,
  "feedback": null,
  "recycled": false,
  "recycleItemId": null,
  "cached": true
}
```

**Scoring Rules:**
- Flashcards: `timeSpentS < 10s` → incorrect
- Spelling: Exact match (case-insensitive) → correct
- MCQ: Correct option index → correct
- Hints/retries/slow (>30s) → Rating 2 (Hard) in FSRS

**Recycling:**
- NEW words: Recycle within section (K=4 gap, max 3 per word)
- REVIEW words: Recycle to end (max 3 per word)

#### Get Hint

```http
POST /api/session/:id/hint
```

**Auth:** Student only (session owner)

**Request Body:**
```json
{
  "itemId": "itm_003",
  "currentHints": 0
}
```

**Response (200):**
```json
{
  "hint": {
    "type": "first_letter",
    "text": "c"
  },
  "hintsUsed": 1,
  "maxHints": 3
}
```

**Hint Types (Progressive):**
1. `morphology`: Show word parts (if available)
2. `first_letter`: Show first letter
3. `cloze`: Show letters with blanks (e.g., "c_n_p_r_")

**Note:** Using hints marks item as weak (eligible for recycle).

#### Finalize Session

```http
POST /api/session/:id/finalize
```

**Auth:** Student only (session owner)

**Response (200):**
```json
{
  "sessionId": "ses_abc123",
  "itemsAnswered": 18,
  "accuracy": 0.83,
  "xpAwarded": 10,
  "summary": {
    "newWords": 5,
    "reviewWords": 8,
    "totalCorrect": 15,
    "totalIncorrect": 3,
    "avgLatencyMs": 1450,
    "totalTimeS": 580
  }
}
```

**XP Calculation:**
```typescript
accuracy >= 0.80: 1 XP/min
accuracy >= 0.65: 0.5 XP/min
accuracy < 0.65: 0 XP
```

**Side Effects:**
- Session state → `complete`
- FSRS commits for all words (NEW: trio completion, REVIEW: immediate)
- Session summary saved

---

### Progress & Analytics

#### Get Course Progress

```http
GET /api/me/progress/course/:id
```

**Auth:** Required (student sees own, admin sees all)

**Response (200):**
```json
{
  "progress": {
    "new": 120,
    "learning": 45,
    "reviewing": 78,
    "mastered": 156,
    "updatedAt": "2025-11-06T10:00:00Z"
  }
}
```

**Bucket Definitions:**
- `new`: Never seen
- `learning`: stability < 1 day
- `reviewing`: stability 1-20 days
- `mastered`: stability ≥ 21 days

#### Get Daily Plan

```http
GET /api/me/progress/course/:id/daily-plan
```

**Auth:** Required

**Response (200):**
```json
{
  "plan": {
    "newWords": 5,
    "reviewWords": 12,
    "estimatedMinutes": 10,
    "maxWordsPerSession": 15
  }
}
```

**Logic:**
- NEW words: Up to `defaultNewWordsPerSession` from course config
- REVIEW words: All due words (durability < 0.9, capped at `maxReviewWordsPerSession`)
- Time estimate: 30s/word

#### Get Lesson Progress

```http
GET /api/me/progress/course/:id/lessons
```

**Auth:** Required

**Response (200):**
```json
{
  "lessons": [
    {
      "lessonId": "lsn_001",
      "lessonTitle": "Lesson 1: Action Words",
      "orderNo": 1,
      "totalWords": 10,
      "introducedWords": 10
    },
    {
      "lessonId": "lsn_002",
      "lessonTitle": "Lesson 2: Describing Things",
      "orderNo": 2,
      "totalWords": 12,
      "introducedWords": 5
    }
  ]
}
```

#### Get Word Progress

```http
GET /api/me/progress/course/:id/words?bucket=reviewing&status=due&includeStats=true&limit=20
```

**Auth:** Required

**Query Parameters:**
```typescript
{
  lessonId?: string (UUID);
  bucket?: "new" | "learning" | "reviewing" | "mastered";
  status?: "due" | "upcoming";
  stabilityMin?: number;  // Days
  stabilityMax?: number;  // Days
  includeStats?: boolean; // Include per-word stats (slower)
  limit?: number;
  offset?: number;
}
```

**Response (200):**
```json
{
  "words": [
    {
      "wordId": "wrd_abc123",
      "headword": "conspire",
      "pos": "verb",
      "stability": 24.1,
      "durability": 0.86,
      "nextDue": "2025-11-12T04:00:00Z",
      "bucket": "reviewing",
      "lessonId": "lsn_001",
      "lessonTitle": "Lesson 1: Action Words",
      "stats": {
        "totalAttempts": 8,
        "successRate": 0.75,
        "avgLatencyMs": 1234,
        "totalHintsUsed": 2,
        "timesReviewed": 5,
        "lastAttemptDate": "2025-11-10T14:30:00Z"
      }
    }
  ],
  "total": 78
}
```

**Note:** `includeStats=true` performs additional lookups (slower query).

#### Get XP Ledger

```http
GET /api/me/xp/ledger
```

**Auth:** Required

**Response (200):**
```json
{
  "entries": [
    {
      "entryId": "xpe_001",
      "userId": "usr_123abc",
      "amount": 10,
      "source": "session_completion",
      "sessionId": "ses_abc123",
      "createdTs": "2025-11-06T10:30:00Z"
    },
    {
      "entryId": "xpe_002",
      "userId": "usr_123abc",
      "amount": 8,
      "source": "session_completion",
      "sessionId": "ses_def456",
      "createdTs": "2025-11-05T15:20:00Z"
    }
  ],
  "totalXp": 18
}
```

---

### Admin Operations

#### Students

##### Create Student

```http
POST /api/admin/students
```

**Auth:** Admin only

**Request Body:**
```json
{
  "email": "jane.student@demo.playcademy.com",
  "displayName": "Jane Student",
  "orgId": "org_456def",
  "role": "student",
  "timezone": "America/New_York",
  "reviewRolloverHour": 4
}
```

**Response (200):**
```json
{
  "student": {
    "userId": "usr_123abc",
    "email": "jane.student@demo.playcademy.com",
    "displayName": "Jane Student",
    "role": "student",
    "orgId": "org_456def",
    "timezone": "America/New_York",
    "reviewRolloverHour": 4,
    "createdTs": "2025-11-06T10:00:00Z"
  }
}
```

##### Assign Course to Student

```http
POST /api/admin/students/:id/assign-course
```

**Auth:** Admin only

**Request Body:**
```json
{
  "courseId": "crs_abc123"
}
```

**Response (200):**
```json
{
  "success": true,
  "userId": "usr_123abc",
  "courseId": "crs_abc123"
}
```

##### List Students with Progress

```http
GET /api/admin/students
```

**Auth:** Admin only

**Response (200):**
```json
{
  "students": [
    {
      "userId": "usr_123abc",
      "email": "jane.student@demo.playcademy.com",
      "displayName": "Jane Student",
      "courses": [
        {
          "courseId": "crs_abc123",
          "courseTitle": "Grade 3 Vocabulary",
          "grade": 3,
          "progress": {
            "new": 120,
            "learning": 45,
            "reviewing": 78,
            "mastered": 156
          }
        }
      ]
    }
  ]
}
```

#### Classes

##### Create Class

```http
POST /api/admin/classes
```

**Auth:** Admin only

**Request Body:**
```json
{
  "orgId": "org_456def",
  "name": "Ms. Johnson's 3rd Grade",
  "schoolYear": "2025-2026"
}
```

**Response (200):**
```json
{
  "class": {
    "classId": "cls_abc123",
    "orgId": "org_456def",
    "name": "Ms. Johnson's 3rd Grade",
    "schoolYear": "2025-2026",
    "createdTs": "2025-11-06T10:00:00Z"
  }
}
```

##### Add Students to Class

```http
POST /api/admin/classes/:id/enrollments
```

**Auth:** Admin only

**Request Body:**
```json
{
  "userIds": [
    "usr_123abc",
    "usr_def456",
    "usr_ghi789"
  ]
}
```

**Response (200):**
```json
{
  "success": true,
  "classId": "cls_abc123",
  "userIds": [
    "usr_123abc",
    "usr_def456",
    "usr_ghi789"
  ]
}
```

##### Assign Course to Class

```http
POST /api/admin/classes/:id/assign-course
```

**Auth:** Admin only

**Request Body:**
```json
{
  "courseId": "crs_abc123"
}
```

**Response (200):**
```json
{
  "success": true,
  "classId": "cls_abc123",
  "courseId": "crs_abc123",
  "studentsAssigned": 15
}
```

**Side Effect:** Creates `user_course` records for all enrolled students.

---

### GenAI Pipeline

#### Start Sprint (Simplified Workflow)

```http
POST /api/admin/genai/sprint
```

**Auth:** Admin only

**Description:** Create generation jobs for a batch of words. Generates: senses, examples, morphology, media (audio + images).

**Request Body:**
```json
{
  "words": ["conspire", "persuade", "exaggerate"],
  "grade": 3,
  "courseTitle": "Grade 3 Vocabulary",
  "lessonTitle": "Lesson 1: Action Words"
}
```

**Response (200):**
```json
{
  "batchId": "bat_abc123",
  "jobsCreated": 3,
  "jobIds": [
    "job_001",
    "job_002",
    "job_003"
  ]
}
```

#### Check Sprint Status

```http
GET /api/admin/genai/sprint/:batchId/status
```

**Auth:** Admin only

**Response (200):**
```json
{
  "batchId": "bat_abc123",
  "total": 3,
  "completed": 2,
  "inProgress": 1,
  "counts": {
    "succeeded": 2,
    "failed": 0,
    "pending": 1
  },
  "done": false
}
```

#### Get Sprint Results

```http
GET /api/admin/genai/sprint/:batchId/results
```

**Auth:** Admin only

**Response (200):**
```json
{
  "batchId": "bat_abc123",
  "total": 3,
  "succeeded": [
    {
      "jobId": "job_001",
      "headword": "conspire",
      "bundle": {
        "word": {
          "headword": "conspire",
          "pos": "verb",
          "definition": "To secretly plan with others..."
        },
        "senses": [...],
        "examples": [...],
        "morphology": [...],
        "media": [...]
      }
    }
  ],
  "failed": [
    {
      "jobId": "job_002",
      "headword": "persuade",
      "error": "OpenAI rate limit exceeded"
    }
  ]
}
```

#### Retry Failed Jobs

```http
POST /api/admin/genai/retry-failed
```

**Auth:** Admin only

**Request Body:**
```json
{
  "batchId": "bat_abc123"
}
```

**Response (200):**
```json
{
  "retriedCount": 1,
  "jobIds": ["job_002"]
}
```

---

## Data Models

### Core Entities

#### Word

```typescript
{
  wordId: string;           // UUID
  headword: string;         // The word itself
  lang: string;             // BCP-47 code (default "en")
  pos: "noun" | "verb" | "adjective" | "adverb" | "pronoun" | "preposition" | "conjunction" | "interjection";
  definition: string;       // Primary definition
  notes?: string;
  status: "draft" | "live" | "archived";
  createdTs: string;        // ISO 8601
  updatedTs: string;        // ISO 8601
}
```

#### Course

```typescript
{
  courseId: string;
  grade: number;            // 1-12, unique
  title: string;
  status: "active" | "archived";
  defaultNewWordsPerSession: number;
  maxWordsPerSession: number;
  maxReviewWordsPerSession: number;
  sessionTimeBudgetS: number;
  lessonCount: number;
  createdTs: string;
}
```

#### Lesson

```typescript
{
  lessonId: string;
  courseId: string;
  title: string;
  orderNo: number;
  targetNewPerSession: number;
  maxWordsPerSession: number;
  maxReviewWordsPerSession: number;
  wordCount: number;
  createdTs: string;
}
```

#### Session

```typescript
{
  sessionId: string;
  userId: string;
  courseId: string;
  lessonId?: string;
  state: "active" | "complete" | "abandoned";
  plannedDurationS: number;
  startTs: string;
  endTs?: string;
}
```

#### SRS State

```typescript
{
  stateId: string;
  userId: string;
  wordId: string;
  courseId: string;
  stability: number;        // Days (half-life of recall)
  durability: number;       // 0-1 (current retention)
  difficulty: number;       // FSRS difficulty
  nextDue: string;          // ISO 8601 (snapped to rollover hour)
  bucket: "new" | "learning" | "reviewing" | "mastered";
  lastReviewTs?: string;
  updatedTs: string;
}
```

---

## Rate Limiting & Performance

### Rate Limits

- **Default**: 100 requests/minute per user
- **Admin endpoints**: 200 requests/minute
- **GenAI endpoints**: 10 requests/minute (OpenAI rate limits)

**Rate Limit Headers:**
```http
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 87
X-RateLimit-Reset: 1699459200
```

**Error (429):**
```json
{
  "error": {
    "code": "RATE_LIMIT_EXCEEDED",
    "message": "Rate limit exceeded. Try again in 30 seconds.",
    "details": {
      "retryAfter": 30
    }
  }
}
```

### Performance Optimization

**Caching:**
- Course/lesson metadata: 5 minutes
- Word definitions: 1 hour
- User progress: No cache (real-time)

**Pagination:**
- Default limit: 50
- Max limit: 100
- Use `offset` for pagination (no cursor support yet)

**Query Optimization:**
- Word search uses trigram index for fuzzy matching
- Progress queries use composite indexes on `(userId, courseId, bucket)`
- Media URLs are pre-signed (1 hour expiry)

---

## Testing & Development

### Local Development

```bash
# Start dev server
bun dev

# API available at
http://localhost:8000/api
```

### Test Credentials

```
Admin: admin@demo.playcademy.com / password
Student (fresh): student.fresh@demo.playcademy.com / password
Student (progress): student.progress@demo.playcademy.com / password
```

### Test Headers

**Simulate time for testing:**
```http
X-Simulated-Now: 2025-11-06T10:00:00Z
```

**Disable rate limiting:**
```http
X-Bypass-Rate-Limit: true
```

### Example API Calls

**cURL:**
```bash
# Login
curl -X POST http://localhost:8000/api/auth/sign-in/email \
  -H "Content-Type: application/json" \
  -d '{"email":"student@demo.playcademy.com","password":"password"}' \
  -c cookies.txt

# Start session (with cookies)
curl -X POST http://localhost:8000/api/session/start \
  -H "Content-Type: application/json" \
  -d '{"courseId":"crs_abc123"}' \
  -b cookies.txt
```

**JavaScript (fetch):**
```javascript
// Login
const response = await fetch('http://localhost:8000/api/auth/sign-in/email', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ email: 'student@demo.playcademy.com', password: 'password' }),
  credentials: 'include' // Important: sends/receives cookies
});

// Start session
const session = await fetch('http://localhost:8000/api/session/start', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ courseId: 'crs_abc123' }),
  credentials: 'include'
});
```

---

## Appendix

### Status Codes Reference

| Code | Name | Usage |
|------|------|-------|
| 200 | OK | Successful GET/POST/PATCH/DELETE |
| 201 | Created | Resource created (not used - returns 200) |
| 400 | Bad Request | Invalid input or validation error |
| 401 | Unauthorized | Missing or invalid auth |
| 403 | Forbidden | Insufficient permissions |
| 404 | Not Found | Resource doesn't exist |
| 409 | Conflict | Duplicate or constraint violation |
| 429 | Too Many Requests | Rate limit exceeded |
| 500 | Internal Server Error | Unexpected server error |

### Content-Type Reference

| Type | Usage |
|------|-------|
| `application/json` | All request/response bodies (default) |
| `text/csv` | CSV export endpoints |
| `audio/mpeg` | Audio media files (TTS) |
| `image/png` | Image media files (DALL-E) |

### CORS Configuration

**Allowed Origins:**
- `http://localhost:5173` (Vite dev server)
- `https://your-domain.com` (production)

**Allowed Methods:**
```
GET, POST, PUT, PATCH, DELETE, OPTIONS
```

**Allowed Headers:**
```
Content-Type, Authorization, X-Simulated-Now
```

**Credentials:**
```
Access-Control-Allow-Credentials: true
```

---

## Changelog

### v1.0.0 (2025-11-06)
- Initial API specification
- All endpoints documented with examples
- Error handling patterns defined
- Rate limiting policies established

---

## Support

For questions or issues:
- GitHub Issues: https://github.com/2hourlearning/playcademy-vocab-app/issues
- Documentation: [docs/](./docs/)
- API Route Reference: [API_ROUTES.md](../API_ROUTES.md)
