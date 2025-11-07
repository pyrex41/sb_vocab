# Vocabulary Game Documentation Index

**Project:** Playcademy Vocabulary Learning Application
**Last Updated:** 2025-11-07
**Status:** Common Lisp Implementation Planning Complete

---

## 🎯 Current Focus: Common Lisp Frontend Implementation

The project is transitioning from Godot/GDScript to a **Common Lisp web application** as the frontend. This provides native web deployment without game engine overhead.

---

## 📚 Documentation Structure

### Common Lisp Implementation (NEW - Active)

**Start Here:** [README-cl-frontend.md](./README-cl-frontend.md)
- Overview of Common Lisp approach
- Technology stack
- Quick links to all CL docs

**Core Documents:**

1. **[PRD: Common Lisp Implementation](./prd-common-lisp-implementation.md)**
   - Complete technical specification
   - Architecture diagrams
   - File structure
   - Full code examples
   - Deployment strategies
   - **Length:** ~80 pages
   - **Use:** Primary reference document

2. **[Task Breakdown](./tasks-cl-implementation.md)**
   - Hour-by-hour task list (22 tasks)
   - 4-phase implementation plan
   - Daily schedule
   - Acceptance criteria per task
   - **Length:** ~50 pages
   - **Use:** Development roadmap

---

### Playcademy Backend Integration (Reference)

3. **[Playcademy Vocab PRD](./playcademy-vocab-prd.md)**
   - Original Godot integration PRD
   - Backend API specifications
   - Activity requirements
   - Still relevant for:
     - Understanding activity types
     - API contract details
     - Success criteria

4. **[Playcademy Quick Reference](./playcademy-quick-reference.md)**
   - API endpoint summary
   - Request/response examples
   - Common patterns

5. **[Implementation Checklist](./playcademy-implementation-checklist.md)**
   - Testing procedures
   - Deployment steps
   - Verification checklist

---

### Archive (Historical Reference)

6. **[prd-cl.md](./prd-cl.md)**
   - ⚠️ **OUTDATED:** Mistakenly contains Rust/Leptos location detection AI
   - **Not relevant** to vocabulary game
   - Kept for reference only

7. **[prd-integrate.md](./prd-integrate.md)**
   - Contains some Godot/GDScript code
   - Partially relevant for understanding previous approach
   - **Superseded by:** prd-common-lisp-implementation.md

8. **[prompt.md](./prompt.md)**
   - Historical planning notes
   - May contain useful context

---

## 🚀 Quick Start Paths

### For Developers Starting Implementation

**Path:** README → PRD → Tasks

1. Read [README-cl-frontend.md](./README-cl-frontend.md) (10 min)
   - Get big picture
   - Understand tech stack

2. Skim [prd-common-lisp-implementation.md](./prd-common-lisp-implementation.md) (30 min)
   - Focus on Sections 1-4 (Architecture, File Structure, Core Files)
   - Review code examples

3. Use [tasks-cl-implementation.md](./tasks-cl-implementation.md) for execution
   - Follow day-by-day breakdown
   - Check off tasks as completed

---

### For Product/Design Review

**Path:** README → PRD (Sections 5, 7, 9)

1. Read [README-cl-frontend.md](./README-cl-frontend.md) (10 min)
   - Understand what we're building

2. Review [prd-common-lisp-implementation.md](./prd-common-lisp-implementation.md)
   - **Section 5:** Activity Implementations (templates)
   - **Section 7:** Godot vs CL Comparison
   - **Section 9:** Success Criteria

3. Reference [playcademy-vocab-prd.md](./playcademy-vocab-prd.md)
   - Activity descriptions still apply
   - UI/UX requirements unchanged

---

### For Understanding Backend API

**Path:** Quick Reference → API Contract

1. Read [playcademy-quick-reference.md](./playcademy-quick-reference.md)
   - API endpoints summary
   - Request/response examples

2. Reference [playcademy-vocab-prd.md](./playcademy-vocab-prd.md)
   - **Section 5:** Data Structures & API Contract
   - Detailed request/response specs

3. Check [prd-common-lisp-implementation.md](./prd-common-lisp-implementation.md)
   - **Section 3.4:** API Client implementation
   - Common Lisp-specific API integration

---

## 📊 Document Comparison

| Document | Purpose | Length | Status | Primary Audience |
|----------|---------|--------|--------|------------------|
| **README-cl-frontend** | Overview & navigation | 5 pages | ✅ Current | All stakeholders |
| **prd-common-lisp-implementation** | Complete spec | 80 pages | ✅ Current | Developers |
| **tasks-cl-implementation** | Task breakdown | 50 pages | ✅ Current | Developers |
| **playcademy-vocab-prd** | Backend integration | 70 pages | 📚 Reference | Developers, Product |
| **playcademy-quick-reference** | API summary | 10 pages | 📚 Reference | Developers |
| **implementation-checklist** | Testing/deploy | 15 pages | 📚 Reference | QA, DevOps |
| prd-cl.md | Unrelated project | 5 pages | ⚠️ Archive | None |
| prd-integrate.md | Godot code snippets | 10 pages | ⚠️ Archive | Historical |

---

## 🔄 Implementation Status

### Completed ✅
- [x] Common Lisp architecture designed
- [x] Technology stack selected
- [x] File structure defined
- [x] Complete PRD written
- [x] Task breakdown created
- [x] Hour-by-hour schedule planned

### In Progress 🔄
- [ ] Phase 1: Core Infrastructure (0 / 6 hours)
- [ ] Phase 2: Web Server & Routing (0 / 6 hours)
- [ ] Phase 3: Frontend & Templates (0 / 8 hours)
- [ ] Phase 4: Polish & Testing (0 / 4 hours)

### Not Started ⏸️
- [ ] Deployment to production
- [ ] Performance optimization
- [ ] Cross-browser testing

---

## 💡 Key Decisions

### Why Common Lisp over Godot?

**Advantages:**
- ✅ Native web deployment (no WASM export)
- ✅ Standard HTTP server (easier hosting)
- ✅ REPL-driven development (faster iteration)
- ✅ Strong type safety (fewer runtime errors)
- ✅ Server-side rendering (better SEO)

**Trade-offs:**
- ⚠️ Steeper learning curve (Lisp knowledge required)
- ⚠️ Less visual tooling (no scene editor)
- ⚠️ Smaller ecosystem (fewer UI libraries)

**Decision:** Common Lisp chosen for this project due to:
1. API-heavy nature (minimal game logic)
2. Team has Lisp experience
3. Standard web deployment preferred
4. Strong typing benefits outweigh visual editor loss

---

## 🎯 Success Metrics

### Technical
- [ ] All 5 activity types functional
- [ ] Complete session flow (start → results)
- [ ] Page load < 500ms
- [ ] API response < 2s
- [ ] Zero console errors
- [ ] Works on Chrome, Firefox, Safari, Edge

### Functional
- [ ] Grade 3, 4, 5 sessions complete correctly
- [ ] Score calculation accurate
- [ ] Progress persists across sessions
- [ ] Audio playback works (spelling activity)
- [ ] Error handling graceful

### Code Quality
- [ ] All unit tests pass
- [ ] No compiler warnings
- [ ] Complete documentation
- [ ] Follows Common Lisp style guide
- [ ] Code coverage > 80%

---

## 📞 Getting Help

### Questions About...

**Architecture & Design**
→ Read [prd-common-lisp-implementation.md](./prd-common-lisp-implementation.md) Sections 1-2

**Implementation Tasks**
→ Check [tasks-cl-implementation.md](./tasks-cl-implementation.md)

**Backend API**
→ Reference [playcademy-quick-reference.md](./playcademy-quick-reference.md)

**Activity Requirements**
→ See [playcademy-vocab-prd.md](./playcademy-vocab-prd.md) Section 3

**Deployment**
→ Read [prd-common-lisp-implementation.md](./prd-common-lisp-implementation.md) Section 6

---

## 🔗 External Resources

### Common Lisp Learning
- [Practical Common Lisp](http://www.gigamonkeys.com/book/) - Free book
- [Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/)
- [Caveman2 Guide](https://github.com/fukamachi/caveman)
- [Parenscript Manual](https://parenscript.common-lisp.dev/)

### Playcademy Platform
- API Documentation: `docs.playcademy.net`
- Developer Portal: `dev.playcademy.net`
- Support: `support@playcademy.net`

---

## 📝 Document Maintenance

### Adding New Documentation

1. Create document in `.taskmaster/docs/`
2. Add entry to this INDEX.md
3. Update relevant quick start paths
4. Update status table

### Deprecating Old Documentation

1. Move to "Archive" section
2. Add ⚠️ warning at top of deprecated doc
3. Link to replacement doc
4. Keep for 90 days, then delete

### Updating Existing Documentation

1. Increment version number
2. Add "Last Updated" date
3. Note changes in commit message
4. Update related docs if needed

---

**Last Updated:** 2025-11-07
**Maintainer:** Development Team
**Status:** Planning Complete → Ready for Implementation
