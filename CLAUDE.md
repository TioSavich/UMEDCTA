# UMEDCTA — Project Instructions for Claude

## Voice and Epistemic Commitments

This is a philosophical project. Words matter — identifiers, comments, docstrings, UI copy, READMEs, and prose all carry inferential weight. Apply the **epistemic-code-voice** skill (`/epistemic-code-voice`) whenever writing or reviewing code artifacts in this repository.

Four commitments govern all prose surfaces in this codebase:

1. **Epistemic humility** — documentation invites understanding rather than asserting authority
2. **Anti-ocular language** — knowledge is not sight; avoid picture-thinking in non-technical prose
3. **Philosophical coherence** — when terms from the project dictionary appear (Recognition, Vernunft, Geist, Dialectic, Sublation, Intersubjectivity, and others), they carry their correct inferential commitments
4. **Anti-schlock** — user-facing prose (README, website, UI, manuscript) is free of AI parallelisms and foreclosing meta-commentary

The skill file at `.claude/skills/epistemic-code-voice/SKILL.md` documents the full procedure, including the philosophical dictionary, anti-ocular substitution tables, and anti-schlock pattern thresholds.

> **Note on scope**: Schlock rules and anti-ocular passes apply to prose users *read*. They do not apply to code logic.

---

## gstack

Use the `/browse` skill from gstack for all web browsing. Never use `mcp__claude-in-chrome__*` tools.

Available gstack skills:
- `/office-hours` - Office hours discussion
- `/plan-ceo-review` - CEO review planning
- `/plan-eng-review` - Engineering review planning
- `/plan-design-review` - Design review planning
- `/design-consultation` - Design consultation
- `/review` - Code review
- `/ship` - Ship a change
- `/browse` - Web browsing (use this for all web browsing)
- `/qa` - QA testing
- `/qa-only` - QA only (no code changes)
- `/design-review` - Design review
- `/setup-browser-cookies` - Set up browser cookies
- `/retro` - Retrospective
- `/investigate` - Investigate an issue
- `/document-release` - Document a release
- `/codex` - Codex tasks
- `/careful` - Careful mode
- `/freeze` - Freeze changes
- `/guard` - Guard mode
- `/unfreeze` - Unfreeze changes
- `/gstack-upgrade` - Upgrade gstack

If gstack skills aren't working, run `cd .claude/skills/gstack && ./setup` to build the binary and register skills.
