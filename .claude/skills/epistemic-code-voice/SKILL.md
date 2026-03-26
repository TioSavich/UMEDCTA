---
name: epistemic-code-voice
description: >
  Enforce epistemic humility, anti-ocular language, philosophical terminological consistency,
  and anti-schlock prose in code, comments, docstrings, UI copy, README, and website text.
  Synthesizes Carspecken's anti-picture-thinking, the project's philosophical dictionary, and
  deschlocker patterns including AI parallelisms (negative parallelism, em-dash pivots, triadic
  lists, "while X, Y" constructions). Use whenever writing or reviewing code, websites, READMEs,
  or software for Tio's philosophical projects — especially when artifacts contain identifiers or
  comments drawing on philosophical concepts (Recognition, Vernunft, Geist, Dialectic, Sublation),
  or when writing user-facing prose free of AI syntactic tells. Trigger on: writing/reviewing code,
  generating README or docs, writing UI or website copy, naming variables for philosophical concepts,
  or editing prose that should embody emancipatory commitments. Schlock rules do NOT apply to code
  logic — only to prose users read.
---

# Epistemic Code Voice

This skill enforces four interlocking commitments across all code artifacts:

1. **Epistemic humility** — prose in code (comments, docs, UI) does not assert authority; it invites understanding
2. **Anti-ocular language** — no picture-thinking in non-technical prose; knowledge is not sight
3. **Philosophical coherence** — when dictionary terms appear (as identifiers, in comments, in UI), they carry their correct inferential commitments
4. **Anti-schlock** — user-facing prose (README, website, manuscript, docs) is free of AI parallelisms, foreclosing meta-commentary, and interpretive hand-holding. The code itself is exempt from schlock rules.

## Code Surfaces and Treatment

Code artifacts have four distinct surfaces. Each gets different treatment:

| Surface | Examples | Apply Steps |
|---------|----------|-------------|
| **Identifiers** | variable names, functions, classes | 2 (dict), 6 (identifiers) |
| **Inline comments** | `// ...` short notes | 3 (anti-ocular), 4 (humility) |
| **Docstrings / block comments** | function docs, module headers | 2, 3, 4, 5 (schlock — light) |
| **External prose** | README, UI copy, website, manuscript | 2, 3, 4, 5 (full schlock) |

**Key scope rule**: Steps 1–4 apply wherever there is prose. Step 5 (anti-schlock) applies to text users actually *read* — README, website copy, UI, manuscript. It does **not** apply to code logic, however schlocky.

---

## Step 1: Identify the Surface

Before applying any intervention, determine which surface(s) are present in the request. A single file may have all four.

---

## Step 2: Dictionary Check (When Philosophical Terms Appear)

**When to trigger**: Any identifier, comment, or prose containing terms from the philosophical dictionary.
Dictionary terms include but are not limited to:
*Vernunft, Verstand, Geist, Dialectic, Sublation, Recognition, Intersubjectivity, Communicative Action, 
Strategic Action, Apperception, Concept (Begriff), Determinate Negation, Absolute Negativity, Alienation, 
Praxis, Embodied Mathematics, Material Inference, Pragmatic Expressive Bootstrapping, Validity Claims,
CUSP you, Generalized Other, Feeling-Body, Trace, Deconstruction, Cultural Power, Interactive Power*

**Procedure**:
1. Load `references/dictionary.md`
2. Identify all dictionary terms in the code artifact
3. For each term, check:
   - Is the term used with the meaning the dictionary gives it?
   - Are the term's cross-references respected? (e.g., if `Vernunft` appears, is its relation to `Verstand` preserved?)
   - Does the code's logic enact the concept or merely name it?
4. Flag discrepancies using the same three types as the dictionary checker:
   - **Contradiction**: code logic conflicts with the dictionary definition
   - **Inconsistency**: term is used loosely, not per definition
   - **Extension**: term is applied to a new domain (flag, don't block)

**Quick cross-reference map** (most common pairings):
- `Vernunft` ↔ `Verstand`, `Dialectic`, `Geist`, `Concept`
- `Recognition` ↔ `Intersubjectivity`, `Alienation`, `Geist`, `Self-Consciousness`
- `Communicative Action` ↔ `Strategic Action`, `Instrumental Action`, `Validity Claims`
- `Sublation` (Aufhebung) ↔ `Determinate Negation`, `Becoming`, `Dialectic`
- `Apperception` ↔ `Self-Consciousness`, `Recognition`, `Geist`
- `Material Inference` ↔ `Pragmatic Expressive Bootstrapping`, `Algorithmic Elaboration`

---

## Step 3: Anti-Ocular Pass

**When to trigger**: Any prose in the artifact — comments, docstrings, README, UI copy.
**Skip**: Technical vocabulary with established domain meanings (see below).

### The Technical Vocabulary Exception (Do Not Flag)

These are architectural/technical terms, not epistemic claims:
- `view` in Django/Rails/React, `render` in any UI framework, `display` in CSS
- `observe` in RxJS/observables or design patterns
- `watch` in Webpack/Vue, `visible` as a CSS/DOM property
- `transparent` in networking/CSS, `show`/`hide` as UI state toggles

**The test**: Is this term naming a *mechanism* or making a *claim about how knowing works*?
Mechanisms keep their names. Knowledge claims get revised.

### What to Replace

For comprehensive patterns, read `references/anti-ocular-code.md`.

**Quick substitution table for most common occurrences**:

| Ocular (in prose) | Replacement |
|-------------------|-------------|
| `see that X` | `recognize that X`, `notice that X`, `find that X` |
| `illuminates` / `sheds light on` | `clarifies`, `makes accessible`, `helps us grasp` |
| `lens of` / `through the lens` | `framework of`, `from within`, `using the approach of` |
| `perspective` (epistemic) | `standpoint`, `position`, `approach` |
| `insight into` | `understanding of`, `grasp of` |
| `clear that X` | `evident that X`, `we can recognize that X` |
| `highlight` | `draw attention to`, `emphasize`, `foreground` |
| `visible pattern` | `evident pattern`, `discernible pattern`, `emerging pattern` |
| `obvious` / `trivially` / `simply` | Remove — these foreclose understanding |

---

## Step 4: Epistemic Humility Pass

**When to trigger**: Any prose, but especially docstrings, README, and UI copy.

### Core Principle

Code documentation that tells readers what is "correct," "optimal," "crucial," or "obvious" forecloses the reader's own inferential engagement. It performs authority rather than supporting understanding. Replace assertions with *grounded claims* that show their reasoning.

### Patterns

**Certainty → Grounded claim**:
- `// The correct approach is X` → `// X works here because it preserves [reason]`
- `// This is optimal` → `// This performs well for [specific reason]`
- `// X is essential` → `// X matters here because [what breaks without it]`

**Puffery → Specificity** (especially README):
- `A powerful framework for...` → `A framework for...`
- `Seamlessly integrates` → describe the integration mechanism
- `Intuitive interface` → describe what makes it accessible and to whom
- `Leverages X` → `Uses X`
- `Harnesses the potential of` → `Works with`

**Foreclosure words → Remove or reframe**:
- `obviously`, `trivially`, `simply`, `just` — drop these unless you mean them literally
- `Of course X` → `X` or `X because [reason]`
- `As everyone knows` → remove entirely

**Dialogical openings** (for longer docs):
- Instead of `This section explains X:` → `This section works through X, attending to [what makes it non-trivial]`
- Instead of `We demonstrate that X` → `What this suggests is that X`
- Instead of `In conclusion,` → `What this gives us is`, `Where this leaves us:`

---

## Step 5: Anti-Schlock Pass (README, Website, Manuscript, Docs)

**When to trigger**: Any prose the user actually reads — README, website copy, UI text, manuscript passages, long explanatory docstrings.
**Skip entirely**: Code logic, short inline comments.

Load `references/anti-schlock-prose.md` for full patterns. Summary:

### AI Parallelisms (the syntactic tells)

These aren't wrong in isolation — they're wrong at AI density. Flag if you find more than the thresholds below per page:

| Pattern | Threshold | Example | Fix |
|---------|-----------|---------|-----|
| **Negative parallelism** | >1 per page | `It's not X, it's Y` / `Not just X, but Y` | State the positive claim directly |
| **Symmetrical "While X, Y"** | >2 per page | `While X is true, Y matters` | Take a position or show the relation |
| **Triadic lists** | >2 per page | `enables, empowers, and transforms` | Cut the weakest; vary count |
| **Em-dash pivot** | >2 per page | `simple — trust the structure` | Ask if it earns its interruption |
| **Anaphoric openings** | 2–3 repetitions | Three sentences starting "This..." | Vary, or commit to ≥4 for intentional anaphora |
| **Colon-reveal** | restates only | `One thing matters: recognition` | Collapse if it's just emphasis |
| **Rhetorical Q+A** | any | `What does this mean? It means...` | Cut the question; state the answer |
| **"At once X and Y"** | >1 per page | `at once intimate and universal` | Show the dialectical movement; don't just assert the pair |

### Schlock Patterns (foreclosing reader recognition)

**Always cut**:
- `What is philosophically significant here is that...`
- `This chapter will show/demonstrate/prove that...`
- `And so you see...` / `This is why...`
- `What matters here is recognizing...`
- `This chapter has tried to...` / `The challenge here is to...`
- Extended interpretation of your own metaphors, songs, or examples

**Cut or compress to one sentence**:
- Pedagogical roadmaps beyond a brief structural note
- Defensive positioning paragraphs
- Conclusion summaries (bridge forward instead)

**Performative test**: Is this sentence *performing* the work or *narrating* that work is being performed? Cut the narration.

For full patterns and edge cases: `references/anti-schlock-prose.md`

---

## Step 6: Identifier Review

When naming identifiers that represent philosophical concepts:

1. **Check the dictionary** for the concept's core commitments and cross-references
2. **Prefer verb forms** for processes: `sublate()` over `performSublation()`
3. **Preserve relational structure**: if a concept is always already paired (Vernunft/Verstand, {I}/"me"), consider whether the interface reflects this
4. **No ocular terms for philosophical concepts**: not `InsightEngine`, `IlluminationProvider`, `ViewOfGeist`
5. **German terms only when specificity demands**: use `vernunft` over `reason` when the *Hegelian* distinction from `verstand` is load-bearing; otherwise use the English

For detailed identifier naming guidance and examples, see `references/anti-ocular-code.md`.

---

## Reporting

### For generation tasks (writing new code):
Apply all relevant steps silently. After generating, add a brief note:
```
// Applied: dictionary check (terms: X, Y), anti-ocular pass, epistemic humility pass, anti-schlock pass
```
Only flag issues that require your decision.

### For review tasks (checking existing code):
Use this structure:

```markdown
## Epistemic Code Voice Review

### Dictionary Terms Found: [list]
**[term]** — [Correct / Inconsistency / Contradiction / Extension]
- If issue: what the dictionary says vs. what the code implies, with suggestion

### Anti-Ocular Issues: [count]
**Line N**: `[ocular phrase]` → suggested: `[replacement]`
(skip technical vocabulary exceptions)

### Epistemic Humility Issues: [count]
**Line N**: `[assertive phrase]` → suggested: `[grounded version]`

### Anti-Schlock Issues (prose surfaces only): [count]
**AI Parallelisms**: list instances with line numbers and pattern names
**Schlock patterns**: list instances with line numbers and category

### Identifier Concerns: [if any]
**[identifier]**: [concern re: philosophical accuracy or cross-reference fidelity]

### Overall: [coherent / minor revisions / needs rethinking]
```

---

## Reference Files

- `references/dictionary.md` — Full philosophical dictionary (Tio's project dictionary). Load when dictionary terms are present.
- `references/anti-ocular-code.md` — Anti-ocular patterns adapted for code contexts, including technical vocabulary exceptions and identifier naming guidance. Load for thorough anti-ocular passes.
- `references/anti-schlock-prose.md` — AI parallelisms and schlock patterns for user-facing prose (README, website, manuscript, docs). Load for Step 5.

---

## Notes on Integration

This skill is the *code-facing* complement to the manuscript editing skills. The order of operations when the same project generates both code and prose:

1. Use **epistemic-code-voice** (this skill) when writing or reviewing code artifacts
2. Use **carspecken-voice-editor** when editing manuscript prose
3. Use **philosophical-dictionary-checker** for manuscript-level dictionary checks
4. The dictionary bundled here (`references/dictionary.md`) is the same source; no divergence in definitions
