# Anti-Schlock Prose Reference

Applies to: README files, website/UI copy, manuscript text, docstrings (explanatory sections), long block comments that explain concepts.
Does NOT apply to: code logic, short inline comments, variable names.

---

## Part I: AI Parallelisms

These are syntactically distinctive patterns that appear at statistically anomalous rates in LLM output — not because they're inherently wrong but because their *density* is inhuman. A Washington Post analysis of 328,744 ChatGPT messages found "not just X, but Y" variants in roughly 6% of all messages. The goal is not to prohibit the forms but to notice when you've deployed them more than once per page — or when they're doing no work the prose couldn't do more directly.

### Pattern 1: Negative Parallelism (the biggest tell)

The structure "It's not X, it's Y" or "Not just X, but Y" or "Less about X, more about Y."

**Why it appears**: LLMs use it as a cheap method of seeming to add nuance while avoiding commitment.

**Diagnosis**: If you find this structure more than once per page, you have a density problem. If you find it in a conclusion, you have a schlock problem — it's doing the reader's recognition work for them.

**Examples:**
❌ `This isn't just a tool — it's a philosophy.`
❌ `It's not about efficiency. It's about recognition.`
❌ `Less a methodology, more an encounter.`

**Fixes:**
- If the negation is doing real work (the reader might genuinely think X), keep it but make it specific: *"Unlike formal grading systems that rank competence, this approach..."*
- If it's just emphasis, state the positive claim directly: `Recognition is the center.` not `It's not about X, it's about recognition.`
- If you have two or more on a page, cut all but the strongest.

---

### Pattern 2: The Symmetrical "While X, Y" Trap

The structure "While X is true, Y is also important" — used to avoid commitment by diplomatically balancing two things.

**Why it appears**: Safe. Hedged. Sounds nuanced. Actually just diffuse.

**Examples:**
❌ `While formal logic provides rigor, embodied experience adds depth.`
❌ `While this approach has limitations, it also has significant strengths.`
❌ `Whether you are a practitioner or a researcher, this framework applies.`

**Fixes:**
- Take a position. Which is *more* important here and why?
- If both really do matter equally, say that directly: `The rigor and the embodiment are inseparable.`
- "Whether X or Y" constructions almost always signal the writer avoiding commitment; cut them or pick a side.

---

### Pattern 3: The Triadic List (Rule of Three Overuse)

LLMs systematically overuse three-part structures because they appear in so much training data. The pattern comes in two variants:

**Variant A — the verb escalation**: `It enables, empowers, and transforms.`
**Variant B — the noun trio**: `This is a project of recognition, understanding, and emancipation.`

**Diagnosis**: Ask whether all three items are doing distinct work. Usually one of the three is either redundant or weaker than the others.

**Fix:**
- Keep the strongest one or two.
- If you need three, make sure they're genuinely non-substitutable: `It requires time, willingness to be wrong, and a second person.`
- Vary the count. Human writing uses twos, fours, singles, and irregulars. The constant three is the tell.

---

### Pattern 4: The Em-Dash Pivot

The construction `[claim] — [restatement or surprise]` used to perform drama.

**Why it appears**: Trained on literary prose that uses em-dashes expressively; reproduces the *form* without the *function*.

**Examples:**
❌ `The answer is deceptively simple — trust the structure.`
❌ `Recognition is not passive — it is a form of action.`
❌ `This is the real insight — mathematics is already philosophical.`

**Diagnosis**: A legitimate em-dash introduces a genuine surprise, an appositive that earns its interruption, or a sharp syntactic contrast. A schlocky em-dash just delays an obvious conclusion.

**Fix:**
- If the second half genuinely reframes or surprises, keep it.
- If the second half is just emphasis, cut the em-dash and the first half: `Recognition is a form of action.`
- If you have more than two em-dash pivots per page, cut half.

---

### Pattern 5: Anaphoric Sentence-Opening (Repetition of Lead Words)

Multiple consecutive sentences beginning with the same word or phrase:

❌
```
This approach enables recognition. This recognition requires vulnerability.
This vulnerability is the condition for genuine encounter.
```

❌
```
We begin with the body. We find in the body a logic. We then trace that logic...
```

**Why it appears**: Creates an artificial sense of thematic unity. Sounds like rhetoric; reads as mechanical.

**Fix:**
- If the repetition is intentional and rhythmically deliberate (genuine anaphora), commit to it with at least four instances so it reads as a device. Two or three reads as accident.
- Otherwise, vary the sentence openings. Let one sentence begin mid-thought.

---

### Pattern 6: The Colon-Reveal as Pseudo-Depth

The structure `[Abstract claim]: [concrete restatement].`

❌ `There is one thing that matters: recognition.`
❌ `The core insight is this: mathematics was never purely formal.`
❌ `Three things define this approach: rigor, humility, and care.`

**Why it appears**: Mimics the rhetorical colon of good expository writing. Usually just delays a claim that could be stated directly.

**Fix:**
- If the material after the colon is genuinely more specific than what preceded it, the colon earns its place.
- If it's just restatement with extra weight, collapse it: `Recognition is what matters.`

---

### Pattern 7: The Rhetorical Question-Answer Pair

LLMs use this to simulate thinking-through. The deschlocker already targets it; here's the syntactic variant:

❌ `What does this mean for pedagogy? It means that...`
❌ `How do these threads resolve? They resolve through...`
❌ `Why does this matter? Because...`

**Fix:** Cut the question entirely. State the answer. The question is only doing work if its *specific phrasing* creates productive uncertainty the answer then navigates. That's rare.

---

### Pattern 8: The "At Once X and Y" / "Both X and Y" Construction

❌ `This is at once intimate and universal.`
❌ `The framework is both rigorous and accessible.`
❌ `Mathematics is simultaneously personal and impersonal.`

These are not wrong — they are *overused*. In LLM writing they appear as a cheap way to claim a synthesis without showing the dialectical movement that would earn it.

**Fix:**
- Show the movement instead of asserting the pairing: `The rigor of the formalism does not neutralize the intimacy — it heightens it, because the formalism is the intimacy made explicit.`
- If you keep the construction, it should come after you've earned it, not before.

---

## Part II: Schlock Patterns (from Deschlocker)

These apply primarily to manuscript text but also to README files and explanatory documentation. The core principle: **trust your structure**. When you step between your material and your reader to explain what's happening, you foreclose the very recognition you're trying to enable.

### What Is Definitely Schlock

| Pattern | Example | Fix |
|---------|---------|-----|
| Pedagogical forecasting | "This chapter will show/demonstrate that..." | Delete. Let the chapter show. |
| Interpretive cushioning | "What is philosophically significant here is that..." | Delete the frame. State the claim. |
| Retrospective self-interpretation | "I would reconstruct this experience differently now..." | Cut. |
| Meta-commentary | "This chapter has tried to..." / "The challenge here is to..." | Cut the meta-layer. Perform the work. |
| Over-explaining metaphors | Telling readers what your song/image "means" | Trust the metaphor. |
| Defensive positioning | Extended justifications for methodological choices | One sentence max. |
| Conclusion summaries | Rehearsing what just happened | Bridge forward instead. |
| Foreclosing recognition | "And so you see..." / "This is why..." | Cut. Let the reader see. |

### The Recognition Test

For each flagged passage, ask:
- Can the reader recognize this connection without my help?
- Does this passage interpret rather than perform?
- Am I explaining significance or letting it emerge?

If yes to any: probably schlock. Cut or compress radically.

### The Performative Test

Ask: "Is this sentence **performing** the work or **narrating** that I'm performing it?"

- **Performing**: "Ordinality precedes cardinality."
- **Narrating**: "What is philosophically significant here is that ordinality precedes cardinality."

Cut the narration. Keep the performance.

### Three-Sentence Rule for Conclusions

1. Minimal synthesis (1–2 sentences): name the movement without rehearsing details
2. Bridge forward (required): what grows from this ground?
3. Poetic closure (optional): image or phrase that recapitulates without explaining

Cut everything else.

### Red Flag Phrases — ALWAYS Cut

- "What is philosophically significant/important/crucial is that..."
- "It is worth noting that..."
- "Let me try to connect/show/demonstrate..."
- "If I'm understanding this correctly..."
- "I would reconstruct this differently now..."
- "This chapter has tried/attempted/worked to..."
- "The challenge of this section is..."
- "And so you see..."
- "What matters here is recognizing..."
- "How do these resolve? They resolve through..."

### What to Keep

- Connections to earlier chapters: "This pattern from Chapter 2..."
- Economical theoretical labels: "This is sublation — X remains yet Y is negated."
- Brief transitions between major sections
- Minimal positioning against alternatives (one sentence)

---

## Part III: Application by Surface

| Surface | Apply Part I (AI Parallelisms)? | Apply Part II (Schlock)? |
|---------|--------------------------------|--------------------------|
| Manuscript prose | Yes, full treatment | Yes, full treatment |
| README (explanatory sections) | Yes — especially triads, em-dashes, negative parallelism | Yes — especially meta-commentary and conclusion moralizing |
| Website copy / UI text | Yes, especially negative parallelism and em-dash pivot | Lightly — avoid "what this means for you is..." |
| Docstrings (explanatory) | Yes — especially triads and colon-reveals | Light treatment — short text limits schlock |
| Long block comments | Yes | Yes |
| Short inline comments | No | No |
| Code logic | No | No |

---

## Quick-Reference Density Rules

When reviewing a page of prose, flag if you find:

- **2+ instances** of negative parallelism ("not X, but Y")
- **3+ triadic lists** (usually one is fine)
- **3+ em-dash pivots**
- **2+ rhetorical question-answer pairs**
- **2+ "at once X and Y" constructions**
- **3+ consecutive sentences** beginning with the same word
- **Any colon-reveal** that restates rather than specifies
