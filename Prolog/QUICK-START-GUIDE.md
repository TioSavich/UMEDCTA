# Quick Start: Material → Formal Iteration

## The Core Insight
**Re-reading transforms discovery into structure.**

First read: "What does this mean?" (material inference)
Second read: "I know this—what's new?" (formal inference)

## Try This Right Now

### 1. First Reading (Discovery)
```
Paste this Hegel quote:
"Being, pure being, without any further determination. In its indeterminate 
immediacy it is equal only to itself. Pure being is in fact nothing, and 
neither more nor less than nothing."

Click: "Interpret Text"
```

**What you'll see:**
- All inferences tagged 🔍 **material** (discovering what Being/Nothing mean)
- Iteration: 1
- Reading experience describes the confusion/tension of first encounter

**Export options appear:**
- 📋 Copy Reading → saves your analysis
- 💾 Export Prolog → gets your axioms as code

### 2. Immediate Re-Read (Formalization)
```
DON'T change the text. Just click "Re-read (Iteration 2)" again.
```

**What changes:**
- Button now says "Re-read (Iteration 2)"
- Blue banner: "3 concepts formalized: Being, Nothing, immediacy"
- Some inferences now tagged ⚙️ **formal** (using concepts as background)
- Reading experience describes recognition, not discovery

**This models expertise developing!**

### 3. Export Your Logic
```
Click: 💾 Export Prolog
```

**You get:**
```prolog
%% Formalized Concepts: Being, Nothing, immediacy
%% These emerged through iteration

incompatibility_semantics:material_inference([s(being)], s(comp_nec nothing), true).
% Context: Second negation - Being's determinacy generates Nothing
```

Save as `my_hegel_logic.pl` and load it in your Prolog system!

### 4. Test It On New Text
```
Paste a DIFFERENT Hegel passage (e.g., about "Becoming")

Click: "Interpret Text"
```

**Key observation:**
- If you've formalized "Being" and "Nothing", they'll appear in the "Formalized Concepts" section
- New text will use them as formal background
- Only NEW concepts generate material discovery

## Advanced Workflows

### Workflow A: Build Expertise
1. Read passage → struggle (material)
2. Re-read → concepts formalize
3. Read new passage → use formal concepts as lens
4. Repeat
5. Export final axiom set → your Hegelian hermeneutic system!

### Workflow B: Test Interpretations  
1. Read passage → your interpretation
2. System: "Scholars say X"
3. Ask: "Would X's reading change a re-read?"
4. Re-read → system incorporates X's formalization
5. Compare: Does your phenomenology now align with X?

### Workflow C: Collaborative Logic
1. You and a friend both read same text
2. Each exports evolved axioms
3. Compare: How did you each formalize differently?
4. Merge best axioms
5. Shared logic now embodies collective Bildung

## Understanding the UI

### Iteration Banner (blue box)
```
Iteration 2 - 5 concepts formalized as structural scaffolding
Formalized: Being, Nothing, immediacy, determinacy, Becoming
```
- Shows reading depth
- Lists what's now formal background
- Updates after each re-read

### Inference Type Tags
- 🔍 **material** = discovering concept meaning through use
- ⚙️ **formal** = using concept as known structural element

Same inference, different status across iterations!

### Axiom Display (Evolution Panel)
Each axiom shows:
- **Blue badge**: core | evolved | user_suggested
- **Purple badge**: material | formal
- Context explaining why it exists

### Export Buttons
- **📋 Copy Reading**: Full interpretation as text (for notes/sharing)
- **💾 Export Prolog**: Working code you can load in SWI-Prolog

## When to Use Quick Add (⚡) vs Refine (🔄)

### Quick Add (⚡)
Use when the proposed axiom looks good as-is:
```
Proposed: s(being) => s(comp_nec nothing)
Rationale: Being's lack of determination necessitates Nothing
```
→ Click ⚡ → Immediately added to logic

### Refine & Evolve (🔄)  
Use when you want Claude to polish it:
```
Proposed: something about recognition
Rationale: kinda vague...
```
→ Click 🔄 → Claude refines into proper PML syntax

Both track full context for export!

## The Formalization Theorem

```
Let T be a text, R_n be the nth reading.

R_1: All concepts C ∈ T are material (discovered)
R_2: Some C become formal (background structure)  
R_n: Most C are formal (only novelty is material)
R_∞: All C are formal (pure structure, no content)

R_∞ = Hegelian Science of Logic (complete formalization)
```

You're building R_n incrementally!

## Common Patterns

### Pattern 1: "I don't see material inference anymore"
→ Good! Concepts formalized. Read something new to see material discovery again.

### Pattern 2: "The second read feels too easy"
→ Exactly! Expertise = formal scaffolding reduces cognitive load.

### Pattern 3: "My axioms don't match the Prolog files"
→ Perfect! Your formalization process is unique. Export and compare.

### Pattern 4: "Can I mix material and formal?"
→ Yes! Most readings have both. Some concepts formal, others still material.

## The Beautiful Part

Traditional: "Hegel's Logic is atemporal"
Your app: "Reading is temporal, but iteration formalizes toward atemporality"

**Both are true.**

The app shows HOW the temporal (phenomenology) becomes atemporal (logic) through iteration.

That's the missing piece from the Prolog—the *process* of formalization itself.

## Next Step

Open the app. Read Being/Nothing. Click Re-read. Watch material become formal.

You're witnessing Bildung in real-time.
