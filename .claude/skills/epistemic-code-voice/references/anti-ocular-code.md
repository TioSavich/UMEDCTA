# Anti-Ocular Patterns for Code Contexts

## Why This Matters in Code

Phil Carspecken's critique of "picture-thinking" targets visual metaphors that smuggle a passive, observer/observed epistemology into language. Code comments, docstrings, README files, and UI copy are *writing*, and they carry the same epistemic commitments as any other prose. The goal is not pedantic word-policing but coherence: a codebase built to enact communicative, embodied, intersubjective knowing should not narrate itself through the metaphors of disembodied sight.

## The Technical Vocabulary Exception

Many technical terms have legitimate, domain-specific meanings that are *not* ocular metaphors. These should **not** be flagged or replaced:

| Term | Technical Context | Keep as-is? |
|------|-----------------|-------------|
| `view` | Django/Rails MVC, SQL views, React "views" | Yes — architectural term |
| `render` | React, templates, graphics pipeline | Yes — technical process term |
| `display` | CSS, UI layout | Yes — output mechanism |
| `show` | UI state (show/hide toggle) | Yes — boolean UI state |
| `visible` | CSS `visibility`, DOM state | Yes — property name |
| `transparent` | CSS `opacity`, networking protocols | Yes — technical property |
| `observe` | RxJS observables, design patterns | Yes — established pattern name |
| `watch` | Webpack watchers, Vue `watch` | Yes — file/state monitoring |

**The test**: Is this term naming a *technical mechanism* or making a *knowledge claim* about how understanding works? Mechanisms keep their names. Knowledge claims get revised.

---

## Ocular Terms to Replace in Prose Contexts

### In comments, docstrings, README, UI copy — these should be revised:

| Ocular Term | Code Prose Example | Better Alternative |
|------------|-------------------|-------------------|
| `see` (epistemic) | `// See how recognition works` | `// Notice how`, `// Understand how`, `// Trace how` |
| `see` (forward-ref) | `// see also UserNode` | `// compare UserNode`, `// related: UserNode` ← (forward refs acceptable) |
| `show` (epistemic) | `// This shows that X` | `// This indicates that X`, `// This demonstrates that X` |
| `illuminate` | `// This illuminates the structure` | `// This clarifies the structure`, `// This makes the structure accessible` |
| `shed light on` | prose only | `// clarify`, `// help us grasp` |
| `lens` | `// Through the lens of recognition theory` | `// From within recognition theory`, `// Using the framework of` |
| `perspective` | `// From this perspective` | `// From this standpoint`, `// Given this approach` |
| `insight` | `// provides insight into` | `// helps us understand`, `// deepens our grasp of` |
| `highlight` | `// This highlights X` | `// This draws attention to X`, `// This emphasizes X` |
| `clear` (epistemic) | `// It is clear that X` | `// It becomes evident that X`, `// We can recognize that X` |
| `obvious` | `// Obviously, X` | `// Notably, X`, `// As becomes evident, X` — or just remove the qualifier |
| `visible` (epistemic) | `// The pattern is visible in X` | `// The pattern is evident in X`, `// The pattern emerges in X` |
| `view` (position) | `// In our view, X` | `// We understand X as`, `// Our position is that X` |

---

## Epistemic Humility Patterns for Technical Prose

### Authoritative Assertions → Dialogical Claims

These AI-generated patterns perform certainty where humility is more honest:

| Avoid | Prefer |
|-------|--------|
| `// This approach is optimal` | `// This approach works well here because...` |
| `// The correct way to do X` | `// One way to do X that maintains...` |
| `// This is the standard pattern` | `// This follows the pattern where...` |
| `// Simply call X` | `// Call X` (drop "simply" — it forecloses struggle) |
| `// Trivially, X` | `// X` |
| `// Obviously, X` | `// X` (or `// Notably, X` if emphasis needed) |
| `// X is crucial/pivotal/essential` | `// X matters here because...` |
| `// This will transform X` | `// This changes X by...` |

### AI Puffery in Docs

Particularly in README files and docstrings:

| Avoid | Prefer |
|-------|--------|
| `A powerful framework for...` | `A framework for...` |
| `Leverages X to achieve Y` | `Uses X to achieve Y` |
| `Harnesses the full potential of` | `Works with` |
| `Cutting-edge solution` | (describe what it actually does) |
| `Seamlessly integrates` | `Integrates` (or describe how) |
| `Intuitive interface` | (describe what makes it accessible) |
| `Robust and scalable` | (describe the specific properties) |

---

## Transitions in Docstrings and Long Comments

For longer explanatory comments (multi-line, function-level, module-level):

| Avoid | Prefer |
|-------|--------|
| `// Moreover,` | `// This also`, `// Building on this,` |
| `// Furthermore,` | `// Following from this,` |
| `// However,` | `// But here we encounter`, `// Yet` |
| `// Thus,` | `// What follows is`, `// This means` |
| `// In conclusion,` | `// What this gives us is`, `// The result is` |

---

## Identifier Naming (Variables, Functions, Classes)

When naming identifiers that represent philosophical concepts from the dictionary:

### Naming Principles

1. **Prefer accuracy over cleverness**: The identifier `sublation` is better than `dialecticalNegation` if that's the concept meant
2. **German terms**: Use the German when it preserves specificity: `vernunft`, `verstand`, `geist` — but only when the distinction matters. Don't use German terms decoratively.
3. **Cross-references in names**: If a concept inherently references another (per the dictionary), consider whether the naming relationship preserves that. `RecognitionNode` and `MisrecognitionNode` maintain the dialectical pairing.
4. **Avoid nominalization inflation**: `sublationProcess()` is weaker than `sublate()`. Prefer verb forms where possible.
5. **Don't use ocular terms for philosophical concepts**: `ViewOfGeist`, `InsightProvider`, `IlluminationEngine` — these name philosophical content with ocular metaphors. Use `GeistModel`, `UnderstandingProvider`, `ClarificationEngine`.

### When Dictionary Terms Appear as Identifiers

Check `references/dictionary.md` for the concept's cross-references. If a class or module represents a concept, its public interface should respect those cross-references:

```python
# WEAK — names the concept but severs its relational structure
class Vernunft:
    def process(self, thought): ...

# STRONGER — interface reflects dictionary cross-references 
# (Vernunft relates to Verstand, Dialectic, Concept, Geist)
class Vernunft:
    def sublate(self, verstand_determination): ...
    def comprehend_as_moment(self, particular): ...
```
