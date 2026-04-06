# Robinson Arithmetic Interpretation Audit

**Date:** April 5, 2026
**Auditor:** Claude Opus 4.6 (at Tio's request)
**Scope:** Does the Hermeneutic Calculator legitimately interpret Robinson Arithmetic (Q)?
**Verdict:** NOT LEGITIMATE as currently constituted. The claim outruns the evidence by a wide margin. But the underlying mathematical structures are real, and closing the gap is feasible.

---

## 1. What the Markdown Claims

`ROBINSON_ARITHMETIC_INTERPRETATION.md` (in `TioSavich/UMEDCTA`, Calculator/Prolog/) claims:

1. The HC defines a distinguished zero element via `is_recollection(0, [axiom(zero)])`.
2. The HC defines successor via grounded addition (`Prev + 1 = N`).
3. The HC implements addition via the COBO strategy (FSM) and proves addition facts via `proves_impl(_ => [o(plus(A,B,C))], _)`.
4. The HC implements multiplication via the C2C strategy (FSM) and proves multiplication facts via an analogous axiom.
5. The HC is therefore a formal axiomatic system that interprets Q.
6. Godel's First Incompleteness Theorem applies to the HC.

The document also claims the HC has a "full axiomatic system" with deductive apparatus (sequent calculus), rules of inference, material inferences, and grounded semantics.

---

## 2. What the Prolog Code Actually Does

### 2.1 What exists and works

**Zero.** `is_recollection(0, [axiom(zero)])` exists in `incompatibility_semantics.pl`. This is a legitimate distinguished zero element. CONFIRMED.

**Successor.** `is_recollection(N, History)` for `N > 0` constructs N by computing `(N-1) + 1` via the hermeneutic calculator. This is a real successor construction with a computational trace. CONFIRMED.

**Grounded arithmetic.** `grounded_arithmetic.pl` defines:
- `zero/1` as `recollection([])`
- `successor/2` as prepending a tally mark
- `add_grounded/3` as list concatenation (append)
- `multiply_grounded/3` as repeated addition via predecessor recursion
- `subtract_grounded/3` as list difference
- `divide_grounded/3` as repeated subtraction

These are real implementations over a tally-mark representation. They compute correct results. Tests pass (8 = 5+3, 15 = 5*3, etc.).

**Strategy FSMs.** COBO addition and C2C multiplication are genuine finite state machines that model children's arithmetic strategies. They compute correct results. 19/19 strategies pass tests as of October 2025.

**Sequent calculus.** `proves/1` and `proves_impl/2` implement a real sequent prover with identity, explosion, negation rules, conjunction rules, modal logic rules, and forward chaining. This is actual working code.

**Addition in the axiomatic layer.** This rule exists:

```prolog
proves_impl(_ => [o(plus(A,B,C))], _) :-
    is_recollection(A, _), is_recollection(B, _),
    arith_op(A, B, +, C),
    is_recollection(C, _).
```

This lets the prover derive `o(plus(7, 5, 12))` as a theorem. CONFIRMED.

**Commutativity of addition.** This axiom exists:

```prolog
proves_impl([n(plus(A,B,C))] => [n(plus(B,A,C))], _).
```

CONFIRMED (though this is a schematic axiom, not derived from more basic principles).

### 2.2 What is missing or wrong

**MULTIPLICATION HAS NO `proves_impl` RULE.**

I searched the entire `incompatibility_semantics.pl` file. There is no:
- `proves_impl(_ => [o(mult(A,B,C))], _)`
- `proves_impl(_ => [o(times(A,B,C))], _)`
- `proves_impl(_ => [o(product(A,B,C))], _)`

The only multiplication-adjacent rule is `iterate(U, M, R)` in the fraction grounding section, which computes `R = U * M` but is not part of the arithmetic axiom set. It uses `is` (Prolog's built-in arithmetic), not the grounded multiply.

The markdown says the multiplication axiom is "implicit in the grounded arithmetic layer." This is an honest admission buried in a document that otherwise presents the interpretation as complete. An implicit axiom is not an axiom. The proves_impl system has addition; it does not have multiplication.

**ROBINSON AXIOMS Q1-Q3 ARE ENTIRELY ABSENT.**

Robinson Arithmetic requires three structural axioms about successor:
1. `S(x) != 0` (zero is not a successor)
2. `S(x) = S(y) -> x = y` (successor is injective)
3. `x = 0 v exists y (x = S(y))` (every number is zero or a successor)

None of these appear anywhere in the codebase. Not as `proves_impl` rules. Not as test assertions. Not as Prolog predicates. The word "injective" appears zero times. The concept "zero is not a successor" appears zero times.

These axioms are trivially true of the implementation (tally-mark lists have these properties by construction), but they are not formalized as theorems of the system. The HC cannot prove them; it merely happens to satisfy them at the meta-level.

**ROBINSON AXIOMS Q4-Q7 (RECURSIVE DEFINITIONS) ARE PARTIALLY PRESENT.**

- Q4: `x + 0 = x` -- Not stated as an axiom. True of `add_grounded` (appending empty list) but not a proves_impl theorem.
- Q5: `x + S(y) = S(x + y)` -- Not stated as an axiom. True of `add_grounded` (append associativity) but not a proves_impl theorem.
- Q6: `x * 0 = 0` -- Not stated as an axiom. True of `multiply_grounded` (base case) but not a proves_impl theorem.
- Q7: `x * S(y) = (x * y) + x` -- Not stated as an axiom. True of `multiply_grounded` (recursive case) but not a proves_impl theorem.

The grounded_arithmetic.pl code SATISFIES these axioms in the same way that any correct implementation of arithmetic satisfies them. But satisfying axioms in an implementation is not the same as interpreting them in a formal system.

**NO UNIVERSALLY QUANTIFIED STATEMENTS.**

The markdown notes (correctly) that Robinson Arithmetic does not require induction and that Q only requires instances, not universally quantified theorems. But it then fails to produce even the instances. There are no proves_impl rules that correspond to Q1-Q7 even for specific numbers.

**THE TESTS DO NOT TEST WHAT THE MARKDOWN CLAIMS.**

`test_arithmetic_ops.pl` tests:
- That COBO computes 7+5=12 and 23+17=40
- That C2C computes 3*4=12 and 5*7=35
- That grounded arithmetic computes 5+3=8, 5*3=15, 5-3=2, 15/3=5

These are tests of computational correctness. They are NOT tests of axiomatic derivability. No test calls `proves/1` with an arithmetic sequent. No test verifies that the axiomatic layer can derive arithmetic facts. No test checks Robinson axioms.

---

## 3. Gap Analysis: Claims vs. Evidence

| Component | Claimed | Status | Gap |
|-----------|---------|--------|-----|
| Zero element | HC defines 0 | PRESENT | None |
| Successor | HC defines S(x) | PRESENT | None |
| Addition (computation) | HC computes addition | PRESENT | None |
| Addition (axiom) | proves_impl derives plus facts | PRESENT | None |
| Multiplication (computation) | HC computes multiplication | PRESENT | None |
| Multiplication (axiom) | proves_impl derives mult facts | **ABSENT** | Critical |
| Q1: S(x) != 0 | "Met" | **ABSENT** | No formalization |
| Q2: S injective | "Met" | **ABSENT** | No formalization |
| Q3: x=0 or x=S(y) | "Met" | **ABSENT** | No formalization |
| Q4: x+0=x | Implicit | **ABSENT** | Not a theorem |
| Q5: x+S(y)=S(x+y) | Implicit | **ABSENT** | Not a theorem |
| Q6: x*0=0 | Implicit | **ABSENT** | Not a theorem |
| Q7: x*S(y)=(x*y)+x | Implicit | **ABSENT** | Not a theorem |
| Deductive apparatus | "Full axiomatic system" | PRESENT | Real but irrelevant to Q |
| Tests of Q | Tests verify Q axioms | **ABSENT** | Tests only check computation |

---

## 4. Verdict: NOT LEGITIMATE

The claim that the HC "interprets Robinson Arithmetic" is not supported by the code.

What the HC actually has:
- A correct computational implementation of addition and multiplication over tally-mark representations
- A real sequent calculus prover that can derive addition facts as theorems
- Godel numbering machinery that can encode its own states

What the HC does not have:
- Any formalization of the seven Robinson axioms as theorems or axiom schemas
- A proves_impl rule for multiplication
- Any test that verifies the Robinson axiom correspondence
- Any formal notion of "interpretation" (in the model-theoretic sense: a mapping from Q's language to the HC's language that preserves theoremhood)

**This is not hand-waving, but it is a real gap.** The markdown reads like it was produced by an AI that understood the structure of the argument but did not verify each step against the code. The checkmarks (the repeated "Q Requirement Met") are assertions, not verifications.

The deeper problem: the markdown conflates "the HC's tally-mark arithmetic happens to satisfy the Robinson axioms" (which is trivially true of any correct arithmetic implementation) with "the HC formally interprets Q" (which requires the axioms to be theorems of the HC's deductive system). A Python calculator also satisfies the Robinson axioms. That does not make Python a formal system that interprets Q.

---

## 5. What Would Need to Happen to Close the Gaps

The good news: the infrastructure is almost entirely in place. The missing pieces are small and well-defined.

### 5.1 Add multiplication to the axiomatic layer (30 minutes)

Add to `incompatibility_semantics.pl`:

```prolog
proves_impl(_ => [o(mult(A,B,C))], _) :-
    is_recollection(A, _), is_recollection(B, _),
    integer(A), integer(B),
    C is A * B,
    is_recollection(C, _).
```

Or better, route through grounded arithmetic:

```prolog
proves_impl(_ => [o(mult(A,B,C))], _) :-
    is_recollection(A, _), is_recollection(B, _),
    integer_to_recollection(A, RecA),
    integer_to_recollection(B, RecB),
    multiply_grounded(RecA, RecB, RecC),
    recollection_to_integer(RecC, C),
    is_recollection(C, _).
```

### 5.2 Formalize Robinson axioms as proves_impl rules (1-2 hours)

Each Robinson axiom should be expressible as a proves_impl fact or rule:

**Q1** (S(x) != 0): Could be expressed via incoherence:
```prolog
is_incoherent([o(eq(succ(X), 0))]) :- is_recollection(X, _).
```

**Q2** (S injective): 
```prolog
proves_impl([o(eq(succ(X), succ(Y)))] => [o(eq(X, Y))], _) :-
    is_recollection(X, _), is_recollection(Y, _).
```

**Q3** (every number is 0 or a successor): Already implicit in `is_recollection/2`'s recursive structure. Could be made explicit as a derivable fact.

**Q4-Q7** (recursive definitions of + and *): These could be formalized as derivable facts for any concrete instances:
```prolog
proves_impl(_ => [o(eq(plus(X, 0), X))], _) :- is_recollection(X, _).
proves_impl(_ => [o(eq(plus(X, succ(Y)), succ(plus(X, Y))))], _) :-
    is_recollection(X, _), is_recollection(Y, _).
% etc.
```

### 5.3 Write Robinson-specific tests (1 hour)

A test file `test_robinson_axioms.pl` that:
- For each axiom, verifies it holds for specific instances using `proves/1`
- Checks both computational correctness and axiomatic derivability
- Documents the mapping explicitly

### 5.4 Define the interpretation mapping explicitly (1 hour)

A document or Prolog module that states:
- Q's 0 maps to HC's `0` (via `is_recollection(0, [axiom(zero)])`)
- Q's S maps to HC's successor construction
- Q's + maps to HC's `plus/3` in the proves_impl layer
- Q's * maps to HC's `mult/3` in the proves_impl layer
- Each Q axiom maps to a specific proves_impl derivation

### Total estimated work: 3-5 hours of Prolog coding and testing.

The point is that the gap is a gap of formalization, not a gap of substance. The tally-mark arithmetic genuinely does satisfy Robinson's axioms. The work needed is to make that satisfaction explicit in the deductive layer rather than implicit in the implementation.

---

## 6. What the Book Should Say

### What NOT to say

Do not say: "The Hermeneutic Calculator interprets Robinson Arithmetic."

This claim, as stated in the markdown, is not currently supported. Putting it in a Routledge book would expose you to legitimate criticism from any logician who reads the Prolog.

### What you CAN say (honestly)

**Version A (strong, after doing the work in Section 5):**

> The Hermeneutic Calculator's tally-mark arithmetic, formalized in Prolog as a sequent calculus with grounded operations, satisfies the axioms of Robinson Arithmetic. Zero is axiomatically grounded. Successor is constructed via the hermeneutic calculator's strategy automata. Addition and multiplication are defined over recollection structures (tally-mark lists) and can be derived as theorems in the system's incompatibility semantics. The seven Robinson axioms hold as derivable facts for all concrete instances. Since Robinson Arithmetic is the minimal system to which Godel's First Incompleteness Theorem applies, this establishes that any consistent extension of the Hermeneutic Calculator is incomplete.

This is defensible IF you do the work in Section 5 first.

**Version B (current state, no additional work):**

> The Hermeneutic Calculator implements arithmetic operations -- addition via the COBO strategy, multiplication via the C2C strategy -- grounded in tally-mark representations rather than abstract number objects. These operations are computationally correct and verified by test. The system also includes a sequent calculus (incompatibility semantics) capable of deriving arithmetic facts as theorems. The grounded arithmetic layer satisfies the axioms of Robinson Arithmetic at the implementation level: zero is distinguished, successor is injective, and the recursive definitions of addition and multiplication hold by construction. A fuller formalization -- expressing the Robinson axioms as explicit theorems of the sequent calculus -- remains to be completed.

This is honest, defensible, and does not overclaim. It also sets up the Godel discussion without making the interpretation claim that hasn't been earned yet.

**Version C (for the Bridge chapter's philosophical argument):**

> What matters is not the formal claim that the Hermeneutic Calculator "is" a model of Robinson Arithmetic -- though the correspondence can be made precise. What matters is that the arithmetic strategies children invent, when formalized as finite state machines operating over tally-mark representations, already possess the structural complexity that Godel's theorem requires. The operations are not abstract: they are traces of counting acts, decompositions into tens and ones, coordinations of two simultaneous counts. And yet this embodied complexity suffices. The incompleteness result does not require a system designed for incompleteness. It finds any system that can count.

This version makes the philosophical point without depending on a formal claim that hasn't been fully earned. It is stronger prose and more honest mathematics.

---

## 7. Provenance Assessment

**Was this AI-generated hand-waving?**

Partially. The `ROBINSON_ARITHMETIC_INTERPRETATION.md` file has the signature pattern of AI-assisted mathematical writing:
- Correct high-level structure (the seven axioms, the interpretation schema)
- Correct identification of relevant code locations
- Green checkmarks on every item without actually verifying them
- A critical gap (no multiplication proves_impl) papered over with "implicit in the grounded arithmetic layer"
- No tests to back the claims
- Rhetorically confident where mathematically uncertain

But the underlying Prolog is NOT hand-waving. The strategy automata are real. The grounded arithmetic is mathematically sound. The sequent calculus works. The Godel numbering is correct. The AI produced a document that overstated the completeness of work that was genuinely underway but not finished.

The `SYNTHESIS_HONESTY.md` and `SYSTEM_ASSESSMENT.md` in the formalization repo show that Tio (or a more careful Claude session) already knows the difference between claims and implementations. The Robinson markdown is at a lower standard of honesty than those documents.

**Bottom line:** The mathematical substance is real. The formal claim is premature. Closing the gap requires hours, not months. The book should either do the work or state the claim carefully.
