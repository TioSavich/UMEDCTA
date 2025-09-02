# Hermeneutic Calculator: Strategy Formalizations (Clean Draft)

This draft reorganizes the strategies as primary sections. Each section supplies:

1. Phenomenological description (student-facing practice).
2. Formal automaton / register-machine specification in LaTeX-friendly notation.
3. Core choreography (temporal compression/decompression dynamics).
4. Algorithmic elaboration lineage (what primitives it builds upon).

All implementation details (Python prototypes) and historical critiques have been removed. Mathematical symbols are formatted for Pandoc → LaTeX conversion.

Notation (uniform across strategies):

- $M = (Q, V, \delta, q_0, F)$: machine with states $Q$, registers (or variables) $V$, transition function $\delta$, start state $q_0$, accepting states $F$.
- When convenient, we use auxiliary internal variables; these are included in $V$ implicitly.
- Counting primitives: Count Up (+1), Count Back (−1) regarded as atomic embodied actions.
- Temporal Compression: synthesizing many unit actions into a higher-order unit (e.g., a “ten”).
- Temporal Decompression: strategic expansion of a unit into constituent parts.

---

# Counting and Counting On

**Description.** Sequential unit counting within a bounded base-10 place-value structure (0–999). Embodied iterations (“ticks”) increment units, propagate carries (sublation) into tens and hundreds.

**Formal Model (Sketch).** Deterministic PDA (bounded) or 3-register counter. For LaTeX exposition we specify a DPDA tuple:

\[
M_{count} = (Q, \Sigma, \Gamma, \delta, q_{start}, Z_0, F)\
\]
with place-value stack symbols $U_i, T_j, H_k$. (Full transition table omitted here for brevity; each ripple carry is modeled via $\varepsilon$-moves.)

**Choreography.** Carry = temporal compression: ten unit steps recollected as one higher unit. Borrow (in inverse counting) is temporal decompression.

**Elaboration Lineage.** Primitive for all subsequent additive, subtractive, multiplicative, and divisional strategies.

---

# Rearranging to Make Bases (RMB)

**Description.** For $A + B$, identify gap $K$ from $A$ to next base (e.g., 10, 100), decompose $B = K + R$, form $A' = A + K$ (a base), then compute $A' + R$.

**Machine.**
\[
M_{RMB} = (Q, V, \delta, q_0, F)\
\]
with
$Q = \{q_{start}, q_{calcK}, q_{decompose}, q_{recombine}, q_{accept}\}$,
$V = \{A, B, K, A', R\}$.

Key transitions:
1. $q_{calcK}$: Count Up from $A$ to next base; increment $K$.
2. $q_{decompose}$: Count Down $K$ from $B$ to find remainder $R$.
3. $q_{recombine}$: Output $A' + R$.

**Choreography.** Decompression (splitting $B$) enables immediate compression (forming base $A'$).

**Lineage.** Elaborates Counting Up + Counting Down primitives; anticipates strategic boundary manipulation used later in Rounding, Chunking, Sliding.

---

# COBO (Counting On by Bases then Ones)

**Description.** For $A + B$, decompose $B = b\cdot Base + r$; iterate base jumps (+Base) then unit steps (+1).

**Machine.** $M_{COBO}$ with states $\{q_{start}, q_{bases}, q_{ones}, q_{accept}\}$ and registers $\{Sum, BaseCounter, OneCounter\}$.

Loop invariants:
- Base loop: while $BaseCounter > 0$, $Sum \leftarrow Sum + Base$.
- Ones loop: while $OneCounter > 0$, $Sum \leftarrow Sum + 1$.

**Choreography.** Two-phase rhythm: compressed temporal blocks (bases) followed by decompressed fine resolution (ones).

**Lineage.** Builds on counting; prepares for Chunking and Rounding by habitualizing base jumps.

---

# Rounding and Adjusting (Addition)

**Description.** Select addend closer to next base: round up $A \to A' = A + K$, compute $A' + B$, then adjust back: $(A' + B) - K$.

**Machine.** States $\{q_{start}, q_{calcK}, q_{add}, q_{adjust}, q_{accept}\}$; registers $\{A,B,K,A',Temp,Result\}$.

**Core Transitions.**
1. Determine $K$ by Count Up from $A$ to next base.
2. COBO subroutine for $A'+B$ (fast due to base).
3. Count Back $K$ to compensate.

**Choreography.** Strategic temporal detour: initial decompression (deriving $K$) enables major compression (base addition), followed by inverse correction.

**Lineage.** Elaborates RMB (boundary anticipation) and COBO (base efficiency); introduces explicit compensation schema.

---

# Chunking (Addition)

**Description.** Decompose $B$ into large base chunk + strategic residual chunks to force successive bases: $B = B_{base} + K + R$ where $K$ bridges current sum to next base.

**Machine.** States $\{q_{init}, q_{addBase}, q_{calcK}, q_{applyK}, q_{finishR}, q_{accept}\}$. Registers $\{Sum, BasesRem, OnesRem, K\}$.

**Choreography.** Iterative cycle: (1) large compression via aggregated base, (2) micro decompression to find $K$, (3) re-compression to new base, (4) terminal residue.

**Lineage.** Synthesizes COBO (bulk bases) + RMB (strategic gap finding).

---

# Subtraction Chunking (Three Orientations)

Given $M - S = D$.

**A. Backwards by Part (Take-Away).** Sequentially subtract decomposed parts of $S$ (place value or strategic chunks) from $M$.

**B. Forwards from Part (Missing Addend).** Treat as $S + D = M$; Count Up (RMB logic) accumulating $D$.

**C. Backwards to Part (Distance Down To).** Count Back from $M$ toward $S$ using strategic base landings; accumulate distance.

Each orientation can be formalized as a register machine with states reflecting loop (chunk identification / application) and termination when target reached.

**Choreography.** Orientation selects temporal direction; strategies B and C exploit boundary compression via RMB subroutines.

**Lineage.** Inversions and adaptations of Addition Chunking + RMB.

---

# Subtraction COBO / CBBO

**COBO (Missing Addend).** Start at $S$, perform base jumps toward $M$ (without overshoot), then ones; distance accumulated is $D$.

**CBBO (Counting Back).** Start at $M$, subtract base units (from decomposed $S$) then ones; final position is $D$.

**Machines.** Two dual loop structures differing only in iteration direction and accumulator semantics.

**Choreography.** Directional inversion of the same two-phase rhythm (bases → ones). Overshoot detection acts as control boundary in COBO.

**Lineage.** Direct inversion (CBBO) and repurposed forward algorithm (COBO) from addition.

---

# Subtraction Decomposition (Borrowing)

**Description.** Left-to-right: subtract higher place (tens), detect insufficiency in lower place, decompose (borrow) one higher unit into base smaller units, then subtract ones.

**Machine.** $Q = \{q_{init}, q_{subBases}, q_{checkOnes}, q_{decompose}, q_{subOnes}, q_{accept}\}$ with registers for place values $R_T, R_O, S_T, S_O$.

**Choreography.** Inversion of sublation: temporal decompression of a ten into ten ones to restore operability.

**Lineage.** Builds on internalized carry (from counting) now executed in reverse.

---

# Subtraction Rounding and Adjusting

**Description.** Dual rounding (e.g., $M \to M'$ down, $S \to S'$ down) yields simplified $M' - S'$, then contrasting compensations: add $K_M$, subtract $K_S$.

**Machine.** States $\{q_{roundM}, q_{roundS}, q_{subtract}, q_{adjustM}, q_{adjustS}, q_{accept}\}$; registers $\{M,S,K_M,K_S,Temp,Result\}$.

**Choreography.** Opposed adjustments highlight subtraction asymmetry: modification of minuend vs. subtrahend impacts result in inverse directions.

**Lineage.** Integrates rounding (addition strategy) and inverse compensation sequencing.

---

# Subtraction Sliding (Constant Difference)

**Description.** Find $K$ so that $S + K$ is a base (or friendly) number; compute $(M + K) - (S + K)$ exploiting invariance: $M - S = (M+K) - (S+K)$.

**Machine.** States $\{q_{calcK}, q_{slide}, q_{subtract}, q_{accept}\}$; registers $\{M,S,K,M',S'\}$.

**Choreography.** Up-front decompression (deriving $K$) enables single compressed subtraction against a base-aligned subtrahend.

**Lineage.** Extends RMB gap-finding; anticipates relational “distance” framing central to subtraction fluency.

---

# Commutative Reasoning (Multiplication Optimization)

**Description.** For $A \times B$, evaluate heuristic difficulty of $(A,B)$ vs $(B,A)$; select orientation minimizing cognitive load (iteration count & skip difficulty), then perform iterative addition (skip counting).

**Machine.** $Q = \{q_{evaluate}, q_{repackage}, q_{calc}, q_{accept}\}$; registers $\{A,B,Groups,Items,Total,Counter\}$ plus heuristic function $H$.

**Choreography.** Meta-level selection precedes execution; commutative symmetry exploited for temporal compression.

**Lineage.** Builds on C2C / Skip Counting; introduces optimization layer.

---

# Coordinating Two Counts (C2C)

**Description.** Foundational multiplication: nested counting—items within group, groups within total; total $T = N \cdot S$ emerges from exhaustive unit enumeration.

**Machine.** $Q = \{q_{init}, q_{checkG}, q_{countItems}, q_{nextGroup}, q_{accept}\}$; registers $\{G,I,T,N,S\}$.

**Choreography.** Maximal temporal decompression (no compression yet); establishes structural scaffold for later compression (skip counting, distributive reasoning).

**Lineage.** Direct elaboration of counting primitives into nested loops.

---

# Conversion to Bases and Ones (CBO Multiplication)

**Description.** Redistribute units among groups so that many groups become exact base multiples, leaving a compact residual: $(k \cdot Base) + r$.

**Machine.** Array register $Groups[1..N]$; states manage selection of source, iterative redistribution until targets reach base threshold.

**Choreography.** Proactive sublation: simultaneous decompression (source group) and compression (targets) to manufacture base units early.

**Lineage.** Multiplicative analogue of RMB and addition Chunking with explicit inter-group transfers.

---

# Distributive Reasoning (Multiplication)

**Description.** Decompose $S = S_1 + S_2$ (heuristically “easy” numbers), compute $N S_1$ and $N S_2$ (skip counting or compressed methods), then sum.

**Machine.** States $\{q_{split}, q_{P1}, q_{P2}, q_{sum}, q_{accept}\}$; registers $\{N,S,S_1,S_2,P_1,P_2,Total\}$.

**Choreography.** Temporal decompression (factor split) followed by parallelizable compressed sub-calculations and final recombination.

**Lineage.** Extends skip counting with heuristic structural decomposition; precursor to algebraic distributivity recognition.

---

# Dealing by Ones (Division – Sharing)

**Description.** Partitive division: distribute single units round-robin into $N$ groups until total $T$ exhausted; per-group size $S$ emerges.

**Machine.** $Q = \{q_{init}, q_{deal}, q_{accept}\}$; registers $\{Remaining, Groups[1..N], idx\}$.

**Choreography.** Maximal temporal decompression; rhythmic rounds establish invariant increase pattern (foundation for later compression insights).

**Lineage.** Inversion of C2C perspective (constructing equal groups from total rather than composing total from groups).

---

# Inverse Distributive Reasoning (Division)

**Description.** Measurement division $T / S$: decompose $T$ into known multiples of $S$: $T = \sum_i (m_i S)$; quotient $= \sum_i m_i$.

**Machine.** States $\{q_{search}, q_{apply}, q_{accept}\}$; registers $\{Remaining, TotalQ, (PartialT, PartialQ)\}$; knowledge base of facts $\{(mS, m)\}$ ordered by descending $mS$.

**Choreography.** Temporal compression via retrieval of pre-compressed multiplication facts; loop greedily subtracts largest available chunk.

**Lineage.** Inversion of Distributive Reasoning in multiplication (switch from constructing product to decomposing dividend).

---

# Using Commutative Reasoning (Division via Iterated Accumulation)

**Description.** For $E / G$ (sharing reframed as measurement): iteratively accumulate $G$ until total $E$ reached; iteration count is quotient.

**Machine.** $Q = \{q_{init}, q_{iterate}, q_{check}, q_{accept}\}$; registers $\{T, G, Acc, Q\}$.

**Choreography.** Symmetric inversion of repeated addition (multiplication) focusing on completion criterion instead of fixed loop count.

**Lineage.** Bridges between Dealing by Ones and chunk-based division (fact retrieval).

---

# Conversion to Groups Other than Bases (CGOB Division)

**Description.** Leverage base decomposition of dividend $T$ (e.g., tens & ones) plus analysis of base/divisor relation: $Base = q_1 S + r_1$; process all base units in bulk, aggregate remainders, finalize.

**Machine.** States $\{q_{init}, q_{analyze}, q_{processBases}, q_{combineR}, q_{processR}, q_{accept}\}$; registers $\{T_B, T_O, Q, R, S\}$ with derived $S_{inBase}, R_{inBase}$.

**Choreography.** Dual decompression (dividend by base, base by divisor) → large compression (bulk quotient) → residual resolution.

**Lineage.** Division analogue of CBO (multiplication) and Distributive Reasoning; integrates multi-level structural analysis.

---

# Conceptual Dependency Graph (Narrative)

Counting → (RMB, COBO) → (Chunking, Rounding & Adjusting, Sliding) → (Subtraction Inversions: COBO/CBBO, Chunking orientations, Decomposition, Rounding, Sliding) → (C2C) → (Skip Counting / implicit in COBO Multiplication) → (Commutative & Distributive Reasoning, CBO Multiplication) → (Division primitives: Dealing by Ones, Iterated Accumulation) → (Inverse Distributive Reasoning, Fact-Based Decomposition) → (CGOB Division).

Each arrow denotes an algorithmic elaboration where prior compressed units or reversible decompositions become callable subroutines.

---

# Temporal Dynamics Summary

- **Primitive Decompression:** Counting by ones; Dealing by Ones; C2C (inner loop).
- **First Compression Layer:** COBO (bases as units); subtraction COBO/CBBO; iterative accumulation for division.
- **Strategic Boundary Forcing:** RMB, Chunking, Sliding, Rounding (anticipatory manipulation of base thresholds).
- **Structural Decomposition / Synthesis:** Distributive Reasoning, Inverse Distributive Reasoning, CBO (Multiplication & Division), CGOB.

---

# Glossary of Symbols

- $Base$: Typically 10 (extendable to other positional bases).
- $K$: Gap to next base (RMB, Rounding, Chunking, Sliding).
- $R$: Remainder after decomposition or partial processing.
- $S_1, S_2$: Split components of a factor (Distributive Reasoning).
- $Groups, Items$: Multiplicative roles after commutative optimization.
- $Remaining$: Unprocessed portion of a dividend in division strategies.
- $Q$: Quotient / accumulated result in division; also generic state set symbol context-dependent.
- $Acc$: Accumulated total during iterative division.

---

End of Clean Draft.
