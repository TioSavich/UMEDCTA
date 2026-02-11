# The Cognitive Calculator: Teacher's Edition

## Overview
This interactive simulation is designed for pre-service teachers to practice diagnosing and guiding student mathematical thinking. It uses the underlying computational models (Python scripts) from the UMEDCTA project to simulate student strategies.

## How to Play
Run the game from the terminal:
```bash
python3 strategy_game.py
```

## Modules

### 1. The Robot Counter (Algorithmic Thinking)
**Concept:** Place Value & Stack Operations.
**Goal:** Predict the state of a Deterministic Pushdown Automaton (DPDA) that counts.
**Pedagogical Value:** Understanding that counting is an algorithmic process involving state changes (carries/borrows) rather than just "knowing" the next number.

### 2. Sarah's Addition (Rearranging to Make Bases)
**Concept:** Making 10 (or other bases).
**Goal:** Guide the student "Sarah" to decompose the second addend to fill the gap to the next base multiple for the first addend.
**Strategy:** $A + B \rightarrow A + (K + R) \rightarrow (A+K) + R \rightarrow \text{Base} + R$

### 3. Sam's Subtraction (Sliding / Constant Difference)
**Concept:** Invariance of difference.
**Goal:** Adjust both the minuend and subtrahend by the same amount ($K$) so that the subtrahend becomes a friendly base number.
**Strategy:** $M - S \rightarrow (M+K) - (S+K) \rightarrow M' - \text{Base}$

## Technical Note
This game imports the logic directly from the `Calculator/Python_Tests` directory, ensuring that the gameplay is faithful to the project's theoretical models.
