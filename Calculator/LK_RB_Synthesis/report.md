# Algorithmic Elaboration Analysis Report

Generated on: 2025-09-15 16:00:38

## Overview

- **Computational Patterns Detected**: 2
- **Algorithmic Elaborations Found**: 16
- **MUD Diagrams Generated**: 8

## Computational Patterns

| Pattern | Type | Usage Count | Strategies |
|---------|------|-------------|------------|
| base_decomposition | decomposition | 4 | ADD_Rounding, SUB_Rounding, SMR_DIV_CGOB... |
| incremental_counting | counting | 5 | ADD_COBO, SMR_MULT_C2C, ADD_Counting... |

## Key Algorithmic Elaborations

| Base Strategy | Elaborated Strategy | Shared Patterns | Confidence |
|---------------|---------------------|----------------|------------|
| ADD_Rounding | SUB_Rounding | base_decomposition | 1.00 |
| ADD_Rounding | SMR_DIV_CGOB | base_decomposition | 1.00 |
| ADD_Rounding | SMR_DIV_DealingByOnes | base_decomposition | 1.00 |
| ADD_Counting | ADD_Chunking | incremental_counting | 1.00 |
| ADD_Counting | ADD_COBO | incremental_counting | 1.00 |
| ADD_Counting | COBO | incremental_counting | 1.00 |
| ADD_Counting | SMR_MULT_C2C | incremental_counting | 1.00 |
| ADD_Chunking | ADD_COBO | incremental_counting | 1.00 |
| ADD_Chunking | COBO | incremental_counting | 1.00 |
| ADD_Chunking | SMR_MULT_C2C | incremental_counting | 1.00 |

## Meaning-Use Diagrams

The following diagrams illustrate the algorithmic elaborations detected in the analysis. Each diagram shows strategies connected by shared computational patterns.

### Addition To Subtraction

**Operation:** Addition To Subtraction
**Strategies Analyzed:** 2
**Elaborations Detected:** 1

#### Strategies:
- ADD_Rounding
- SUB_Rounding

#### Key Elaborations:
- **ADD_Rounding** → **SUB_Rounding**
  - Shared patterns: base_decomposition
  - Confidence: 1.00

#### TikZ Diagram Code:

```latex
\begin{tikzpicture}[node distance=2cm and 2cm, auto]
\node[draw, fill=blue!10, rounded corners] (title) at (0,0) {\textbf{Addition To Subtraction MUD}};

\node[draw, fill=green!10, rounded corners] (strategy_0) at (0.00,0.00) {ADD Rounding};
\node[draw, fill=green!10, rounded corners] (strategy_1) at (12.57,9.42) {SUB Rounding};

\draw[red, ->, thick] (strategy_0) -- (strategy_1)
    node[midway, above] {\scriptsize base_decomposition};

\end{tikzpicture}
```

---
### Addition To Division

**Operation:** Addition To Division
**Strategies Analyzed:** 3
**Elaborations Detected:** 2

#### Strategies:
- ADD_Rounding
- SMR_DIV_DealingByOnes
- SMR_DIV_CGOB

#### Key Elaborations:
- **ADD_Rounding** → **SMR_DIV_CGOB**
  - Shared patterns: base_decomposition
  - Confidence: 1.00
- **ADD_Rounding** → **SMR_DIV_DealingByOnes**
  - Shared patterns: base_decomposition
  - Confidence: 1.00

#### TikZ Diagram Code:

```latex
\begin{tikzpicture}[node distance=2cm and 2cm, auto]
\node[draw, fill=blue!10, rounded corners] (title) at (0,0) {\textbf{Addition To Division MUD}};

\node[draw, fill=green!10, rounded corners] (strategy_0) at (0.00,0.00) {ADD Rounding};
\node[draw, fill=green!10, rounded corners] (strategy_1) at (8.38,6.28) {SMR DIV DealingByOnes};
\node[draw, fill=green!10, rounded corners] (strategy_2) at (16.76,12.57) {SMR DIV CGOB};

\draw[red, ->, thick] (strategy_0) -- (strategy_2)
    node[midway, above] {\scriptsize base_decomposition};
\draw[red, ->, thick] (strategy_0) -- (strategy_1)
    node[midway, above] {\scriptsize base_decomposition};

\end{tikzpicture}
```

---
### Addition

**Operation:** Addition
**Strategies Analyzed:** 3
**Elaborations Detected:** 3

#### Strategies:
- ADD_COBO
- ADD_Counting
- ADD_Chunking

#### Key Elaborations:
- **ADD_Counting** → **ADD_Chunking**
  - Shared patterns: incremental_counting
  - Confidence: 1.00
- **ADD_Counting** → **ADD_COBO**
  - Shared patterns: incremental_counting
  - Confidence: 1.00
- **ADD_Chunking** → **ADD_COBO**
  - Shared patterns: incremental_counting
  - Confidence: 1.00

#### TikZ Diagram Code:

```latex
\begin{tikzpicture}[node distance=2cm and 2cm, auto]
\node[draw, fill=blue!10, rounded corners] (title) at (0,0) {\textbf{Addition MUD}};

\node[draw, fill=green!10, rounded corners] (strategy_0) at (0.00,0.00) {ADD COBO};
\node[draw, fill=green!10, rounded corners] (strategy_1) at (8.38,6.28) {ADD Counting};
\node[draw, fill=green!10, rounded corners] (strategy_2) at (16.76,12.57) {ADD Chunking};

\draw[blue, ->, thick] (strategy_1) -- (strategy_2)
    node[midway, above] {\scriptsize incremental_counting};
\draw[blue, ->, thick] (strategy_1) -- (strategy_0)
    node[midway, above] {\scriptsize incremental_counting};
\draw[blue, ->, thick] (strategy_2) -- (strategy_0)
    node[midway, above] {\scriptsize incremental_counting};

\end{tikzpicture}
```

---
### Addition To General

**Operation:** Addition To General
**Strategies Analyzed:** 4
**Elaborations Detected:** 3

#### Strategies:
- ADD_COBO
- ADD_Counting
- ADD_Chunking
- COBO

#### Key Elaborations:
- **ADD_Counting** → **COBO**
  - Shared patterns: incremental_counting
  - Confidence: 1.00
- **ADD_Chunking** → **COBO**
  - Shared patterns: incremental_counting
  - Confidence: 1.00
- **ADD_COBO** → **COBO**
  - Shared patterns: incremental_counting
  - Confidence: 1.00

#### TikZ Diagram Code:

```latex
\begin{tikzpicture}[node distance=2cm and 2cm, auto]
\node[draw, fill=blue!10, rounded corners] (title) at (0,0) {\textbf{Addition To General MUD}};

\node[draw, fill=green!10, rounded corners] (strategy_0) at (0.00,0.00) {ADD COBO};
\node[draw, fill=green!10, rounded corners] (strategy_1) at (6.28,4.71) {ADD Counting};
\node[draw, fill=green!10, rounded corners] (strategy_2) at (12.57,9.42) {ADD Chunking};
\node[draw, fill=green!10, rounded corners] (strategy_3) at (18.85,14.14) {COBO};

\draw[blue, ->, thick] (strategy_1) -- (strategy_3)
    node[midway, above] {\scriptsize incremental_counting};
\draw[blue, ->, thick] (strategy_2) -- (strategy_3)
    node[midway, above] {\scriptsize incremental_counting};
\draw[blue, ->, thick] (strategy_0) -- (strategy_3)
    node[midway, above] {\scriptsize incremental_counting};

\end{tikzpicture}
```

---
### Addition To Multiplication

**Operation:** Addition To Multiplication
**Strategies Analyzed:** 4
**Elaborations Detected:** 3

#### Strategies:
- ADD_COBO
- SMR_MULT_C2C
- ADD_Counting
- ADD_Chunking

#### Key Elaborations:
- **ADD_Counting** → **SMR_MULT_C2C**
  - Shared patterns: incremental_counting
  - Confidence: 1.00
- **ADD_Chunking** → **SMR_MULT_C2C**
  - Shared patterns: incremental_counting
  - Confidence: 1.00
- **ADD_COBO** → **SMR_MULT_C2C**
  - Shared patterns: incremental_counting
  - Confidence: 1.00

#### TikZ Diagram Code:

```latex
\begin{tikzpicture}[node distance=2cm and 2cm, auto]
\node[draw, fill=blue!10, rounded corners] (title) at (0,0) {\textbf{Addition To Multiplication MUD}};

\node[draw, fill=green!10, rounded corners] (strategy_0) at (0.00,0.00) {ADD COBO};
\node[draw, fill=green!10, rounded corners] (strategy_1) at (6.28,4.71) {SMR MULT C2C};
\node[draw, fill=green!10, rounded corners] (strategy_2) at (12.57,9.42) {ADD Counting};
\node[draw, fill=green!10, rounded corners] (strategy_3) at (18.85,14.14) {ADD Chunking};

\draw[blue, ->, thick] (strategy_2) -- (strategy_1)
    node[midway, above] {\scriptsize incremental_counting};
\draw[blue, ->, thick] (strategy_3) -- (strategy_1)
    node[midway, above] {\scriptsize incremental_counting};
\draw[blue, ->, thick] (strategy_0) -- (strategy_1)
    node[midway, above] {\scriptsize incremental_counting};

\end{tikzpicture}
```

---
### General To Multiplication

**Operation:** General To Multiplication
**Strategies Analyzed:** 2
**Elaborations Detected:** 1

#### Strategies:
- SMR_MULT_C2C
- COBO

#### Key Elaborations:
- **COBO** → **SMR_MULT_C2C**
  - Shared patterns: incremental_counting
  - Confidence: 1.00

#### TikZ Diagram Code:

```latex
\begin{tikzpicture}[node distance=2cm and 2cm, auto]
\node[draw, fill=blue!10, rounded corners] (title) at (0,0) {\textbf{General To Multiplication MUD}};

\node[draw, fill=green!10, rounded corners] (strategy_0) at (0.00,0.00) {SMR MULT C2C};
\node[draw, fill=green!10, rounded corners] (strategy_1) at (12.57,9.42) {COBO};

\draw[blue, ->, thick] (strategy_1) -- (strategy_0)
    node[midway, above] {\scriptsize incremental_counting};

\end{tikzpicture}
```

---
### Subtraction To Division

**Operation:** Subtraction To Division
**Strategies Analyzed:** 3
**Elaborations Detected:** 2

#### Strategies:
- SUB_Rounding
- SMR_DIV_DealingByOnes
- SMR_DIV_CGOB

#### Key Elaborations:
- **SUB_Rounding** → **SMR_DIV_CGOB**
  - Shared patterns: base_decomposition
  - Confidence: 1.00
- **SUB_Rounding** → **SMR_DIV_DealingByOnes**
  - Shared patterns: base_decomposition
  - Confidence: 1.00

#### TikZ Diagram Code:

```latex
\begin{tikzpicture}[node distance=2cm and 2cm, auto]
\node[draw, fill=blue!10, rounded corners] (title) at (0,0) {\textbf{Subtraction To Division MUD}};

\node[draw, fill=green!10, rounded corners] (strategy_0) at (0.00,0.00) {SUB Rounding};
\node[draw, fill=green!10, rounded corners] (strategy_1) at (8.38,6.28) {SMR DIV DealingByOnes};
\node[draw, fill=green!10, rounded corners] (strategy_2) at (16.76,12.57) {SMR DIV CGOB};

\draw[red, ->, thick] (strategy_0) -- (strategy_2)
    node[midway, above] {\scriptsize base_decomposition};
\draw[red, ->, thick] (strategy_0) -- (strategy_1)
    node[midway, above] {\scriptsize base_decomposition};

\end{tikzpicture}
```

---
### Division

**Operation:** Division
**Strategies Analyzed:** 2
**Elaborations Detected:** 1

#### Strategies:
- SMR_DIV_DealingByOnes
- SMR_DIV_CGOB

#### Key Elaborations:
- **SMR_DIV_CGOB** → **SMR_DIV_DealingByOnes**
  - Shared patterns: base_decomposition
  - Confidence: 1.00

#### TikZ Diagram Code:

```latex
\begin{tikzpicture}[node distance=2cm and 2cm, auto]
\node[draw, fill=blue!10, rounded corners] (title) at (0,0) {\textbf{Division MUD}};

\node[draw, fill=green!10, rounded corners] (strategy_0) at (0.00,0.00) {SMR DIV DealingByOnes};
\node[draw, fill=green!10, rounded corners] (strategy_1) at (12.57,9.42) {SMR DIV CGOB};

\draw[red, ->, thick] (strategy_1) -- (strategy_0)
    node[midway, above] {\scriptsize base_decomposition};

\end{tikzpicture}
```

---