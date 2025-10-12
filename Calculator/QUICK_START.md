# The Hermeneutic Calculator - Quick Start Guide

An interactive system for exploring arithmetic strategies, their formal logic, and mathematical connections.

## Quick Start

Open **[index.html](index.html)** to access the main calculator interface with interactive strategy explorations.

---

## Folder Organization by Audience

This repository serves three distinct audiences:

### 🍎 For Teachers & Educators
**Interactive Strategy Exploration**

- **Start here:** [index.html](index.html) - Main calculator interface
- **Strategy pages:** Click any button to explore individual arithmetic strategies with interactive visualizations
- **[AceofBases/](AceofBases/)** - Explore arithmetic in different number bases (binary, ternary, etc.)
- **PDF Documentation:** Each strategy has a detailed PDF explaining the underlying automaton

**What you'll find:**
- Visual number line demonstrations
- Step-by-step explanations of student thinking
- Interactive tools for classroom exploration

---

### 🔬 For Researchers
**Mathematical Connections Between Strategies**

- **[LK_RB_Synthesis/](LK_RB_Synthesis/)** - Formal analysis connecting strategies to Robinson Arithmetic and Logical Knowledge
- **[Hermeneutic_Calculator.pdf](Hermeneutic_Calculator.pdf)** - Draft manuscript explaining the theoretical framework
- **[Combined_Strategies_and_Automata.pdf](Combined_Strategies_and_Automata.pdf)** - Complete technical documentation

**What you'll find:**
- Formal proofs of strategy equivalences
- Connections to foundational mathematics (Peano Arithmetic, Robinson Arithmetic)
- Analysis of incompatibility semantics and inferential movement

---

### 💻 For Developers & Logicians
**Formal Logic Implementation**

- **[Prolog/](Prolog/)** - Complete Prolog implementation of the arithmetic strategies
  - Incompatibility semantics
  - Meta-interpretation and reflection
  - Neuro-symbolic bridge
  - Self-reorganizing knowledge base
- **[Python_Tests/](Python_Tests/)** - Python implementations and testing harness

**What you'll find:**
- Executable formal logic for all strategies
- Meta-interpreter for strategy composition
- Automated testing and verification
- Neuro-symbolic integration examples

---

## File Structure

### HTML Strategy Files
Individual strategy visualizations following the pattern:
- `SAR_ADD_*.html` - Addition strategies
- `SAR_SUB_*.html` - Subtraction strategies
- `SMR_MULT_*.html` - Multiplication strategies
- `SMR_DIV_*.html` - Division strategies

Each includes:
- Interactive input fields
- Real-time visualization (SVG number lines, etc.)
- Step-by-step textual explanation
- Link to detailed PDF documentation

### PDF Documentation
Comprehensive automaton descriptions for each strategy, showing:
- Formal state transitions
- Mathematical foundations
- Pedagogical context

### Support Files
- `strategy_styles.css` - Unified styling for all strategy pages
- `counting.html` - Foundational counting strategies
- `presentation.html` - IMERS 2025 conference presentation

---

## Strategy Abbreviations

- **COBO** - Counting On by Bases and Ones
- **ABAO** - Add Bases And Ones
- **RMB** - Right Minus Both
- **C2C** - Coordinating Two Counts
- **DR** - Distributive Reasoning
- **IDP** - Inverse of Distributive Property
- **UCR** - Using Coordinated Rounds
- **CGOB** - Conversion to Groups Other than Bases

---

## Philosophy

The Hermeneutic Calculator represents arithmetic strategies as **autonomous automata** that can be:
1. **Executed** - Run interactively by students/teachers
2. **Analyzed** - Studied for mathematical connections by researchers
3. **Formalized** - Implemented in logic by developers

This tri-level structure reflects the project's commitment to making mathematical thinking accessible, rigorous, and executable.

---

## Navigation

- **[index.html](index.html)** - Main calculator interface
- **[README.md](README.md)** - Theoretical background on diagonalization, reflective abstraction, and self-modifying systems
- **This file (QUICK_START.md)** - Practical navigation guide

---

## Questions?

- **For pedagogy:** Explore the interactive HTML files
- **For theory:** Read the PDFs and LK_RB_Synthesis documentation
- **For implementation:** Dive into the Prolog folder

Each layer builds on the others, creating a coherent system from classroom practice to formal foundations.
