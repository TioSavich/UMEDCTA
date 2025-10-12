# Algorithmic Elaboration Discovery System

## Automated Pattern Analysis for Student Arithmetic Strategies

This project implements a system for **automatically discovering algorithmic elaborations** between student arithmetic strategies. The analyzer examines Python automaton implementations to identify shared computational patterns and generate Meaning-Use Diagrams (MUDs).

**What this system does:** Analyzes student-invented arithmetic strategies to discover how they share computational patterns and build upon each other.

**Theoretical foundation:** Inspired by Robert Brandom's Meaning-Use Analysis (MUA) and George Lakoff's embodied mathematics, but implemented as a practical pattern analyzer rather than a full formal framework.

## 🚀 Quick Start

### Installation

1. **Clone the repository:**
   ```bash
   git clone <repository-url>
   cd LK_RB_Synthesis
   ```

2. **Install dependencies:**
   ```bash
   pip install -r requirements.txt
   ```

3. **Run the full analysis pipeline:**
   ```bash
   python main.py analyze
   ```

That's it! The analyzer will automatically:
- Parse all automaton implementations via AST analysis
- Extract rich metadata (embodied metaphors, material inferences) from automata
- Discover computational patterns in the code
- Identify algorithmic elaborations between strategies
- Generate Meaning-Use Analysis (MUA) reports in Markdown

## 📋 What This System Does

### 🔬 Automated Pattern Discovery (AST Analysis)
The analyzer uses AST (Abstract Syntax Tree) parsing to examine Python automaton source code and identify computational patterns:
- **`base_decomposition`**: Breaking numbers into base components (// and % operations)
- **`incremental_counting`**: State-based counting loops
- **`iterative_arithmetic`**: Repeated addition/subtraction operations
- **`value_adjustment`**: Target value calculations

### 🎯 Algorithmic Elaboration Detection
Automatically discovers how strategies build upon each other:
```
ADD_Counting → ADD_COBO → ADD_Chunking
    (incremental counting pattern)

ADD_Rounding → ADD_RMB → ADD_COBO
    (base decomposition pattern)
```

### 🧠 Rich Metadata Extraction
Automatically extracts and reports existing documentation from automata:
- **Embodied Metaphors** (Lakoff & Núñez): Source/target domains and entailments
- **Material Inferences** (Brandom): Premises, conclusions, and prerequisites
- **Visualization Hints**: Suggested cognitive representations (NumberLine, Object Piles, etc.)
- **Deployed Vocabulary**: Key conceptual terms introduced by each strategy

### 📊 Brandomian MUA Reports
Generates detailed Meaning-Use Analysis reports using Robert Brandom's framework:
- PV-Sufficiency (Practices sufficient for Vocabulary)
- PP-Sufficiency (Practices sufficient for Practices)
- VP-Sufficiency (Vocabulary sufficient for Practices)
- LX Relations (Elaborated-Explicating relationships)
- Pragmatic Metavocabulary analysis
- Pragmatic Expressive Bootstrapping

## 🛠️ Usage Guide

### Command Line Interface

```bash
# Run complete analysis pipeline
python main.py analyze

# Generate report for specific strategy
python main.py report --strategy ADD_COBO

# List all available strategies
python main.py list

# Interactive exploration mode
python main.py explore
```

### Interactive Mode

```bash
python main.py explore
```

Commands in interactive mode:
- `list` - Show all strategies
- `info <strategy>` - Get strategy details
- `report <strategy>` - Generate detailed report
- `overview` - Show system overview
- `patterns` - List computational patterns
- `help` - Show commands
- `quit` - Exit

### Advanced Usage

#### Generate Custom Reports
```bash
# Markdown report for specific strategy
python main.py report --strategy ADD_COBO --format markdown

# Overview report
python main.py report --format markdown > overview.md
```

#### Access Raw Analysis Data
```python
from mud_generator import AutomatonAnalyzer, MUDGenerator

# Analyze automata
analyzer = AutomatonAnalyzer("src/automata")
results = analyzer.analyze_all_automata()

# Generate diagrams
generator = MUDGenerator(results)
diagrams = generator.generate_mud_diagrams()
```

## 📁 Project Structure

```
LK_RB_Synthesis/
├── main.py                    # CLI entry point
├── mud_generator.py          # Core AST analysis and diagram generation
├── requirements.txt          # Python dependencies
├── README.md                 # This file
├── src/
│   ├── automata/            # Student strategy automaton implementations
│   │   ├── addition/        # Addition strategies (Counting, COBO, RMB, etc.)
│   │   ├── subtraction/     # Subtraction strategies (Sliding, Chunking, etc.)
│   │   ├── multiplication/  # Multiplication strategies
│   │   └── division/        # Division strategies
│   └── analysis/            # Metadata structures
├── output/                  # Generated diagrams and reports
├── scripts/                 # Utility scripts
└── [documentation files]    # Reference materials and analyses
```

## 🔍 Example Output

### Meaning-Use Analysis Report
```markdown
# Meaning-Use Analysis: ADD_COBO

## Strategy Metadata (From Automaton Documentation)

**Full Name:** COBO (Counting On by Bases and Ones)
**Description:** Simulates an addition strategy where the second number (B)
is decomposed into its base-ten and ones components...

### Embodied Metaphors (Lakoff & Núñez)

**Arithmetic as Motion Along a Path**
- **Source Domain:** Motion
- **Target Domain:** Arithmetic
- **Key Entailments:** Moving along a path can be done in segments.
  The final position is the sum of the starting position and the
  lengths of all segments.

### Material Inferences (Brandom)

**Iterative Addition**
- **Premise:** A quantity can be added by repeatedly adding a smaller unit.
- **Conclusion:** Adding a number B is equivalent to adding '1' B times,
  or adding '10' (B//10) times and then '1' (B%10) times.
- **Prerequisites (PP-Necessities):** Counting skills, Place value decomposition

---

## PV-Sufficiency Analysis
**Question:** What practices (P) are PV-sufficient to deploy V_ADD_COBO?

The following computational practices are necessary:
- **P_incremental_counting**: State-based iteration with accumulation

**Interpretation:** To deploy the vocabulary of ADD_COBO,
a practitioner must master these computational practices.

## PP-Sufficiency Analysis
**Question:** What practices are PP-sufficient for P_ADD_COBO?

### Prerequisite Strategies (PP-Necessities)
- **P_ADD_Counting** (via incremental_counting)
- **P_ADD_Chunking** (via incremental_counting)

**Interpretation:** ADD_COBO is algorithmically elaborated from
these prerequisite strategies.

## Pragmatic Metavocabulary Analysis
Following Lakoff & Núñez, embodied practices likely serve as metavocabulary:
- **V_Embodied** (e.g., 'collect objects', 'move along line')

**Expressive Bootstrapping:** Weaker vocabularies (Python, patterns, embodiment)
serve as metavocabularies for stronger vocabulary (arithmetic).
```

## 🎯 Key Features

### ✅ Fully Automated
- No manual specification of relationships required
- Discovers patterns from actual computational behavior
- Scales to any number of strategies

### 🎨 Theoretically Grounded Reports
- Brandomian Meaning-Use Analysis framework
- Lakoff & Núñez embodied metaphors with entailments
- Material inferences with premises and conclusions
- PV/VP/PP-sufficiency analyses
- LX relation identification
- Pragmatic metavocabulary analysis
- Confidence scores for elaboration relationships

### 🔬 Research Driven
- Inspired by Brandom's Meaning-Use Analysis framework
- Informed by Lakoff & Núñez's embodied mathematics
- Reveals computational structure of student arithmetic strategies

## ⚠️ Scope and Limitations

### What This System Does
- ✅ Discovers computational patterns in automaton source code (AST analysis)
- ✅ Extracts rich metadata (embodied metaphors, material inferences) from automata
- ✅ Identifies algorithmic elaborations based on shared patterns
- ✅ Generates Brandomian MUA reports (PV/VP/PP-sufficiency, LX relations)
- ✅ Analyzes pragmatic metavocabulary and expressive bootstrapping
- ✅ Provides confidence-scored elaboration relationships

### What This System Does NOT Do
- ❌ Does not generate visual MUD diagrams (reports only)
- ❌ Does not implement full Brandomian deontic scorekeeping
- ❌ Does not model Lakoff's conceptual metaphor mappings formally
- ❌ Does not identify practical elaboration through training (only algorithmic)
- ❌ Does not perform theorem proving or formal verification
- ❌ Does not include LLM-based pragmatic projection capabilities

This is an **analysis tool** for studying existing strategy implementations. It reveals computational structure that may correspond to Brandomian MUA concepts, but verification of philosophical claims requires human judgment.

## 🤝 Contributing

### Adding New Strategies
1. Create automaton in appropriate `src/automata/<operation>/` directory
2. Follow the `BaseAutomaton` interface
3. Run `python main.py analyze` to include in analysis

### Extending Pattern Detection
Modify `mud_generator.py` to add new computational pattern detectors.

## 📚 Research Background

This analyzer is a practical tool for studying student-invented arithmetic strategies. It discovers how strategies share computational patterns, revealing structural relationships between different approaches to arithmetic.

**Theoretical motivation:** The analysis framework is inspired by Brandom's Meaning-Use Analysis and Lakoff & Núñez's embodied mathematics, applying these ideas to study actual computational implementations of student strategies.

**What it analyzes:** Python automaton implementations of 23+ student strategies from Carpenter & Moser's Cognitively Guided Instruction (CGI) research.

## 📄 License

[Add license information here]

## 🙏 Acknowledgments

- Robert Brandom's *Between Saying and Doing*
- George Lakoff and Rafael Núñez's *Where Mathematics Comes From*
- The automata implementations that make this analysis possible