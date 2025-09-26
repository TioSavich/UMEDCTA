# EPLE (Embodied Pragmatic Logic Engine)

## Automated Algorithmic Elaboration Discovery System

This project implements a novel system for **automatically discovering algorithmic elaborations** between student arithmetic strategies. Using Robert Brandom's Meaning-Use Analysis (MUA) framework and George Lakoff's conceptual metaphors, EPLE analyzes actual automaton implementations to identify shared computational patterns and generate Meaning-Use Diagrams (MUDs).

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

That's it! EPLE will automatically:
- Analyze all automaton implementations
- Discover computational patterns
- Identify algorithmic elaborations
- Generate professional MUD diagrams in TikZ format

## 📋 What EPLE Does

### 🔬 Automated Pattern Discovery
EPLE analyzes Python automaton source code to identify computational patterns:
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

### 📊 Professional Diagram Generation
Creates publication-ready TikZ diagrams showing elaboration relationships with proper Brandom category theory conventions.

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
├── main.py                    # Unified entry point
├── mud_generator.py          # Core analysis and diagram generation
├── requirements.txt          # Python dependencies
├── README.md                 # This file
├── src/automata/            # Automaton implementations
│   ├── addition/            # Addition strategies
│   ├── subtraction/         # Subtraction strategies
│   ├── multiplication/      # Multiplication strategies
│   └── division/            # Division strategies
├── data/                    # Analysis results and metadata
├── output/                  # Generated diagrams and reports
├── scripts/                 # Utility scripts
├── testing_system/          # Experimental/test scripts
└── eple/                    # EPLE framework core
```

## 🔍 Example Output

### Strategy Analysis Report
```markdown
## Strategy Analysis: ADD_COBO

### Computational Patterns Used
- **incremental_counting** (counting)
  - Used by 5 other strategies
- **base_decomposition** (decomposition)
  - Used by 13 other strategies

### Algorithmic Elaborations

#### As Base Strategy:
- **Elaborates** → ADD_Chunking
  - Shared patterns: incremental_counting, base_decomposition
  - Confidence: 0.67

#### As Elaborated Strategy:
- **Elaborated from** ← ADD_Counting
  - Shared patterns: incremental_counting
  - Confidence: 1.00
```

### Generated TikZ Diagram
```latex
\\begin{tikzpicture}[node distance=2cm and 2cm, auto]
\\node[draw, fill=blue!10, rounded corners] (title) at (0,0) {\\textbf{Addition MUD}};
\\node[draw, fill=green!10, rounded corners] (strategy_0) at (0.00,0.00) {Counting};
\\node[draw, fill=green!10, rounded corners] (strategy_1) at (5.03,3.77) {COBO};
\\draw[blue, ->, thick] (strategy_0) -- (strategy_1)
    node[midway, above] {\\scriptsize incremental_counting};
\\end{tikzpicture}
```

## 🎯 Key Features

### ✅ Fully Automated
- No manual specification of relationships required
- Discovers patterns from actual computational behavior
- Scales to any number of strategies

### 🎨 Professional Output
- Publication-ready TikZ diagrams
- Multiple report formats (Markdown, LaTeX, HTML)
- Confidence scores for all relationships

### 🔬 Research Driven
- Implements Brandom's Meaning-Use Analysis
- Grounded in Lakoff's conceptual metaphors
- Reveals cognitive structure of mathematical reasoning

## 🤝 Contributing

### Adding New Strategies
1. Create automaton in appropriate `src/automata/<operation>/` directory
2. Follow the `BaseAutomaton` interface
3. Run `python main.py analyze` to include in analysis

### Extending Pattern Detection
Modify `mud_generator.py` to add new computational pattern detectors.

## 📚 Research Background

This system demonstrates how **conceptual metaphors function as mechanisms of pragmatic elaboration** (Brandom) that allow embodied practices to confer content on abstract mathematical vocabularies.

**Core Thesis:** Mathematical necessity is the explicit expression of constraints inherent in embodied practices, structured by image schemas and elaborated through conceptual metaphors.

## 📄 License

[Add license information here]

## 🙏 Acknowledgments

- Robert Brandom's *Between Saying and Doing*
- George Lakoff and Rafael Núñez's *Where Mathematics Comes From*
- The automata implementations that make this analysis possible