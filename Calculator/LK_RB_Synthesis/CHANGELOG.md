# LK_RB_Synthesis Changelog

## Summary

This system analyzes student-invented arithmetic strategies using:
- **AST pattern detection** for computational analysis
- **Brandomian Meaning-Use Analysis** for pragmatic semantics
- **Lakoff & Núñez embodied metaphors** for conceptual grounding

## Version History

### v2.0 - October 12, 2025

**Major Changes:**
- Replaced TikZ MUD diagram generation with Markdown MUA reports
- Added rich metadata extraction from automata (metaphors, inferences, prerequisites)
- Added cross-referencing of conceptual metaphors across strategies
- Fixed strategy listing to show all 21 strategies (was only showing 9)
- Cleaned up repository: removed all .tex/.pdf files and obsolete documentation

**Features Added:**
1. **Metadata Mining (Enhancement 1)**
   - Extracts embodied metaphors from automata
   - Extracts material inferences with premises/conclusions
   - Displays PP-Necessities (prerequisites)
   - Shows visualization hints and deployed vocabulary
   - Result: 18/21 strategies with rich metadata

2. **Metaphor Cross-Referencing (Enhancement 2)**
   - Groups strategies by shared conceptual metaphors
   - Categorizes as Foundational (≥5 uses), Common (3-4), or Specialized (<3)
   - Provides PP-Sufficiency hypotheses about embodied source domains
   - Result: 4 unique metaphors identified

**Files Modified:**
- `main.py` - Fixed list_strategies() to use strategy_patterns
- `mud_generator.py` - Added metadata extraction and metaphor analysis
- `mua_report_generator.py` - Added metadata sections and metaphor analysis
- `README.md` - Updated to reflect current capabilities

**Files Removed:**
- All .tex and .pdf files (MUD diagram generation obsolete)
- parse_latex.py
- Intermediate markdown documents (lakoff_tiny.md, synthesis_lk_rb.md, etc.)
- Old MUD-related files (automated_mud_results.json, MUD_Evolution_Comparison.md)
- Obsolete utilities (generate_skeletons.py, strategies.json)

### v1.0 - September 24, 2025

**Initial Implementation:**
- AST-based computational pattern detection
- Algorithmic elaboration discovery
- TikZ MUD diagram generation
- Basic Brandomian analysis framework

## Current Capabilities

### Analysis Features
- ✅ 21 strategies analyzed
- ✅ 2 computational patterns detected (base_decomposition, incremental_counting)
- ✅ 16 algorithmic elaboration relationships identified
- ✅ 18 strategies with rich metadata (metaphors, inferences)
- ✅ 4 unique embodied metaphors cross-referenced

### Report Sections Generated
1. **Strategy Metadata** - Metaphors, inferences, prerequisites from automaton documentation
2. **Strategy Overview** - Patterns used, elaboration relationships
3. **PV-Sufficiency Analysis** - Practices needed to deploy vocabulary
4. **PP-Sufficiency Analysis** - Prerequisite strategies and practices
5. **VP-Sufficiency Analysis** - Vocabulary sufficient to specify practices
6. **LX Relation Analysis** - Candidate elaborated-explicating relationships
7. **Pragmatic Metavocabulary Analysis** - What serves as metavocabulary
8. **Conceptual Metaphor Analysis** (full report only) - Shared metaphors across strategies
9. **Computational Pattern Analysis** (full report only) - Primitive practices
10. **Elaboration Analysis** (full report only) - PP-sufficiency chains

## Architecture

```
LK_RB_Synthesis/
├── main.py                          # CLI interface
├── mud_generator.py                 # Pattern analyzer & metadata extractor
├── mua_report_generator.py          # Brandomian report generator
├── requirements.txt                 # Python dependencies
├── README.md                        # User guide
├── CHANGELOG.md                     # This file
├── src/
│   ├── automata/                    # Strategy implementations
│   │   ├── addition/
│   │   ├── subtraction/
│   │   ├── multiplication/
│   │   └── division/
│   └── analysis/                    # Analysis utilities
│       ├── MUA_Metadata.py
│       └── ast_analyzer.py
└── output/                          # Generated reports
```

## Known Limitations

1. **Pattern Detection**: Only detects 2 computational patterns currently (base_decomposition, incremental_counting). Many strategies show 0 patterns - may need more sophisticated detection.

2. **Metadata Coverage**: 10/21 strategies have documented metaphors. More documentation needed for comprehensive metaphor analysis.

3. **LX Relations**: System identifies *candidate* LX relations but cannot verify if they represent genuine explication (requires philosophical analysis).

4. **Practical Elaboration**: Cannot detect practical elaboration through training (Wittgensteinian "going on in the same way") - only algorithmic elaboration visible in code.

## Future Enhancement Opportunities

From ENHANCEMENT_PROPOSALS.md:

**Easy (2-3 hours each):**
- Enhancement 3: Inference Chain Analysis
- Enhancement 6: Visualization Hint Clustering
- Enhancement 7: Prerequisites Verification

**Medium (4-6 hours each):**
- Enhancement 4: Metaphor-Pattern Correlation
- Enhancement 5: Vocabulary Deployment Analysis

## References

**Theoretical Foundations:**
- Robert Brandom, *Between Saying and Doing: Towards an Analytic Pragmatism*
- George Lakoff & Rafael Núñez, *Where Mathematics Comes From: How the Embodied Mind Brings Mathematics into Being*
- Cognitively Guided Instruction (CGI) framework for student arithmetic strategies
