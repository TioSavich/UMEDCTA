#!/usr/bin/env python3
"""
mua_report_generator.py: Meaning-Use Analysis Report Generator

Generates Brandomian MUA reports for student arithmetic strategies based on
discovered computational patterns and elaboration relationships.

Concepts from Brandom's "Between Saying and Doing":
- PV-Sufficiency: Practices sufficient to deploy a Vocabulary
- VP-Sufficiency: Vocabulary sufficient to specify Practices
- PP-Sufficiency: Practices sufficient for other Practices
  - Algorithmic Elaboration: Decomposable into primitives + algorithm
  - Practical Elaboration through Training: Developed via experience (pragmatic projection)
- Pragmatic Metavocabulary: V1 that specifies practices for V2
- LX Relation: V' is elaborated from and explicates V
- Pragmatic Expressive Bootstrapping: Weaker V serves as metavocabulary for stronger V
"""

from typing import Dict, List, Set, Any
from datetime import datetime


class MUAReportGenerator:
    """Generates Meaning-Use Analysis reports for strategy elaborations."""

    def __init__(self, analysis_results: Dict[str, Any]):
        """
        Initialize with analysis results from AutomatonAnalyzer.

        Args:
            analysis_results: Dict containing:
                - patterns: Dict of ComputationalPattern objects
                - strategy_patterns: Dict mapping strategy -> set of pattern names
                - elaborations: List of AlgorithmicElaboration objects
        """
        self.analysis_results = analysis_results
        self.patterns = analysis_results.get('patterns', {})
        self.strategy_patterns = analysis_results.get('strategy_patterns', {})
        self.elaborations = analysis_results.get('elaborations', [])
        self.strategy_metadata = analysis_results.get('strategy_metadata', {})  # NEW
        self.metaphor_sharing = analysis_results.get('metaphor_sharing', {})  # NEW

    def generate_full_report(self) -> str:
        """Generate complete MUA report for all strategies."""
        report = []
        report.append("# Meaning-Use Analysis Report")
        report.append(f"\nGenerated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report.append("\n" + "="*80 + "\n")

        report.append(self._generate_overview())
        report.append(self._generate_theoretical_framework())
        report.append(self._generate_pattern_analysis())
        report.append(self._generate_metaphor_analysis())  # NEW
        report.append(self._generate_elaboration_analysis())
        report.append(self._generate_lx_analysis())

        return "\n".join(report)

    def generate_strategy_report(self, strategy_name: str) -> str:
        """Generate detailed MUA report for a specific strategy."""
        report = []
        report.append(f"# Meaning-Use Analysis: {strategy_name}")
        report.append(f"\nGenerated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report.append("\n" + "="*80 + "\n")

        # Get strategy data
        patterns = self.strategy_patterns.get(strategy_name, set())
        base_elabs = [e for e in self.elaborations if e['base_strategy'] == strategy_name]
        elab_elabs = [e for e in self.elaborations if e['elaborated_strategy'] == strategy_name]

        # NEW: Add rich metadata section first if available
        metadata = self.strategy_metadata.get(strategy_name)
        if metadata:
            report.append(self._generate_metadata_section(strategy_name, metadata))

        report.append(self._generate_strategy_overview(strategy_name, patterns, base_elabs, elab_elabs))
        report.append(self._generate_pv_analysis(strategy_name, patterns))
        report.append(self._generate_pp_analysis(strategy_name, patterns, elab_elabs))
        report.append(self._generate_vp_analysis(strategy_name, patterns))
        report.append(self._generate_strategy_lx_analysis(strategy_name, base_elabs, elab_elabs))
        report.append(self._generate_pragmatic_metavocabulary_analysis(strategy_name, patterns))

        return "\n".join(report)

    def _generate_overview(self) -> str:
        """Generate overview section."""
        lines = ["## Overview\n"]
        lines.append(f"**Strategies Analyzed:** {len(self.strategy_patterns)}")
        lines.append(f"**Computational Patterns Identified:** {len(self.patterns)}")
        lines.append(f"**Elaboration Relationships Discovered:** {len(self.elaborations)}")
        lines.append("\n### Strategies")
        for strategy in sorted(self.strategy_patterns.keys()):
            pattern_count = len(self.strategy_patterns[strategy])
            lines.append(f"- {strategy} ({pattern_count} patterns)")
        return "\n".join(lines) + "\n"

    def _generate_theoretical_framework(self) -> str:
        """Explain Brandomian framework used in analysis."""
        return """## Theoretical Framework

This report analyzes student arithmetic strategies using Robert Brandom's Meaning-Use Analysis
(MUA) framework from "Between Saying and Doing: Towards an Analytic Pragmatism."

### Key Concepts

**1. Practice-Vocabulary Relations:**
- **PV-Sufficiency**: Practices (P) are sufficient to deploy a Vocabulary (V)
  - What you must DO to correctly use the vocabulary
- **VP-Sufficiency**: Vocabulary (V) is sufficient to specify Practices (P)
  - What you can SAY to describe what needs to be done

**2. Practice-Practice Relations (PP-Sufficiency):**
- **Algorithmic Elaboration**: Complex practice decomposable into primitive practices + algorithm
  - Example: Long division = repeated subtraction + place value tracking
  - Fully mechanizable; no creative insight needed

- **Practical Elaboration through Training** (Pragmatic Projection):
  - New practice developed from prior practice through experience
  - Cannot be fully algorithmically decomposed
  - Requires "going on in the same way" (Wittgenstein)
  - Involves genuine learning, not just mechanical execution

**3. Pragmatic Metavocabulary:**
- V₁ is a pragmatic metavocabulary for V₂ when:
  - V₁ can specify the practices P₂ needed to deploy V₂
  - Often V₁ is expressively weaker than V₂

**4. LX Relation (Elaborated-Explicating):**
- V' is LX to V when V' is:
  - **(L) Elaborated from** the practices underlying V
  - **(X) Explicating of** those practices (makes implicit explicit)
- LX captures conceptual progress: knowing-how → knowing-that

**5. Pragmatic Expressive Bootstrapping:**
- A weaker vocabulary can serve as metavocabulary for a stronger one
- Example: Non-indexical language can specify use of indexical language
- Explains how embodied practices ground abstract mathematics

### What This Analysis Can Determine

Based on AST analysis of Python automaton implementations, this system identifies:

✅ **Computational patterns** as proxies for primitive practices
✅ **Algorithmic elaborations** where strategies share computational structure
✅ **PP-sufficiency chains** showing prerequisite relationships
✅ **Candidate LX relations** where complex strategies make simple ones explicit

⚠️ **Limitations**:
- Cannot determine semantic content or conceptual metaphors from code alone
- Cannot identify practical elaborations requiring genuine insight
- Patterns are computational, not necessarily cognitive
- LX relations are candidates requiring philosophical verification

"""

    def _generate_pattern_analysis(self) -> str:
        """Analyze identified computational patterns as primitive practices."""
        lines = ["## Computational Patterns as Primitive Practices\n"]
        lines.append("The following patterns represent primitive computational practices")
        lines.append("identified in the strategy implementations:\n")

        for pattern_name, pattern_data in sorted(self.patterns.items()):
            lines.append(f"### Pattern: `{pattern_name}`\n")
            lines.append(f"**Type:** {pattern_data.get('type', 'unknown')}")
            lines.append(f"**Used by:** {pattern_data.get('usage_count', 0)} strategies")
            lines.append(f"**Strategies:** {', '.join(sorted(pattern_data.get('strategies_using', [])))}")

            # Interpret pattern as practice
            lines.append(f"\n**Interpretation as Practice (P_{pattern_name}):**")
            if pattern_name == 'base_decomposition':
                lines.append("- Breaking numbers into base-10 components (tens, ones)")
                lines.append("- Computational signature: `num // 10`, `num % 10`")
                lines.append("- Cognitive analogue: Place value understanding")
                lines.append("- **PP-Necessity for:** Most multi-digit strategies")
            elif pattern_name == 'incremental_counting':
                lines.append("- State-based iteration with accumulation")
                lines.append("- Computational signature: `while` loops, register increments")
                lines.append("- Cognitive analogue: Iterated succession, counting on")
                lines.append("- **PP-Necessity for:** Strategies building on counting")
            else:
                lines.append(f"- Computational practice identified in code")
                lines.append(f"- Represents shared subroutine across strategies")

            lines.append("")

        return "\n".join(lines)

    def _generate_metaphor_analysis(self) -> str:
        """Analyze conceptual metaphors shared across strategies."""
        lines = ["## Conceptual Metaphor Analysis (Lakoff & Núñez)\n"]

        if not self.metaphor_sharing:
            lines.append("No metaphor data available from automaton metadata.\n")
            return "\n".join(lines)

        lines.append("This section analyzes which strategies share embodied conceptual metaphors,")
        lines.append("revealing foundational grounding metaphors vs. specialized ones.\n")

        # Sort metaphors by usage count (descending)
        metaphor_counts = [(metaphor, len(strategies)) for metaphor, strategies in self.metaphor_sharing.items()]
        metaphor_counts.sort(key=lambda x: x[1], reverse=True)

        for metaphor_name, count in metaphor_counts:
            strategies = sorted(self.metaphor_sharing[metaphor_name])
            lines.append(f"### \"{metaphor_name}\"\n")
            lines.append(f"**Used by {count} strateg{'y' if count == 1 else 'ies'}:**")
            lines.append(f"{', '.join(strategies)}\n")

            # Get metaphor details from first strategy that uses it
            metaphor_details = None
            for strategy in strategies:
                metadata = self.strategy_metadata.get(strategy, {})
                for m in metadata.get('metaphors', []):
                    if m['name'] == metaphor_name:
                        metaphor_details = m
                        break
                if metaphor_details:
                    break

            if metaphor_details:
                lines.append(f"**Source Domain:** {metaphor_details['source_domain']}")
                lines.append(f"**Target Domain:** {metaphor_details['target_domain']}")
                lines.append(f"**Key Entailments:** {metaphor_details['entailments']}\n")

            # Interpret based on usage
            if count >= 5:
                lines.append(f"**Interpretation:** This is a **foundational grounding metaphor** (Lakoff's 4Gs).")
                lines.append(f"Wide usage suggests it's a core conceptual structure for arithmetic reasoning.")
                lines.append(f"Students must master the source domain ({metaphor_details['source_domain'] if metaphor_details else '?'})")
                lines.append(f"to deploy these strategies.\n")

                lines.append(f"**PP-Sufficiency Hypothesis:** Mastery of embodied practices in the")
                lines.append(f"{metaphor_details['source_domain'] if metaphor_details else 'source'} domain")
                lines.append(f"may be PP-sufficient for basic arithmetic practices via this metaphor.\n")
            elif count >= 3:
                lines.append(f"**Interpretation:** This is a **common metaphor** used across multiple operations.")
                lines.append(f"Strategies using this metaphor form a conceptual cluster.\n")
            else:
                lines.append(f"**Interpretation:** This is a **specialized metaphor** for specific strategies.")
                lines.append(f"May represent advanced or operation-specific conceptualizations.\n")

            lines.append("---\n")

        # Summary
        lines.append(f"### Summary\n")
        lines.append(f"**Total unique metaphors identified:** {len(self.metaphor_sharing)}")

        foundational = [m for m, c in metaphor_counts if c >= 5]
        common = [m for m, c in metaphor_counts if 3 <= c < 5]
        specialized = [m for m, c in metaphor_counts if c < 3]

        lines.append(f"- **Foundational (≥5 strategies):** {len(foundational)}")
        if foundational:
            lines.append(f"  - {', '.join(foundational)}")
        lines.append(f"- **Common (3-4 strategies):** {len(common)}")
        if common:
            lines.append(f"  - {', '.join(common)}")
        lines.append(f"- **Specialized (<3 strategies):** {len(specialized)}")
        if specialized:
            lines.append(f"  - {', '.join(specialized)}")

        lines.append(f"\n**Pedagogical Implication:** Students who struggle with foundational metaphors")
        lines.append(f"may need remediation in the corresponding embodied source domains before")
        lines.append(f"advancing to abstract arithmetic strategies.\n")

        return "\n".join(lines)

    def _generate_elaboration_analysis(self) -> str:
        """Analyze PP-sufficiency relationships (algorithmic elaborations)."""
        lines = ["## PP-Sufficiency: Algorithmic Elaborations\n"]
        lines.append("These are **algorithmic elaborations** where one strategy's practices")
        lines.append("are computationally sufficient for another strategy.\n")

        # Group by shared patterns
        by_pattern = {}
        for elab in self.elaborations:
            patterns = tuple(sorted(elab['shared_patterns']))
            if patterns not in by_pattern:
                by_pattern[patterns] = []
            by_pattern[patterns].append(elab)

        for patterns, elabs in sorted(by_pattern.items()):
            pattern_str = ", ".join(patterns)
            lines.append(f"### Elaborations via: `{pattern_str}`\n")

            for elab in elabs:
                base = elab['base_strategy']
                elaborated = elab['elaborated_strategy']
                conf = elab['confidence']

                lines.append(f"**{base} → {elaborated}** (confidence: {conf:.2f})")
                lines.append(f"- **Type:** Algorithmic Elaboration")
                lines.append(f"- **PP-Sufficiency:** P_{base} is sufficient for P_{elaborated}")
                lines.append(f"- **Shared practices:** {pattern_str}")
                lines.append(f"- **Interpretation:** {elaborated} builds on computational patterns from {base}")
                lines.append("")

        lines.append("\n**Note on Algorithmic vs. Practical Elaboration:**")
        lines.append("- These are *algorithmic* elaborations (code reuse, shared subroutines)")
        lines.append("- *Practical* elaborations (insight, training) cannot be detected from code")
        lines.append("- Cognitive development may involve practical elaboration not visible here")

        return "\n".join(lines)

    def _generate_lx_analysis(self) -> str:
        """Identify candidate LX relations."""
        lines = ["## LX Relations: Candidate Elaborated-Explicating Pairs\n"]
        lines.append("Candidate LX relations where a complex strategy may make")
        lines.append("a simpler strategy's implicit practices explicit.\n")

        # Find chains: A → B → C suggests B might be LX to A
        strategy_to_prereqs = {}
        strategy_to_successors = {}

        for elab in self.elaborations:
            base = elab['base_strategy']
            elaborated = elab['elaborated_strategy']

            if elaborated not in strategy_to_prereqs:
                strategy_to_prereqs[elaborated] = []
            strategy_to_prereqs[elaborated].append(base)

            if base not in strategy_to_successors:
                strategy_to_successors[base] = []
            strategy_to_successors[base].append(elaborated)

        # Strategies that both depend on others and are depended upon
        lx_candidates = []
        for strategy in self.strategy_patterns.keys():
            prereqs = strategy_to_prereqs.get(strategy, [])
            successors = strategy_to_successors.get(strategy, [])
            if prereqs and successors:
                lx_candidates.append((strategy, prereqs, successors))

        if lx_candidates:
            for strategy, prereqs, successors in lx_candidates:
                lines.append(f"### Candidate: `{strategy}` as LX\n")
                lines.append(f"**Elaborated from:** {', '.join(prereqs)}")
                lines.append(f"**Explicates for:** {', '.join(successors)}")
                lines.append(f"\n**Why this might be LX:**")
                lines.append(f"- {strategy} builds on practices from {', '.join(prereqs)}")
                lines.append(f"- {strategy} provides structure used by {', '.join(successors)}")
                lines.append(f"- May make implicit patterns in {prereqs[0]} explicit")
                lines.append(f"\n**Verification needed:** Philosophical analysis of whether {strategy}")
                lines.append(f"genuinely explicates (makes sayable) what {prereqs[0]} only does.\n")
        else:
            lines.append("No clear LX candidates detected in elaboration chains.")
            lines.append("LX relations typically require longer elaboration sequences.\n")

        return "\n".join(lines)

    def _generate_strategy_overview(self, strategy_name: str, patterns: Set[str],
                                   base_elabs: List, elab_elabs: List) -> str:
        """Generate overview section for specific strategy."""
        lines = [f"## Strategy Overview\n"]
        lines.append(f"**Computational Patterns Used:** {len(patterns)}")
        if patterns:
            lines.append(f"- {', '.join(sorted(patterns))}")
        lines.append(f"\n**Elaborates from** (depends on): {len(elab_elabs)} strategies")
        lines.append(f"**Elaborates to** (enables): {len(base_elabs)} strategies\n")
        return "\n".join(lines)

    def _generate_pv_analysis(self, strategy_name: str, patterns: Set[str]) -> str:
        """Analyze PV-sufficiency: practices sufficient to deploy this strategy."""
        lines = [f"## PV-Sufficiency Analysis\n"]
        lines.append(f"**Question:** What practices (P) are PV-sufficient to deploy V_{strategy_name}?")
        lines.append(f"**Answer (from computational analysis):**\n")

        if patterns:
            lines.append(f"The following computational practices are necessary:\n")
            for pattern in sorted(patterns):
                lines.append(f"- **P_{pattern}**: {self._describe_pattern(pattern)}")
            lines.append(f"\n**Interpretation:** To deploy the vocabulary of {strategy_name},")
            lines.append(f"a practitioner must master these computational practices.")
        else:
            lines.append(f"No distinctive computational patterns detected.")
            lines.append(f"This strategy may: (a) be primitive, (b) rely on patterns not yet identified,")
            lines.append(f"or (c) involve practical elaboration not visible in code.\n")

        lines.append(f"\n**Limitations:** AST analysis reveals computational practices only.")
        lines.append(f"Cognitive practices (e.g., 'recognizing patterns', 'strategic thinking')")
        lines.append(f"may be PV-necessary but not detectable from code.")

        return "\n".join(lines) + "\n"

    def _generate_pp_analysis(self, strategy_name: str, patterns: Set[str], elab_elabs: List) -> str:
        """Analyze PP-sufficiency: practices sufficient for this strategy."""
        lines = [f"## PP-Sufficiency Analysis\n"]
        lines.append(f"**Question:** What practices are PP-sufficient for P_{strategy_name}?")
        lines.append(f"**Answer (from computational analysis):**\n")

        if elab_elabs:
            lines.append(f"### Prerequisite Strategies (PP-Necessities)\n")
            for elab in elab_elabs:
                base = elab['base_strategy']
                shared = ', '.join(elab['shared_patterns'])
                lines.append(f"- **P_{base}** (via {shared})")

            lines.append(f"\n**Interpretation:** {strategy_name} is algorithmically elaborated from")
            lines.append(f"these prerequisite strategies. Mastering the prerequisites provides")
            lines.append(f"computational patterns sufficient for {strategy_name}.\n")
        else:
            lines.append(f"No prerequisite strategies detected.")
            lines.append(f"This may be a **primitive strategy** in the elaboration hierarchy.\n")

        lines.append(f"**Note:** This analysis identifies *algorithmic* PP-sufficiency only.")
        lines.append(f"*Practical elaboration through training* would require additional practices")
        lines.append(f"(e.g., 'strategic insight', 'pattern recognition') not visible in code.")

        return "\n".join(lines) + "\n"

    def _generate_vp_analysis(self, strategy_name: str, patterns: Set[str]) -> str:
        """Analyze VP-sufficiency: vocabulary sufficient to specify practices."""
        lines = [f"## VP-Sufficiency Analysis\n"]
        lines.append(f"**Question:** What vocabulary is VP-sufficient to specify P_{strategy_name}?")
        lines.append(f"**Answer (from computational analysis):**\n")

        lines.append(f"### Computational Metavocabulary\n")
        lines.append(f"The vocabulary of **computational patterns** is VP-sufficient:")
        lines.append(f"- We can SAY what P_{strategy_name} does using pattern vocabulary:")
        if patterns:
            for pattern in sorted(patterns):
                lines.append(f"  - Uses `{pattern}`: {self._describe_pattern(pattern)}")
        lines.append(f"\n### Python Implementation")
        lines.append(f"The Python code itself serves as a VP-sufficient metavocabulary:")
        lines.append(f"- The automaton implementation specifies the practice algorithmically")
        lines.append(f"- Anyone can read the code to understand the computational practice")

        lines.append(f"\n**Pragmatic Expressive Bootstrapping:** The vocabulary of Python + computational")
        lines.append(f"patterns (expressively weaker than arithmetic vocabulary) is sufficient to specify")
        lines.append(f"the practice of {strategy_name}.")

        return "\n".join(lines) + "\n"

    def _generate_strategy_lx_analysis(self, strategy_name: str, base_elabs: List, elab_elabs: List) -> str:
        """Analyze potential LX relations for this strategy."""
        lines = [f"## LX Relation Analysis\n"]
        lines.append(f"**Question:** Is {strategy_name} LX to any simpler strategy?")
        lines.append(f"Or does any strategy serve as LX elaboration of {strategy_name}?\n")

        if elab_elabs and base_elabs:
            lines.append(f"### {strategy_name} as Potential LX Mediator\n")
            prereqs = [e['base_strategy'] for e in elab_elabs]
            successors = [e['elaborated_strategy'] for e in base_elabs]

            lines.append(f"- **Elaborated from:** {', '.join(prereqs)}")
            lines.append(f"- **Enables elaboration to:** {', '.join(successors)}")
            lines.append(f"\n**LX Hypothesis:** {strategy_name} may be LX to {prereqs[0]} if:")
            lines.append(f"1. It makes explicit the practices implicit in {prereqs[0]}")
            lines.append(f"2. It provides vocabulary to SAY what {prereqs[0]} only DOES")
            lines.append(f"3. It represents conceptual progress, not just mechanical elaboration")
            lines.append(f"\n**Verification:** Requires philosophical analysis of whether {strategy_name}")
            lines.append(f"genuinely explicates (not just reuses) the practices of {prereqs[0]}.")

        elif base_elabs:
            lines.append(f"### {strategy_name} as Potential LX Base\n")
            successors = [e['elaborated_strategy'] for e in base_elabs]
            lines.append(f"**Successors:** {', '.join(successors)}")
            lines.append(f"\n{strategy_name} may be a primitive practice that more complex strategies")
            lines.append(f"elaborate and explicate. Check if successors make {strategy_name}'s")
            lines.append(f"implicit structure explicit.")

        elif elab_elabs:
            lines.append(f"### {strategy_name} as Potential LX Elaboration\n")
            prereqs = [e['base_strategy'] for e in elab_elabs]
            lines.append(f"**Prerequisites:** {', '.join(prereqs)}")
            lines.append(f"\n{strategy_name} may be LX to {prereqs[0]} if it makes explicit")
            lines.append(f"what {prereqs[0]} leaves implicit. No further elaborations detected,")
            lines.append(f"so {strategy_name} may be terminal in this elaboration chain.")

        else:
            lines.append(f"No clear LX relationships detected.")
            lines.append(f"{strategy_name} appears isolated in the elaboration network.")

        return "\n".join(lines) + "\n"

    def _generate_pragmatic_metavocabulary_analysis(self, strategy_name: str, patterns: Set[str]) -> str:
        """Analyze pragmatic metavocabulary relationships."""
        lines = [f"## Pragmatic Metavocabulary Analysis\n"]
        lines.append(f"**Question:** What serves as pragmatic metavocabulary for V_{strategy_name}?\n")

        lines.append(f"### Computational Metavocabulary")
        lines.append(f"The vocabulary of computational patterns serves as pragmatic metavocabulary:")
        lines.append(f"- **V_Patterns** can specify the practices P_{strategy_name}")
        lines.append(f"- Patterns detected: {', '.join(sorted(patterns)) if patterns else 'none'}")

        lines.append(f"\n### Embodied Metavocabulary (Hypothetical)")
        lines.append(f"Following Lakoff & Núñez, embodied practices likely serve as metavocabulary:")
        lines.append(f"- **V_Embodied** (e.g., 'collect objects', 'move along line')")
        lines.append(f"- These embodied terms can specify mathematical practices")
        lines.append(f"- **Limitation:** Cannot verify from code; requires cognitive analysis")

        lines.append(f"\n**Expressive Bootstrapping:** Weaker vocabularies (Python, patterns, embodiment)")
        lines.append(f"serve as metavocabularies for stronger vocabulary (arithmetic of {strategy_name}).")

        return "\n".join(lines) + "\n"

    def _generate_metadata_section(self, strategy_name: str, metadata: Dict) -> str:
        """Generate section for rich metadata (metaphors, inferences) from automaton."""
        lines = ["## Strategy Metadata (From Automaton Documentation)\n"]

        # Strategy name and description
        if metadata.get('strategy_name'):
            lines.append(f"**Full Name:** {metadata['strategy_name']}")
        if metadata.get('description'):
            lines.append(f"**Description:** {metadata['description']}\n")

        # Embodied Metaphors (Lakoff & Núñez)
        metaphors = metadata.get('metaphors', [])
        if metaphors:
            lines.append("### Embodied Metaphors (Lakoff & Núñez)\n")
            for m in metaphors:
                lines.append(f"**{m['name']}**")
                lines.append(f"- **Source Domain:** {m['source_domain']}")
                lines.append(f"- **Target Domain:** {m['target_domain']}")
                lines.append(f"- **Key Entailments:** {m['entailments']}\n")

        # Material Inferences (Brandom)
        inferences = metadata.get('inferences', [])
        if inferences:
            lines.append("### Material Inferences (Brandom)\n")
            for inf in inferences:
                lines.append(f"**{inf['name']}**")
                lines.append(f"- **Premise:** {inf['premise']}")
                lines.append(f"- **Conclusion:** {inf['conclusion']}")
                if inf.get('prerequisites'):
                    prereq_str = ', '.join(inf['prerequisites'])
                    lines.append(f"- **Prerequisites (PP-Necessities):** {prereq_str}\n")

        # Visualization hints
        viz_hints = metadata.get('visualization_hints', [])
        if viz_hints:
            lines.append(f"### Cognitive Representation")
            lines.append(f"**Visualization Type:** {', '.join(viz_hints)}\n")

        # Deployed vocabulary
        deployed_vocab = metadata.get('deployed_vocabulary', '')
        if deployed_vocab:
            lines.append(f"### Vocabulary Deployed")
            lines.append(f"This strategy introduces/deploys: **{deployed_vocab}**\n")

        lines.append("---\n")
        return "\n".join(lines)

    def _describe_pattern(self, pattern_name: str) -> str:
        """Provide description of computational pattern."""
        descriptions = {
            'base_decomposition': 'Breaking numbers into base-10 components (// and % operations)',
            'incremental_counting': 'State-based iteration with accumulation',
            'iterative_arithmetic': 'Repeated addition/subtraction in loops',
            'value_adjustment': 'Target value calculation and adjustment'
        }
        return descriptions.get(pattern_name, 'Computational pattern identified in code')


def main():
    """Example usage."""
    import json

    # Load existing analysis results
    with open('output/eple_results.json', 'r') as f:
        data = json.load(f)

    analysis_results = data.get('analysis_results')
    if not analysis_results:
        print("No analysis results found. Run 'python main.py analyze' first.")
        return

    generator = MUAReportGenerator(analysis_results)

    # Generate full report
    report = generator.generate_full_report()
    with open('output/mua_full_report.md', 'w') as f:
        f.write(report)
    print("✅ Generated: output/mua_full_report.md")

    # Generate strategy-specific report
    strategy_report = generator.generate_strategy_report('ADD_COBO')
    with open('output/mua_strategy_ADD_COBO.md', 'w') as f:
        f.write(strategy_report)
    print("✅ Generated: output/mua_strategy_ADD_COBO.md")


if __name__ == '__main__':
    main()
