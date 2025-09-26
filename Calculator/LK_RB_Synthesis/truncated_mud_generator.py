#!/usr/bin/env python3
"""
mud_generator.py: Consolidated Meaning-Use Diagram (MUD) Generator
"""

import os
import sys
import json
import ast
import inspect
from typing import Dict, List, Set, Tuple, Any
from dataclasses import dataclass, field
from collections import defaultdict
import argparse
import math # Import math for layout calculations
import datetime

# Data Classes (Stubs included for structure)
@dataclass
class ComputationalPattern:
    name: str
    operation_type: str
    strategies_using: Set[str] = field(default_factory=set)

@dataclass
class AlgorithmicElaboration:
    base_strategy: str
    elaborated_strategy: str
    shared_patterns: Set[str]
    elaboration_type: str
    confidence: float

# AutomatonAnalyzer Class (Implementation omitted for brevity)
class AutomatonAnalyzer:
    """Analyzes automaton implementations."""
    def __init__(self, automata_dir: str): pass
    def analyze_all_automata(self) -> Dict[str, Any]:
        # Placeholder return
        return {"patterns": {}, "elaborations": [], "strategy_patterns": {}}


# --- MUDGenerator Class (Updated for MUD conventions) ---

class MUDGenerator:
    """Generates MUD diagrams from algorithmic elaboration analysis."""

    def __init__(self, analysis_results: Dict[str, Any]):
        self.analysis_results = analysis_results
        self.mud_diagrams = {}

    def generate_mud_diagrams(self) -> Dict[str, Any]:
        """Generate MUD diagrams for all discovered elaborations."""
        operation_groups = self._group_elaborations_by_operation()

        for operation, elaborations in operation_groups.items():
            mud_diagram = self._generate_operation_mud(operation, elaborations)
            self.mud_diagrams[operation] = mud_diagram

        return self.mud_diagrams

    # (Helper methods _group_elaborations_by_operation and _extract_operation_type remain logically the same)
    def _group_elaborations_by_operation(self) -> Dict[str, List[Dict]]:
        operation_groups = defaultdict(list)
        for elab in self.analysis_results.get('elaborations', []):
            # Simplified grouping logic
            base_op = self._extract_operation_type(elab.get('base_strategy', ''))
            if base_op == 'general':
                base_op = 'miscellaneous'
            operation_groups[base_op].append(elab)
        return operation_groups

    def _extract_operation_type(self, strategy_id: str) -> str:
        strategy_upper = strategy_id.upper()
        if 'ADD' in strategy_upper or 'COUNTING' in strategy_upper:
            return 'addition'
        # ... other operations ...
        return 'general'

    def _generate_operation_mud(self, operation: str, elaborations: List[Dict]) -> Dict[str, Any]:
        """Generate a MUD diagram for a specific operation."""
        strategies = set()
        elaboration_relationships = defaultdict(list)

        for elab in elaborations:
            base = elab.get('base_strategy')
            elaborated = elab.get('elaborated_strategy')
            patterns = elab.get('shared_patterns', [])

            if base and elaborated:
                strategies.add(base)
                strategies.add(elaborated)
                directional_key = (base, elaborated)
                # Collect unique patterns for this specific link
                for pattern in patterns:
                    if pattern not in elaboration_relationships[directional_key]:
                        elaboration_relationships[directional_key].append(pattern)

        tikz_code = self._generate_tikz_diagram(operation, list(strategies), elaboration_relationships)

        return {
            'operation': operation,
            'strategies': list(strategies),
            'tikz_diagram': tikz_code,
            'summary': f"Summary for {operation}" # Placeholder summary
        }

    def _format_strategy_label(self, strategy_name: str) -> str:
        """Formats the strategy name according to MUD typesetting rules: P\textsubscript{Name}."""
        display_name = strategy_name.replace('SAR_', '')

        # Handle potential line breaks for very long names by splitting near the middle underscore
        if len(display_name) > 25:
            best_split_point = -1
            middle = len(display_name) / 2
            for i, char in enumerate(display_name):
                if char == '_':
                    if best_split_point == -1 or abs(i - middle) < abs(best_split_point - middle):
                        best_split_point = i

            if best_split_point != -1 and best_split_point > 0:
                part1 = display_name[:best_split_point]
                part2 = display_name[best_split_point+1:]
                # Format: P\textsubscript{Part1} \\ \textsubscript{Part2}
                # Use raw f-string (rf"") to handle backslashes easily.
                return rf"P\textsubscript{{{part1}}} \\ \textsubscript{{{part2}}}"

        # Default format: P\textsubscript{Name}. Underscores don't need escaping in \textsubscript.
        return rf"P\textsubscript{{{display_name}}}"

    def _generate_tikz_diagram(self, operation: str, strategies: List[str], elaboration_relationships: Dict[Tuple[str, str], List[str]]) -> str:
        """Generate TikZ code for the MUD diagram following Brandom's conventions."""

        # --- Header and Style Definitions ---
        # We generate only the tikzpicture environment. The preamble (libraries) must be handled by the consuming document (e.g., ReportGenerator).
        # Use raw strings (r"") for LaTeX definitions for clarity.
        tikz_lines = [
            r"\begin{tikzpicture}[",
            "  % Node Styles",
            # Rule 1: Vocabularies (V): light gray, filled ellipses, solid black border.
            r"  vnode/.style={ellipse, draw, fill=lightgray!50, text=black, minimum height=1.3cm, minimum width=2.8cm, align=center},",
            # Rule 2: Practices (P): darker gray, filled rounded rectangles, solid black border.
            r"  pnode/.style={rectangle, rounded corners=5pt, draw, fill=gray!70, text=black, minimum height=1.3cm, minimum width=3.5cm, align=center, inner xsep=0.3cm, inner ysep=0.2cm},",
            # Rule 5: Algorithmic Elaboration (PAlgEl): light gray rectangle, sharp corners.
            r"  graybox/.style={rectangle, fill=lightgray!50, inner sep=4pt, minimum height=1.1cm, anchor=center, align=center, text centered},",
            "  % Arrow Styles",
            # Rule 3: Basic MURs: solid, thick, black arrows, Stealth head.
            r"  solidarrow/.style={-Stealth, thick},",
            # Rule 4: Resultant MURs: dashed, thick, gray arrows, Stealth head.
            r"  dashedarrow/.style={dashed, -Stealth, thick, gray},",
            r"  textarrow/.style={align=center, inner sep=1pt}",
            r"]",
            # Set font style and line spacing
            r"\tikzset{font=\linespread{0.8}\selectfont}",
            "",
            f"% Diagram for: {operation.replace('_', ' ')}",
            ""
        ]

        # --- Node Placement (Circular Layout) ---
        strategy_positions = {}
        num_strategies = len(strategies)

        if num_strategies > 0:
            radius = max(5, num_strategies * 1.0)
            angle_step = 360 / num_strategies

            for i, strategy in enumerate(strategies):
                node_id = f"P_{i}"
                strategy_positions[strategy] = node_id

                angle = 90 - (i * angle_step) # Start from top (90 degrees)
                x = radius * math.cos(math.radians(angle))
                y = radius * math.sin(math.radians(angle))

                display_label = self._format_strategy_label(strategy)
                # Use raw f-string (rf"") for the node definition
                tikz_lines.append(rf"\node[pnode] ({node_id}) at ({x:.2f},{y:.2f}) {{{display_label}}};")

        tikz_lines.append("")

        # --- Arrow Generation (Algorithmic Elaborations) ---
        arrow_count = 1
        for (base_strategy, elaborated_strategy), patterns in elaboration_relationships.items():
            if base_strategy in strategy_positions and elaborated_strategy in strategy_positions:
                base_pos = strategy_positions[base_strategy]
                elab_pos = strategy_positions[elaborated_strategy]

                # Rule 7: Typesetting. Escape underscores (\_) for literal printing.
                escaped_patterns = [p.replace('_', r'\_') for p in sorted(patterns)]
                pattern_label = ", ".join(escaped_patterns)

                # Rule 5: Labels. P\textsubscript{AlgEl} Number: PP-suff \\ (Patterns)
                # Use raw f-string for the box content definition.
                box_content = rf"P\textsubscript{{AlgEl}} {arrow_count}: PP-suff \\ ({pattern_label})"

                # Draw the arrow (Basic MUR) with the PAlgEl overlay. Use 'sloped' for better alignment.
                tikz_lines.append(rf"\draw[solidarrow] ({base_pos}) -- node[graybox, midway, sloped] {{{box_content}}} ({elab_pos});")

                arrow_count += 1

        tikz_lines.extend([
            "",
            r"\end{tikzpicture}"
        ])

        # Join with standard newline characters
        return "\n".join(tikz_lines)


# --- ReportGenerator Class ---

class ReportGenerator:
    """Generates reports in multiple formats from analysis results."""

    def __init__(self, analysis_results: Dict[str, Any], mud_diagrams: Dict[str, Any] = None):
        self.analysis_results = analysis_results
        self.mud_diagrams = mud_diagrams or {}

    def generate_latex_report(self, strategy_name: str = None) -> str:
        """Generate a LaTeX report."""
        # Preamble must include all necessary packages and TikZ libraries.
        # Use raw strings for LaTeX commands.
        lines = [
            r"\documentclass{article}",
            r"\usepackage[utf8]{inputenc}",
            r"\usepackage{geometry}",
            r"\usepackage{hyperref}",
            r"\usepackage{booktabs}",
            r"\usepackage{xcolor}",
            r"\usepackage{tikz}",
            # Crucial: Load the libraries required by the MUD diagrams
            r"\usetikzlibrary{positioning, shapes.geometric, arrows.meta, fit, backgrounds, calc, chains}",
            r"\geometry{margin=1in}",
            r"\title{Algorithmic Elaboration Analysis Report}",
            f"\\date{{{self._get_timestamp()}}}",
            r"\begin{document}",
            r"\maketitle",
            r"\section{Overview}"
        ]

        # (Overview and specific strategy analysis content generation omitted for brevity)

        if self.mud_diagrams:
            lines.extend(self._generate_mud_diagrams_latex_section())

        lines.append(r"\end{document}")
        return "\n".join(lines)

    def _generate_mud_diagrams_latex_section(self) -> List[str]:
        """Generate a LaTeX section containing all MUD diagrams."""
        lines = [r"\section{Meaning-Use Diagrams}"]

        for operation, diagram_data in self.mud_diagrams.items():
            # Ensure title is safe for LaTeX
            operation_title = operation.replace('_', ' ').title()
            lines.append(f"\\subsection{{{operation_title}}}")
            lines.append(r"\begin{center}")
            # The TikZ code generated by MUDGenerator is ready to be embedded.
            lines.append(diagram_data.get('tikz_diagram', ''))
            lines.append(r"\end{center}")
            lines.append(r"\newpage")

        return lines

    def _get_timestamp(self) -> str:
        """Get current timestamp for reports."""
        return datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    # (Implementations for generate_markdown_report and generate_html_report omitted for brevity)


# --- Main execution block ---

def main():
    """Main entry point for MUD generation."""
    # (Argument parsing logic remains the same as the original script)
    parser = argparse.ArgumentParser(description="Consolidated MUD Generator")
    # ... (setup subparsers: analyze, generate, report) ...

    # Since the CLI logic wasn't the focus of the fix, we can use the __main__ block
    # to demonstrate the functionality if the script is run directly without arguments.
    if len(sys.argv) > 1:
        # Handle CLI arguments (implementation omitted for brevity)
        print("CLI argument handling not fully shown in this corrected snippet.")
        # args = parser.parse_args()
        # ... handle commands ...
        pass
    else:
        print("--- Running MUD Generation Demonstration ---")
        # Demonstrate with dummy data
        dummy_results = {
            "elaborations": [
                {
                    "base_strategy": "Counting_All",
                    "elaborated_strategy": "Counting_On",
                    "shared_patterns": ["incremental_counting"],
                    "type": "intra_categorial",
                    "confidence": 0.8
                },
                 {
                    "base_strategy": "Counting_On",
                    "elaborated_strategy": "Addition_by_Decomposition_Long_Name_Example",
                    "shared_patterns": ["base_decomposition", "counting_loop"],
                    "type": "intra_categorial",
                    "confidence": 0.6
                },
                {
                    "base_strategy": "Counting_All",
                    "elaborated_strategy": "Skip_Counting",
                    "shared_patterns": ["iterative_arithmetic"],
                    "type": "intra_categorial",
                    "confidence": 0.7
                }
            ]
        }

        mud_gen = MUDGenerator(dummy_results)
        diagrams = mud_gen.generate_mud_diagrams()

        # Display the generated TikZ code
        if diagrams:
            print("\nGenerated TikZ (adhering to MUD rules):")
            print("-" * 30)
            for key in diagrams:
                print(f"--- Diagram for {key} ---")
                print(diagrams[key]['tikz_diagram'])
            print("-" * 30)

        # Display the generated LaTeX Report
        print("\nGenerated LaTeX Report (excerpt):")
        report_gen = ReportGenerator(dummy_results, diagrams)
        latex_report = report_gen.generate_latex_report()
        print("-" * 30)
        print(latex_report[:2000]) # Print excerpt
        print("...")
        print(latex_report[-1000:])
        print("-" * 30)

if __name__ == "__main__":
    main()