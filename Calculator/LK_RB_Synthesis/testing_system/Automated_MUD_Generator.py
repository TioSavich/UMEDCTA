#!/usr/bin/env python3
"""
Automated_MUD_Generator.py: Automatically generates Meaning Use Diagrams (MUDs)
based on discovered algorithmic elaborations from automaton analysis.
"""

import json
from typing import Dict, List, Set, Tuple, Any
from collections import defaultdict
from Automaton_Analysis_Engine import AutomatonAnalyzer

class AutomatedMUDGenerator:
    """Generates MUD diagrams from algorithmic elaboration analysis."""

    def __init__(self, analysis_results: Dict[str, Any]):
        self.analysis_results = analysis_results
        self.mud_diagrams = {}

    def generate_mud_diagrams(self) -> Dict[str, Any]:
        """Generate MUD diagrams for all discovered elaborations."""
        print("🎨 Generating Automated MUD Diagrams")
        print("=" * 50)

        # Group elaborations by operation type
        operation_groups = self._group_elaborations_by_operation()

        for operation, elaborations in operation_groups.items():
            print(f"\n📊 Generating MUD for {operation}")
            mud_diagram = self._generate_operation_mud(operation, elaborations)
            self.mud_diagrams[operation] = mud_diagram

        return self.mud_diagrams

    def _group_elaborations_by_operation(self) -> Dict[str, List[Dict]]:
        """Group elaborations by their primary operation type."""
        operation_groups = defaultdict(list)

        for elab in self.analysis_results.get('elaborations', []):
            # Determine operation from strategy names
            base_op = self._extract_operation_type(elab['base_strategy'])
            elab_op = self._extract_operation_type(elab['elaborated_strategy'])

            # Use the more specific operation if they differ
            if base_op != elab_op:
                operation = f"{base_op}_to_{elab_op}"
            else:
                operation = base_op

            operation_groups[operation].append(elab)

        return operation_groups

    def _extract_operation_type(self, strategy_id: str) -> str:
        """Extract operation type from strategy ID."""
        strategy_upper = strategy_id.upper()

        if any(op in strategy_upper for op in ['ADD', 'COUNTING']):
            return 'addition'
        elif any(op in strategy_upper for op in ['SUB', 'SLIDING']):
            return 'subtraction'
        elif any(op in strategy_upper for op in ['MULT', 'CBO']):
            return 'multiplication'
        elif any(op in strategy_upper for op in ['DIV', 'DEALING']):
            return 'division'
        else:
            return 'general'

    def _generate_operation_mud(self, operation: str, elaborations: List[Dict]) -> Dict[str, Any]:
        """Generate a MUD diagram for a specific operation."""
        # Collect all strategies involved
        strategies = set()
        pattern_relationships = defaultdict(list)

        for elab in elaborations:
            strategies.add(elab['base_strategy'])
            strategies.add(elab['elaborated_strategy'])

            # Group by shared patterns
            for pattern in elab['shared_patterns']:
                pattern_relationships[pattern].append(elab)

        # Generate TikZ code
        tikz_code = self._generate_tikz_diagram(operation, list(strategies), pattern_relationships)

        return {
            'operation': operation,
            'strategies': list(strategies),
            'elaborations': elaborations,
            'pattern_relationships': dict(pattern_relationships),
            'tikz_diagram': tikz_code,
            'summary': self._generate_mud_summary(operation, elaborations)
        }

    def _generate_tikz_diagram(self, operation: str, strategies: List[str], pattern_relationships: Dict) -> str:
        """Generate TikZ code for the MUD diagram."""
        tikz_lines = [
            "\\begin{tikzpicture}[node distance=2cm and 2cm, auto]",
            f"\\node[draw, fill=blue!10, rounded corners] (title) at (0,0) {{\\textbf{{{operation.replace('_', ' ').title()} MUD}}}};",
            ""
        ]

        # Position strategies in a circle
        angle_step = 360 / len(strategies) if strategies else 1
        strategy_positions = {}

        for i, strategy in enumerate(strategies):
            angle = i * angle_step
            x = 4 * (angle * 3.14159 / 180)  # Convert to radians for positioning
            y = 3 * (angle * 3.14159 / 180)

            # Clean strategy name for display
            display_name = strategy.replace('SAR_', '').replace('_', ' ')
            strategy_positions[strategy] = f"strategy_{i}"

            tikz_lines.append(f"\\node[draw, fill=green!10, rounded corners] ({strategy_positions[strategy]}) at ({x:.2f},{y:.2f}) {{{display_name}}};")

        tikz_lines.append("")

        # Add elaboration arrows
        arrow_count = 0
        for pattern, relationships in pattern_relationships.items():
            color = self._get_pattern_color(pattern)

            for elab in relationships:
                base_pos = strategy_positions[elab['base_strategy']]
                elab_pos = strategy_positions[elab['elaborated_strategy']]

                tikz_lines.append(f"\\draw[{color}, ->, thick] ({base_pos}) -- ({elab_pos})")
                tikz_lines.append(f"    node[midway, above] {{\\scriptsize {pattern}}};")

                arrow_count += 1

        tikz_lines.extend([
            "",
            "\\end{tikzpicture}"
        ])

        return "\n".join(tikz_lines)

    def _get_pattern_color(self, pattern: str) -> str:
        """Get color for pattern arrows."""
        color_map = {
            'base_decomposition': 'red',
            'incremental_counting': 'blue',
            'counting_loop': 'green',
            'iterative_arithmetic': 'purple',
            'value_adjustment': 'orange'
        }
        return color_map.get(pattern, 'black')

    def _generate_mud_summary(self, operation: str, elaborations: List[Dict]) -> str:
        """Generate a textual summary of the MUD."""
        if not elaborations:
            return f"No algorithmic elaborations detected for {operation}."

        summary_lines = [
            f"Automated MUD Analysis for {operation.replace('_', ' ').title()}",
            "=" * 60,
            f"Total strategies analyzed: {len(set(e['base_strategy'] for e in elaborations) | set(e['elaborated_strategy'] for e in elaborations))}",
            f"Total elaborations detected: {len(elaborations)}",
            "",
            "Key Patterns Identified:"
        ]

        # Count pattern usage
        pattern_counts = defaultdict(int)
        for elab in elaborations:
            for pattern in elab['shared_patterns']:
                pattern_counts[pattern] += 1

        for pattern, count in sorted(pattern_counts.items(), key=lambda x: x[1], reverse=True):
            summary_lines.append(f"  • {pattern}: {count} relationships")

        summary_lines.extend([
            "",
            "Notable Elaborations:"
        ])

        # Show high-confidence elaborations
        high_confidence = [e for e in elaborations if e['confidence'] > 0.5]
        for elab in high_confidence[:5]:  # Show top 5
            summary_lines.append(f"  • {elab['base_strategy']} → {elab['elaborated_strategy']}")
            summary_lines.append(f"    Shared: {', '.join(elab['shared_patterns'])} (confidence: {elab['confidence']:.2f})")

        if len(high_confidence) > 5:
            summary_lines.append(f"  ... and {len(high_confidence) - 5} more high-confidence relationships")

        return "\n".join(summary_lines)

def main():
    """Run automated MUD generation."""
    # First run the analysis
    analyzer = AutomatonAnalyzer("/Users/tio/Documents/GitHub/LK_RB_Synthesis_Project/LK_RB_Synthesis/src/automata")
    analysis_results = analyzer.analyze_all_automata()

    # Then generate MUDs
    mud_generator = AutomatedMUDGenerator(analysis_results)
    mud_diagrams = mud_generator.generate_mud_diagrams()

    # Save results
    output_file = "/Users/tio/Documents/GitHub/LK_RB_Synthesis_Project/LK_RB_Synthesis/automated_mud_results.json"
    with open(output_file, 'w') as f:
        json.dump({
            'analysis_results': analysis_results,
            'mud_diagrams': mud_diagrams
        }, f, indent=2)

    print("\n💾 Results saved to:", output_file)

    # Print summary
    print("\n🎯 AUTOMATED MUD GENERATION COMPLETE")
    print("=" * 50)

    for operation, mud in mud_diagrams.items():
        print(f"\n📊 {operation.upper()}:")
        print(f"   • {len(mud['strategies'])} strategies")
        print(f"   • {len(mud['elaborations'])} elaborations")
        print(f"   • {len(mud['pattern_relationships'])} pattern types")

        # Show sample TikZ
        print("   • TikZ diagram generated ✓")