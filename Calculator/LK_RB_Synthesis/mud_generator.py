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
    """Represents a detected computational pattern/subroutine."""
    name: str
    operation_type: str  # 'counting', 'decomposition', 'adjustment', etc.
    register_operations: List[str]
    state_transitions: List[str]
    strategies_using: Set[str] = field(default_factory=set)

@dataclass
class AlgorithmicElaboration:
    base_strategy: str
    elaborated_strategy: str
    shared_patterns: Set[str]
    elaboration_type: str
    confidence: float

class AutomatonAnalyzer:
    """Analyzes automaton implementations to detect patterns and relationships."""

    def __init__(self, automata_dir: str):
        self.automata_dir = automata_dir
        self.patterns: Dict[str, ComputationalPattern] = {}
        self.elaborations: List[AlgorithmicElaboration] = []
        self.strategy_patterns: Dict[str, Set[str]] = defaultdict(set)
        self.strategy_metadata: Dict[str, Any] = {}  # NEW: Store metadata from automata

    def analyze_all_automata(self) -> Dict[str, Any]:
        """Main analysis pipeline."""
        print("🔬 Starting Automated Automaton Analysis", file=sys.stderr)
        print("=" * 50, file=sys.stderr)

        # Step 1: Extract patterns from all automata
        self._extract_patterns_from_automata()

        # Step 2: Extract metadata from automata (NEW)
        self._extract_metadata_from_automata()

        # Step 3: Detect algorithmic elaborations
        self._detect_elaborations()

        # Step 4: Generate analysis report
        return self._generate_analysis_report()

    def _extract_patterns_from_automata(self):
        """Extract computational patterns from automaton source code."""
        print("\n📋 Extracting Computational Patterns...", file=sys.stderr)
        for operation_dir in ['addition', 'subtraction', 'multiplication', 'division']:
            op_path = os.path.join(self.automata_dir, operation_dir)
            if not os.path.exists(op_path):
                continue
            for filename in os.listdir(op_path):
                if filename.endswith('.py') and not filename.startswith('__'):
                    strategy_id = filename.replace('.py', '').replace('SAR_', '').replace('SMR_', '')
                    filepath = os.path.join(op_path, filename)
                    try:
                        patterns = self._analyze_single_automaton(filepath, strategy_id, operation_dir)
                        self.strategy_patterns[strategy_id] = patterns
                        print(f"✅ Analyzed {strategy_id}: {len(patterns)} patterns found", file=sys.stderr)
                    except Exception as e:
                        print(f"❌ Error analyzing {strategy_id}: {e}", file=sys.stderr)

    def _extract_metadata_from_automata(self):
        """Extract rich metadata (metaphors, inferences) from automaton instances."""
        print("\n📚 Extracting Strategy Metadata (Metaphors, Inferences)...", file=sys.stderr)

        for operation_dir in ['addition', 'subtraction', 'multiplication', 'division']:
            op_path = os.path.join(self.automata_dir, operation_dir)
            if not os.path.exists(op_path):
                continue

            for filename in os.listdir(op_path):
                if filename.endswith('.py') and not filename.startswith('__'):
                    strategy_id = filename.replace('.py', '').replace('SAR_', '').replace('SMR_', '')
                    filepath = os.path.join(op_path, filename)

                    try:
                        metadata = self._load_metadata_from_file(filepath, operation_dir, filename)
                        if metadata:
                            self.strategy_metadata[strategy_id] = metadata
                            metaphor_count = len(metadata.get('metaphors', []))
                            inference_count = len(metadata.get('inferences', []))
                            print(f"✅ Loaded metadata for {strategy_id}: {metaphor_count} metaphors, {inference_count} inferences", file=sys.stderr)
                    except Exception as e:
                        print(f"⚠️  Could not load metadata for {strategy_id}: {e}", file=sys.stderr)

    def _load_metadata_from_file(self, filepath: str, operation_dir: str, filename: str) -> Dict[str, Any]:
        """Load metadata by importing and instantiating the automaton class."""
        import importlib.util

        # Import the module
        spec = importlib.util.spec_from_file_location("automaton_module", filepath)
        if spec and spec.loader:
            module = importlib.util.module_from_spec(spec)
            sys.modules["automaton_module"] = module
            spec.loader.exec_module(module)

            # Find the automaton class (ends with 'Automaton')
            automaton_class = None
            for name in dir(module):
                obj = getattr(module, name)
                if isinstance(obj, type) and name.endswith('Automaton') and name != 'BaseAutomaton':
                    automaton_class = obj
                    break

            if automaton_class:
                # Create a dummy instance to access metadata
                try:
                    instance = automaton_class(inputs={'A': 0, 'B': 0, 'M': 0, 'S': 0, 'D': 0})
                    metadata_obj = instance.metadata

                    # Convert metadata dataclass to dict
                    return {
                        'strategy_id': metadata_obj.strategy_id,
                        'strategy_name': metadata_obj.strategy_name,
                        'description': metadata_obj.description,
                        'metaphors': [
                            {
                                'name': m.name,
                                'source_domain': m.source_domain,
                                'target_domain': m.target_domain,
                                'entailments': m.entailments
                            } for m in metadata_obj.metaphors
                        ],
                        'inferences': [
                            {
                                'name': i.name,
                                'premise': i.premise,
                                'conclusion': i.conclusion,
                                'prerequisites': i.prerequisites
                            } for i in metadata_obj.inferences
                        ],
                        'visualization_hints': metadata_obj.visualization_hints,
                        'deployed_vocabulary': metadata_obj.deployed_vocabulary if hasattr(metadata_obj, 'deployed_vocabulary') else ''
                    }
                except Exception as e:
                    # Some automata may not have metadata or may fail to instantiate
                    return None

        return None

    def _analyze_single_automaton(self, filepath: str, strategy_id: str, operation: str) -> Set[str]:
        """Analyze a single automaton file to extract patterns."""
        with open(filepath, 'r') as f:
            source_code = f.read()
        tree = ast.parse(source_code)

        patterns_found = set()
        register_ops = []
        state_methods = []

        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                for item in node.body:
                    if isinstance(item, ast.FunctionDef) and item.name.startswith('execute_'):
                        state_name = item.name.replace('execute_', '')
                        operations = self._extract_operations_from_method(item)
                        register_ops.extend(operations)
                        patterns = self._detect_patterns_in_method(item, state_name, operations)
                        patterns_found.update(patterns)
                        for pattern_name in patterns:
                            if pattern_name not in self.patterns:
                                self.patterns[pattern_name] = ComputationalPattern(
                                    name=pattern_name,
                                    operation_type=self._classify_pattern(pattern_name),
                                    register_operations=[],
                                    state_transitions=[]
                                )
                            self.patterns[pattern_name].strategies_using.add(strategy_id)
        return patterns_found

    def _extract_operations_from_method(self, method_node: ast.FunctionDef) -> List[str]:
        """Extract register operations from a method, including conditionals."""
        operations = []
        for node in ast.walk(method_node):
            if isinstance(node, ast.Assign):
                for target in node.targets:
                    if isinstance(target, ast.Name):
                        operations.append(f"{target.id} = {self._extract_value(node.value)}")
            elif isinstance(node, ast.AugAssign):
                if isinstance(node.target, ast.Name):
                    target = node.target.id
                    if isinstance(node.op, ast.Add):
                        operations.append(f"{target} += {self._extract_value(node.value)}")
                    elif isinstance(node.op, ast.Sub):
                        operations.append(f"{target} -= {self._extract_value(node.value)}")
            elif isinstance(node, ast.Call):
                if isinstance(node.func, ast.Attribute):
                    if node.func.attr == 'transition':
                        operations.append(f"transition: {self._extract_call_args(node)}")
                    elif node.func.attr == '_record_history':
                        operations.append(f"record_history: {self._extract_call_args(node)}")
        return operations

    def _extract_value(self, node: ast.AST) -> str:
        """Extract value from AST node."""
        if isinstance(node, ast.Constant):
            return str(node.value)
        elif isinstance(node, ast.Num):
            return str(node.n)
        elif isinstance(node, ast.Name):
            return node.id
        elif isinstance(node, ast.Attribute):
            return f"{node.attr}"
        elif isinstance(node, ast.BinOp):
            left = self._extract_value(node.left)
            right = self._extract_value(node.right)
            if isinstance(node.op, ast.Add):
                return f"{left} + {right}"
            elif isinstance(node.op, ast.Sub):
                return f"{left} - {right}"
            elif isinstance(node.op, ast.Mult):
                return f"{left} * {right}"
            elif isinstance(node.op, ast.Div):
                return f"{left} // {right}"
            elif isinstance(node.op, ast.Mod):
                return f"{left} % {right}"
        return "complex_expr"

    def _extract_call_args(self, call_node: ast.Call) -> str:
        """Extract arguments from a function call."""
        args = []
        for arg in call_node.args:
            if isinstance(arg, ast.Constant):
                args.append(f"'{arg.value}'")
            elif isinstance(arg, ast.Str):
                args.append(f"'{arg.s}'")
            elif isinstance(arg, ast.Name):
                args.append(arg.id)
            elif isinstance(arg, ast.Num):
                args.append(str(arg.n))
            else:
                args.append("expr")
        return ", ".join(args)

    def _detect_patterns_in_method(self, method_node: ast.FunctionDef, state_name: str, operations: List[str]) -> Set[str]:
        """Detect computational patterns in a method."""
        patterns = set()
        method_source = self._get_method_source(method_node)

        if self._is_counting_loop_pattern(method_source, operations):
            patterns.add("counting_loop")
        if self._is_decomposition_pattern(operations) or '//' in method_source or '%' in method_source:
            patterns.add("base_decomposition")
        if self._is_adjustment_pattern(operations) or 'TargetBase' in method_source or 'K =' in method_source:
            patterns.add("value_adjustment")
        if self._is_iterative_arithmetic(operations) or 'Sum += ' in method_source or 'Current += ' in method_source:
            patterns.add("iterative_arithmetic")
        if self._is_state_based_counting(state_name, method_source):
            patterns.add("incremental_counting")
        if self._is_decomposition_reconstruction_pattern(method_source):
            patterns.add("decomposition_reconstruction")
        return patterns

    def _get_method_source(self, method_node: ast.FunctionDef) -> str:
        """Extract source code from method node."""
        return " ".join([str(op) for op in self._extract_operations_from_method(method_node)])

    def _is_counting_loop_pattern(self, method_source: str, operations: List[str]) -> bool:
        """Detect state-based counting loops."""
        has_counter = any('Count' in op for op in operations)
        has_increment = any('+=' in op for op in operations)
        has_comparison = '<' in method_source or '>' in method_source
        has_conditional = 'if' in method_source or 'while' in method_source
        return has_counter and has_increment and (has_comparison or has_conditional)

    def _is_decomposition_pattern(self, operations: List[str]) -> bool:
        """Detect base decomposition patterns."""
        return any('//' in op or '%' in op for op in operations)

    def _is_adjustment_pattern(self, operations: List[str]) -> bool:
        """Detect value adjustment patterns."""
        return any('TargetBase' in op or 'K =' in op for op in operations)

    def _is_iterative_arithmetic(self, operations: List[str]) -> bool:
        """Detect iterative arithmetic patterns."""
        return any('Sum += ' in op or 'Current += ' in op for op in operations)

    def _is_state_based_counting(self, state_name: str, method_source: str) -> bool:
        """Detect state-based counting patterns."""
        counting_states = ['inc_tens', 'inc_hundreds', 'add_bases', 'add_ones', 'loop_K', 'count']
        return any(state in state_name.lower() for state in counting_states)

    def _is_decomposition_reconstruction_pattern(self, method_source: str) -> bool:
        """Detect patterns that decompose and reconstruct values."""
        return ('//' in method_source and '%' in method_source) or \
               ('BaseCounter' in method_source and 'OneCounter' in method_source)

    def _classify_pattern(self, pattern_name: str) -> str:
        """Classify a pattern by its computational type."""
        classifications = {
            "counting_loop": "counting",
            "base_decomposition": "decomposition",
            "value_adjustment": "adjustment",
            "iterative_arithmetic": "arithmetic",
            "incremental_counting": "counting"
        }
        return classifications.get(pattern_name, "general")

    def _detect_elaborations(self):
        """Detect algorithmic elaborations based on shared patterns."""
        print("\n🔗 Detecting Algorithmic Elaborations...", file=sys.stderr)
        strategy_list = list(self.strategy_patterns.keys())
        for i, strategy_a in enumerate(strategy_list):
            for strategy_b in strategy_list[i+1:]:
                shared_patterns = self.strategy_patterns[strategy_a] & self.strategy_patterns[strategy_b]
                if shared_patterns:
                    op_a = self._get_operation_type(strategy_a)
                    op_b = self._get_operation_type(strategy_b)
                    elaboration_type = "intra_categorial" if op_a == op_b else "inter_categorial"
                    confidence = len(shared_patterns) / max(len(self.strategy_patterns[strategy_a]),
                                                          len(self.strategy_patterns[strategy_b]))
                    base_strategy, elab_strategy = self._determine_elaboration_direction(
                        strategy_a, strategy_b, shared_patterns
                    )
                    elaboration = AlgorithmicElaboration(
                        base_strategy=base_strategy,
                        elaborated_strategy=elab_strategy,
                        shared_patterns=shared_patterns,
                        elaboration_type=elaboration_type,
                        confidence=confidence
                    )
                    self.elaborations.append(elaboration)

    def _get_operation_type(self, strategy_id: str) -> str:
        """Determine operation type from strategy ID."""
        if any(keyword in strategy_id.upper() for keyword in ['ADD', 'COUNTING']):
            return 'addition'
        elif any(keyword in strategy_id.upper() for keyword in ['SUB', 'SLIDING']):
            return 'subtraction'
        elif any(keyword in strategy_id.upper() for keyword in ['MULT', 'CBO']):
            return 'multiplication'
        elif any(keyword in strategy_id.upper() for keyword in ['DIV', 'DEALING']):
            return 'division'
        return 'unknown'

    def _determine_elaboration_direction(self, strategy_a: str, strategy_b: str, shared_patterns: Set[str]) -> Tuple[str, str]:
        """Determine which strategy elaborates which based on pattern analysis."""
        unique_a = len(self.strategy_patterns[strategy_a] - shared_patterns)
        unique_b = len(self.strategy_patterns[strategy_b] - shared_patterns)
        if unique_a <= unique_b:
            return strategy_a, strategy_b
        else:
            return strategy_b, strategy_a

    def _analyze_metaphor_sharing(self) -> Dict[str, List[str]]:
        """Find strategies sharing conceptual metaphors."""
        metaphor_to_strategies = defaultdict(list)

        for strategy, metadata in self.strategy_metadata.items():
            for metaphor in metadata.get('metaphors', []):
                metaphor_name = metaphor['name']
                metaphor_to_strategies[metaphor_name].append(strategy)

        return dict(metaphor_to_strategies)

    def _generate_analysis_report(self) -> Dict[str, Any]:
        """Generate comprehensive analysis report."""
        # Analyze metaphor sharing
        metaphor_sharing = self._analyze_metaphor_sharing()

        print(f"\n📊 Analysis Complete:", file=sys.stderr)
        print(f"   • {len(self.patterns)} computational patterns detected", file=sys.stderr)
        print(f"   • {len(self.elaborations)} algorithmic elaborations identified", file=sys.stderr)
        print(f"   • {len(self.strategy_metadata)} strategies with rich metadata", file=sys.stderr)
        print(f"   • {len(metaphor_sharing)} unique embodied metaphors found", file=sys.stderr)

        return {
            "patterns": {
                name: {
                    "type": pattern.operation_type,
                    "strategies_using": list(pattern.strategies_using),
                    "usage_count": len(pattern.strategies_using)
                }
                for name, pattern in self.patterns.items()
            },
            "elaborations": [
                {
                    "base_strategy": elab.base_strategy,
                    "elaborated_strategy": elab.elaborated_strategy,
                    "shared_patterns": list(elab.shared_patterns),
                    "type": elab.elaboration_type,
                    "confidence": elab.confidence
                }
                for elab in self.elaborations
            ],
            "strategy_patterns": {
                strategy: list(patterns)
                for strategy, patterns in self.strategy_patterns.items()
            },
            "strategy_metadata": self.strategy_metadata,  # Include rich metadata
            "metaphor_sharing": metaphor_sharing  # NEW: Include metaphor analysis
        }


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


class ReportGenerator:
    """Generates reports in multiple formats from analysis results."""

    def __init__(self, analysis_results: Dict[str, Any], mud_diagrams: Dict[str, Any] = None):
        self.analysis_results = analysis_results
        self.mud_diagrams = mud_diagrams or {}

    def generate_markdown_report(self, strategy_name: str = None) -> str:
        """Generate a Markdown report for a specific strategy or general overview."""
        lines = [
            "# Algorithmic Elaboration Analysis Report",
            "",
            f"Generated on: {self._get_timestamp()}",
            "",
            "## Overview",
            "",
            f"- **Computational Patterns Detected**: {len(self.analysis_results.get('patterns', {}))}",
            f"- **Algorithmic Elaborations Found**: {len(self.analysis_results.get('elaborations', []))}",
            f"- **MUD Diagrams Generated**: {len(self.mud_diagrams)}",
            ""
        ]

        if strategy_name:
            lines.extend(self._generate_strategy_report(strategy_name))
        else:
            lines.extend(self._generate_overview_report())

        # Add MUD diagrams section if diagrams are available
        if self.mud_diagrams:
            lines.extend(self._generate_mud_diagrams_markdown_section())

        return "\n".join(lines)

    def _generate_strategy_report(self, strategy_name: str) -> List[str]:
        """Generate a detailed report for a specific strategy."""
        lines = [
            f"## Strategy Analysis: {strategy_name}",
            "",
            "### Computational Patterns Used",
        ]

        patterns = self.analysis_results.get('strategy_patterns', {}).get(strategy_name, [])
        if patterns:
            for pattern in patterns:
                pattern_info = self.analysis_results.get('patterns', {}).get(pattern, {})
                lines.append(f"- **{pattern}** ({pattern_info.get('type', 'unknown')})")
                lines.append(f"  - Used by {pattern_info.get('usage_count', 0)} other strategies")
        else:
            lines.append("- No patterns detected")

        lines.extend([
            "",
            "### Algorithmic Elaborations",
            "",
            "#### As Base Strategy:"
        ])

        base_elabs = [e for e in self.analysis_results.get('elaborations', [])
                     if e['base_strategy'] == strategy_name]
        if base_elabs:
            for elab in base_elabs:
                lines.extend([
                    f"- **Elaborates** → {elab['elaborated_strategy']}",
                    f"  - Shared patterns: {', '.join(elab['shared_patterns'])}",
                    f"  - Confidence: {elab['confidence']:.2f}",
                    ""
                ])
        else:
            lines.append("- None found")

        lines.extend([
            "",
            "#### As Elaborated Strategy:"
        ])

        elab_elabs = [e for e in self.analysis_results.get('elaborations', [])
                     if e['elaborated_strategy'] == strategy_name]
        if elab_elabs:
            for elab in elab_elabs:
                lines.extend([
                    f"- **Elaborated from** ← {elab['base_strategy']}",
                    f"  - Shared patterns: {', '.join(elab['shared_patterns'])}",
                    f"  - Confidence: {elab['confidence']:.2f}",
                    ""
                ])
        else:
            lines.append("- None found")

        return lines

    def _generate_overview_report(self) -> List[str]:
        """Generate a general overview report."""
        lines = [
            "## Computational Patterns",
            "",
            "| Pattern | Type | Usage Count | Strategies |",
            "|---------|------|-------------|------------|"
        ]

        for pattern_name, pattern_data in self.analysis_results.get('patterns', {}).items():
            strategies = ", ".join(pattern_data.get('strategies_using', [])[:3])  # Show first 3
            if len(pattern_data.get('strategies_using', [])) > 3:
                strategies += "..."
            lines.append(f"| {pattern_name} | {pattern_data.get('type', 'unknown')} | {pattern_data.get('usage_count', 0)} | {strategies} |")

        lines.extend([
            "",
            "## Key Algorithmic Elaborations",
            "",
            "| Base Strategy | Elaborated Strategy | Shared Patterns | Confidence |",
            "|---------------|---------------------|----------------|------------|"
        ])

        # Show top 10 by confidence
        elaborations = sorted(self.analysis_results.get('elaborations', []),
                            key=lambda x: x['confidence'], reverse=True)[:10]

        for elab in elaborations:
            patterns = ", ".join(elab['shared_patterns'])
            lines.extend([
                f"| {elab['base_strategy']} | {elab['elaborated_strategy']} | {patterns} | {elab['confidence']:.2f} |"
            ])

        return lines

    def generate_latex_report(self, strategy_name: str = None) -> str:
        """Generate a LaTeX report for a specific strategy or general overview."""
        lines = [
            "\\documentclass{article}",
            "\\usepackage[utf8]{inputenc}",
            "\\usepackage{geometry}",
            "\\usepackage{hyperref}",
            "\\usepackage{booktabs}",
            "\\usepackage{xcolor}",
            "\\usepackage{tikz}",
            "\\usetikzlibrary{positioning,arrows.meta}",
            "\\geometry{margin=1in}",
            "",
            "\\title{EPLE Algorithmic Elaboration Analysis Report}",
            f"\\date{{{self._get_timestamp()}}}",
            "\\author{EPLE Automated Analysis System}",
            "",
            "\\begin{document}",
            "\\maketitle",
            "",
            "\\section{Overview}",
            "",
            f"\\textbf{{Computational Patterns Detected:}} {len(self.analysis_results.get('patterns', {}))}\\\\",
            f"\\textbf{{Algorithmic Elaborations Found:}} {len(self.analysis_results.get('elaborations', []))}\\\\",
            f"\\textbf{{MUD Diagrams Generated:}} {len(self.mud_diagrams)}\\\\",
            ""
        ]

        if strategy_name:
            lines.extend(self._generate_strategy_latex_report(strategy_name))
        else:
            lines.extend(self._generate_overview_latex_report())

        # Add MUD diagrams section if diagrams are available
        if self.mud_diagrams:
            lines.extend(self._generate_mud_diagrams_latex_section())

        lines.extend([
            "",
            "\\end{document}"
        ])

        return "\n".join(lines)

    def _generate_strategy_latex_report(self, strategy_name: str) -> List[str]:
        """Generate a detailed LaTeX report for a specific strategy."""
        # Escape underscores in strategy name for LaTeX
        escaped_strategy = strategy_name.replace('_', '\\_')
        
        lines = [
            f"\\section{{Strategy Analysis: {escaped_strategy}}}",
            "",
            "\\subsection{Computational Patterns Used}",
            ""
        ]

        patterns = self.analysis_results.get('strategy_patterns', {}).get(strategy_name, [])
        if patterns:
            lines.append("\\begin{itemize}")
            for pattern in patterns:
                # Escape underscores in pattern names for LaTeX
                escaped_pattern = pattern.replace('_', '\\_')
                pattern_info = self.analysis_results.get('patterns', {}).get(pattern, {})
                lines.append(f"\\item \\textbf{{{escaped_pattern}}} ({pattern_info.get('type', 'unknown')})")
                lines.append(f"  \\textit{{Used by {pattern_info.get('usage_count', 0)} other strategies}}")
            lines.append("\\end{itemize}")
        else:
            lines.append("No patterns detected.")

        lines.extend([
            "",
            "\\subsection{Algorithmic Elaborations}",
            "",
            "\\subsubsection{As Base Strategy:}",
            ""
        ])

        base_elabs = [e for e in self.analysis_results.get('elaborations', [])
                     if e['base_strategy'] == strategy_name]
        if base_elabs:
            lines.append("\\begin{itemize}")
            for elab in base_elabs:
                # Escape underscores in strategy names and patterns for LaTeX
                elab_strategy = elab['elaborated_strategy'].replace('_', '\\_')
                patterns = ", ".join(elab['shared_patterns']).replace('_', '\\_')
                lines.extend([
                    f"\\item \\textbf{{Elaborates}} $\\rightarrow$ {elab_strategy}",
                    f"  \\textit{{Shared patterns: {patterns}}}",
                    f"  \\textit{{Confidence: {elab['confidence']:.2f}}}",
                    ""
                ])
            lines.append("\\end{itemize}")
        else:
            lines.append("None found.")

        lines.extend([
            "",
            "\\subsubsection{As Elaborated Strategy:}",
            ""
        ])

        elab_elabs = [e for e in self.analysis_results.get('elaborations', [])
                     if e['elaborated_strategy'] == strategy_name]
        if elab_elabs:
            lines.append("\\begin{itemize}")
            for elab in elab_elabs:
                # Escape underscores in strategy names and patterns for LaTeX
                base_strategy = elab['base_strategy'].replace('_', '\\_')
                patterns = ", ".join(elab['shared_patterns']).replace('_', '\\_')
                lines.extend([
                    f"\\item \\textbf{{Elaborated from}} $\\leftarrow$ {base_strategy}",
                    f"  \\textit{{Shared patterns: {patterns}}}",
                    f"  \\textit{{Confidence: {elab['confidence']:.2f}}}",
                    ""
                ])
            lines.append("\\end{itemize}")
        else:
            lines.append("None found.")

        return lines

    def _generate_overview_latex_report(self) -> List[str]:
        """Generate a general overview LaTeX report."""
        lines = [
            "\\section{Computational Patterns}",
            "",
            "\\begin{tabular}{@{}lll@{}}",
            "\\toprule",
            "\\textbf{Pattern} & \\textbf{Type} & \\textbf{Usage Count} \\\\",
            "\\midrule"
        ]

        for pattern_name, pattern_data in self.analysis_results.get('patterns', {}).items():
            # Escape underscores in pattern names for LaTeX
            escaped_pattern = pattern_name.replace('_', '\\_')
            lines.append(f"{escaped_pattern} & {pattern_data.get('type', 'unknown')} & {pattern_data.get('usage_count', 0)} \\\\")

        lines.extend([
            "\\bottomrule",
            "\\end{tabular}",
            "",
            "\\section{Key Algorithmic Elaborations}",
            "",
            "\\begin{tabular}{@{}llll@{}}",
            "\\toprule",
            "\\textbf{Base Strategy} & \\textbf{Elaborated Strategy} & \\textbf{Shared Patterns} & \\textbf{Confidence} \\\\",
            "\\midrule"
        ])

        # Show top 10 by confidence
        elaborations = sorted(self.analysis_results.get('elaborations', []),
                            key=lambda x: x['confidence'], reverse=True)[:10]

        for elab in elaborations:
            # Escape underscores in strategy names and patterns for LaTeX
            base_strategy = elab['base_strategy'].replace('_', '\\_')
            elab_strategy = elab['elaborated_strategy'].replace('_', '\\_')
            patterns = ", ".join(elab['shared_patterns']).replace('_', '\\_')
            lines.append(f"{base_strategy} & {elab_strategy} & {patterns} & {elab['confidence']:.2f} \\\\")

        lines.extend([
            "\\bottomrule",
            "\\end{tabular}"
        ])

        return lines

    def _generate_mud_diagrams_latex_section(self) -> List[str]:
        """Generate a LaTeX section containing all MUD diagrams."""
        lines = [
            "",
            "\\section{Meaning-Use Diagrams}",
            "",
            "The following diagrams illustrate the algorithmic elaborations detected in the analysis.",
            "Each diagram shows strategies connected by shared computational patterns.",
            ""
        ]

        for operation, diagram_data in self.mud_diagrams.items():
            # Create a subsection for each operation
            operation_title = operation.replace('_', ' ').title()
            lines.extend([
                f"\\subsection{{{operation_title}}}",
                "",
                f"\\textbf{{Operation:}} {operation_title}\\\\",
                f"\\textbf{{Strategies Analyzed:}} {len(diagram_data.get('strategies', []))}\\\\",
                f"\\textbf{{Elaborations Detected:}} {len(diagram_data.get('elaborations', []))}\\\\",
                "",
                "\\begin{center}",
                diagram_data.get('tikz_diagram', ''),
                "\\end{center}",
                "",
                "\\textbf{Summary:}\\\\",
                "\\begin{verbatim}",
                diagram_data.get('summary', ''),
                "\\end{verbatim}",
                "",
                "\\newpage"
            ])

        return lines

    def _generate_mud_diagrams_markdown_section(self) -> List[str]:
        """Generate a Markdown section containing MUD diagram information."""
        lines = [
            "",
            "## Meaning-Use Diagrams",
            "",
            "The following diagrams illustrate the algorithmic elaborations detected in the analysis. Each diagram shows strategies connected by shared computational patterns.",
            ""
        ]

        for operation, diagram_data in self.mud_diagrams.items():
            # Create a section for each operation
            operation_title = operation.replace('_', ' ').title()
            strategies = diagram_data.get('strategies', [])
            elaborations = diagram_data.get('elaborations', [])
            
            lines.extend([
                f"### {operation_title}",
                "",
                f"**Operation:** {operation_title}",
                f"**Strategies Analyzed:** {len(strategies)}",
                f"**Elaborations Detected:** {len(elaborations)}",
                "",
                "#### Strategies:",
            ])
            
            # List strategies
            for strategy in strategies:
                lines.append(f"- {strategy}")
            
            lines.extend([
                "",
                "#### Key Elaborations:"
            ])
            
            # List elaborations
            for elab in elaborations[:5]:  # Show top 5
                lines.extend([
                    f"- **{elab['base_strategy']}** → **{elab['elaborated_strategy']}**",
                    f"  - Shared patterns: {', '.join(elab['shared_patterns'])}",
                    f"  - Confidence: {elab['confidence']:.2f}"
                ])
            
            if len(elaborations) > 5:
                lines.append(f"- ... and {len(elaborations) - 5} more elaborations")
            
            lines.extend([
                "",
                "#### TikZ Diagram Code:",
                "",
                "```latex",
                diagram_data.get('tikz_diagram', ''),
                "```",
                "",
                "---"
            ])

        return lines

    def generate_html_report(self, strategy_name: str = None) -> str:
        """Generate an HTML report for a specific strategy or general overview."""
        lines = [
            "<!DOCTYPE html>",
            "<html lang='en'>",
            "<head>",
            "    <meta charset='UTF-8'>",
            "    <meta name='viewport' content='width=device-width, initial-scale=1.0'>",
            "    <title>EPLE Algorithmic Elaboration Analysis Report</title>",
            "    <style>",
            "        body { font-family: Arial, sans-serif; margin: 40px; }",
            "        h1, h2, h3 { color: #2c3e50; }",
            "        table { border-collapse: collapse; width: 100%; margin: 20px 0; }",
            "        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
            "        th { background-color: #f2f2f2; }",
            "        .pattern { background-color: #e8f4f8; }",
            "        .elaboration { background-color: #f8e8e8; }",
            "        .confidence-high { color: #27ae60; font-weight: bold; }",
            "        .confidence-medium { color: #f39c12; }",
            "        .confidence-low { color: #e74c3c; }",
            "    </style>",
            "</head>",
            "<body>",
            "    <h1>EPLE Algorithmic Elaboration Analysis Report</h1>",
            f"    <p><strong>Generated on:</strong> {self._get_timestamp()}</p>",
            "    <h2>Overview</h2>",
            "    <ul>",
            f"        <li><strong>Computational Patterns Detected:</strong> {len(self.analysis_results.get('patterns', {}))}</li>",
            f"        <li><strong>Algorithmic Elaborations Found:</strong> {len(self.analysis_results.get('elaborations', []))}</li>",
            f"        <li><strong>MUD Diagrams Generated:</strong> {len(self.mud_diagrams)}</li>",
            "    </ul>"
        ]

        if strategy_name:
            lines.extend(self._generate_strategy_html_report(strategy_name))
        else:
            lines.extend(self._generate_overview_html_report())

        # Add MUD diagrams section if diagrams are available
        if self.mud_diagrams:
            lines.extend(self._generate_mud_diagrams_html_section())

        lines.extend([
            "</body>",
            "</html>"
        ])

        return "\n".join(lines)

    def _generate_strategy_html_report(self, strategy_name: str) -> List[str]:
        """Generate a detailed HTML report for a specific strategy."""
        lines = [
            f"    <h2>Strategy Analysis: {strategy_name}</h2>",
            "    <h3>Computational Patterns Used</h3>",
            "    <ul>"
        ]

        patterns = self.analysis_results.get('strategy_patterns', {}).get(strategy_name, [])
        if patterns:
            for pattern in patterns:
                pattern_info = self.analysis_results.get('patterns', {}).get(pattern, {})
                lines.append(f"        <li class='pattern'><strong>{pattern}</strong> ({pattern_info.get('type', 'unknown')})")
                lines.append(f"            <br><em>Used by {pattern_info.get('usage_count', 0)} other strategies</em></li>")
        else:
            lines.append("        <li>No patterns detected.</li>")

        lines.extend([
            "    </ul>",
            "    <h3>Algorithmic Elaborations</h3>",
            "    <h4>As Base Strategy:</h4>",
            "    <ul>"
        ])

        base_elabs = [e for e in self.analysis_results.get('elaborations', [])
                     if e['base_strategy'] == strategy_name]
        if base_elabs:
            for elab in base_elabs:
                confidence_class = self._get_confidence_class(elab['confidence'])
                lines.extend([
                    f"        <li class='elaboration'><strong>Elaborates</strong> → {elab['elaborated_strategy']}",
                    f"            <br><em>Shared patterns: {', '.join(elab['shared_patterns'])}</em>",
                    f"            <br><span class='{confidence_class}'>Confidence: {elab['confidence']:.2f}</span></li>"
                ])
        else:
            lines.append("        <li>None found.</li>")

        lines.extend([
            "    </ul>",
            "    <h4>As Elaborated Strategy:</h4>",
            "    <ul>"
        ])

        elab_elabs = [e for e in self.analysis_results.get('elaborations', [])
                     if e['elaborated_strategy'] == strategy_name]
        if elab_elabs:
            for elab in elab_elabs:
                confidence_class = self._get_confidence_class(elab['confidence'])
                lines.extend([
                    f"        <li class='elaboration'><strong>Elaborated from</strong> ← {elab['base_strategy']}",
                    f"            <br><em>Shared patterns: {', '.join(elab['shared_patterns'])}</em>",
                    f"            <br><span class='{confidence_class}'>Confidence: {elab['confidence']:.2f}</span></li>"
                ])
        else:
            lines.append("        <li>None found.</li>")

        lines.append("    </ul>")
        return lines

    def _generate_overview_html_report(self) -> List[str]:
        """Generate a general overview HTML report."""
        lines = [
            "    <h2>Computational Patterns</h2>",
            "    <table>",
            "        <tr>",
            "            <th>Pattern</th>",
            "            <th>Type</th>",
            "            <th>Usage Count</th>",
            "            <th>Strategies</th>",
            "        </tr>"
        ]

        for pattern_name, pattern_data in self.analysis_results.get('patterns', {}).items():
            strategies = ", ".join(pattern_data.get('strategies_using', [])[:3])
            if len(pattern_data.get('strategies_using', [])) > 3:
                strategies += "..."
            lines.extend([
                "        <tr>",
                f"            <td>{pattern_name}</td>",
                f"            <td>{pattern_data.get('type', 'unknown')}</td>",
                f"            <td>{pattern_data.get('usage_count', 0)}</td>",
                f"            <td>{strategies}</td>",
                "        </tr>"
            ])

        lines.extend([
            "    </table>",
            "    <h2>Key Algorithmic Elaborations</h2>",
            "    <table>",
            "        <tr>",
            "            <th>Base Strategy</th>",
            "            <th>Elaborated Strategy</th>",
            "            <th>Shared Patterns</th>",
            "            <th>Confidence</th>",
            "        </tr>"
        ])

        # Show top 10 by confidence
        elaborations = sorted(self.analysis_results.get('elaborations', []),
                            key=lambda x: x['confidence'], reverse=True)[:10]

        for elab in elaborations:
            patterns = ", ".join(elab['shared_patterns'])
            confidence_class = self._get_confidence_class(elab['confidence'])
            lines.extend([
                "        <tr>",
                f"            <td>{elab['base_strategy']}</td>",
                f"            <td>{elab['elaborated_strategy']}</td>",
                f"            <td>{patterns}</td>",
                f"            <td><span class='{confidence_class}'>{elab['confidence']:.2f}</span></td>",
                "        </tr>"
            ])

        lines.append("    </table>")
        return lines

    def _generate_mud_diagrams_html_section(self) -> List[str]:
        """Generate an HTML section containing MUD diagram information."""
        lines = [
            "    <h2>Meaning-Use Diagrams</h2>",
            "    <p>The following diagrams illustrate the algorithmic elaborations detected in the analysis. Each diagram shows strategies connected by shared computational patterns.</p>"
        ]

        for operation, diagram_data in self.mud_diagrams.items():
            # Create a section for each operation
            operation_title = operation.replace('_', ' ').title()
            strategies = diagram_data.get('strategies', [])
            elaborations = diagram_data.get('elaborations', [])
            
            lines.extend([
                f"    <h3>{operation_title}</h3>",
                "    <div style='background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-radius: 5px;'>",
                f"        <p><strong>Operation:</strong> {operation_title}</p>",
                f"        <p><strong>Strategies Analyzed:</strong> {len(strategies)}</p>",
                f"        <p><strong>Elaborations Detected:</strong> {len(elaborations)}</p>",
                "        <h4>Strategies:</h4>",
                "        <ul>"
            ])
            
            # List strategies
            for strategy in strategies:
                lines.append(f"            <li>{strategy}</li>")
            
            lines.extend([
                "        </ul>",
                "        <h4>Key Elaborations:</h4>",
                "        <ul>"
            ])
            
            # List elaborations
            for elab in elaborations[:5]:  # Show top 5
                confidence_class = self._get_confidence_class(elab['confidence'])
                lines.extend([
                    f"            <li><strong>{elab['base_strategy']}</strong> → <strong>{elab['elaborated_strategy']}</strong>",
                    f"                <br><em>Shared patterns: {', '.join(elab['shared_patterns'])}</em>",
                    f"                <br><span class='{confidence_class}'>Confidence: {elab['confidence']:.2f}</span></li>"
                ])
            
            if len(elaborations) > 5:
                lines.append(f"            <li><em>... and {len(elaborations) - 5} more elaborations</em></li>")
            
            lines.extend([
                "        </ul>",
                "        <h4>Diagram Code (TikZ):</h4>",
                "        <details>",
                "            <summary>Click to view TikZ code</summary>",
                "            <pre style='background-color: #f4f4f4; padding: 10px; border-radius: 3px; font-family: monospace; white-space: pre-wrap;'>",
                diagram_data.get('tikz_diagram', ''),
                "            </pre>",
                "        </details>",
                "    </div>"
            ])

        return lines

    def _get_confidence_class(self, confidence: float) -> str:
        """Get CSS class for confidence level."""
        if confidence >= 0.8:
            return "confidence-high"
        elif confidence >= 0.5:
            return "confidence-medium"
        else:
            return "confidence-low"

    def _get_timestamp(self) -> str:
        """Get current timestamp for reports."""
        from datetime import datetime
        return datetime.now().strftime("%Y-%m-%d %H:%M:%S")


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
