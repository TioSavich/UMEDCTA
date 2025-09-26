#!/usr/bin/env python3
"""
Automaton_Analysis_Engine.py: Automated discovery of algorithmic elaborations.

This system analyzes automaton implementations to automatically detect:
1. Shared computational patterns (subroutines)
2. Algorithmic elaboration relationships
3. Intra-categorial dependencies

It works by:
- Extracting state machines and register operations
- Identifying common patterns across strategies
- Detecting elaboration relationships based on shared subroutines
"""

import os
import sys
import ast
import inspect
from typing import Dict, List, Set, Tuple, Any
from dataclasses import dataclass, field
from collections import defaultdict
import re

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
    """Represents an algorithmic elaboration relationship."""
    base_strategy: str
    elaborated_strategy: str
    shared_patterns: Set[str]
    elaboration_type: str  # 'intra_categorial', 'inter_categorial'
    confidence: float

class AutomatonAnalyzer:
    """Analyzes automaton implementations to detect patterns and relationships."""

    def __init__(self, automata_dir: str):
        self.automata_dir = automata_dir
        self.patterns: Dict[str, ComputationalPattern] = {}
        self.elaborations: List[AlgorithmicElaboration] = []
        self.strategy_patterns: Dict[str, Set[str]] = defaultdict(set)

    def analyze_all_automata(self) -> Dict[str, Any]:
        """Main analysis pipeline."""
        print("🔬 Starting Automated Automaton Analysis")
        print("=" * 50)

        # Step 1: Extract patterns from all automata
        self._extract_patterns_from_automata()

        # Step 2: Detect algorithmic elaborations
        self._detect_elaborations()

        # Step 3: Generate analysis report
        return self._generate_analysis_report()

    def _extract_patterns_from_automata(self):
        """Extract computational patterns from automaton source code."""
        print("\n📋 Extracting Computational Patterns...")

        for operation_dir in ['addition', 'subtraction', 'multiplication', 'division']:
            op_path = os.path.join(self.automata_dir, operation_dir)
            if not os.path.exists(op_path):
                continue

            for filename in os.listdir(op_path):
                if filename.endswith('.py') and not filename.startswith('__'):
                    strategy_id = filename.replace('.py', '').replace('SAR_', '')
                    filepath = os.path.join(op_path, filename)

                    try:
                        patterns = self._analyze_single_automaton(filepath, strategy_id, operation_dir)
                        self.strategy_patterns[strategy_id] = patterns
                        print(f"✅ Analyzed {strategy_id}: {len(patterns)} patterns found")
                    except Exception as e:
                        print(f"❌ Error analyzing {strategy_id}: {e}")

    def _analyze_single_automaton(self, filepath: str, strategy_id: str, operation: str) -> Set[str]:
        """Analyze a single automaton file to extract patterns."""
        with open(filepath, 'r') as f:
            source_code = f.read()

        # Parse the AST
        tree = ast.parse(source_code)

        patterns_found = set()
        register_ops = []
        state_methods = []

        # Extract class definition and methods
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                # Look for execute_ methods (state handlers)
                for item in node.body:
                    if isinstance(item, ast.FunctionDef) and item.name.startswith('execute_'):
                        state_name = item.name.replace('execute_', '')
                        operations = self._extract_operations_from_method(item)
                        register_ops.extend(operations)

                        # Detect specific patterns
                        patterns = self._detect_patterns_in_method(item, state_name, operations)
                        patterns_found.update(patterns)

                        # Update pattern usage
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
            # Regular assignments
            if isinstance(node, ast.Assign):
                for target in node.targets:
                    if isinstance(target, ast.Name):
                        operations.append(f"{target.id} = {self._extract_value(node.value)}")

            # Augmented assignments (x += 1, etc.)
            elif isinstance(node, ast.AugAssign):
                if isinstance(node.target, ast.Name):
                    target = node.target.id
                    if isinstance(node.op, ast.Add):
                        operations.append(f"{target} += {self._extract_value(node.value)}")
                    elif isinstance(node.op, ast.Sub):
                        operations.append(f"{target} -= {self._extract_value(node.value)}")

            # Binary operations that might be assignments
            elif isinstance(node, ast.BinOp):
                # Look for patterns like: self.registers['X'] = self.registers['Y'] + 1
                if hasattr(node, 'parent') and isinstance(node.parent, ast.Assign):
                    continue  # Already handled above
                else:
                    # Check if this is part of a larger assignment
                    operations.append(f"binop: {self._extract_value(node)}")

            # Function calls that might be transitions or operations
            elif isinstance(node, ast.Call):
                if isinstance(node.func, ast.Attribute):
                    if node.func.attr == 'transition':
                        operations.append(f"transition: {self._extract_call_args(node)}")
                    elif node.func.attr == '_record_history':
                        operations.append(f"record_history: {self._extract_call_args(node)}")

        return operations

    def _extract_call_args(self, call_node: ast.Call) -> str:
        """Extract arguments from a function call."""
        args = []
        for arg in call_node.args:
            if isinstance(arg, ast.Str):
                args.append(f"'{arg.s}'")
            elif isinstance(arg, ast.Name):
                args.append(arg.id)
            elif isinstance(arg, ast.Num):
                args.append(str(arg.n))
            else:
                args.append("expr")
        return ", ".join(args)

    def _extract_value(self, node: ast.AST) -> str:
        """Extract value from AST node."""
        if isinstance(node, ast.Num):
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
                return f"{left} // {right}"  # Integer division common in automata
            elif isinstance(node.op, ast.Mod):
                return f"{left} % {right}"
        return "complex_expr"

    def _detect_patterns_in_method(self, method_node: ast.FunctionDef, state_name: str, operations: List[str]) -> Set[str]:
        """Detect computational patterns in a method."""
        patterns = set()

        # Get the source code for more detailed analysis
        method_source = self._get_method_source(method_node)

        # Pattern 1: Counting loops (state-based iteration)
        if self._is_counting_loop_pattern(method_source, operations):
            patterns.add("counting_loop")

        # Pattern 2: Base decomposition
        if self._is_decomposition_pattern(operations) or '//' in method_source or '%' in method_source:
            patterns.add("base_decomposition")

        # Pattern 3: Adjustment calculations
        if self._is_adjustment_pattern(operations) or 'TargetBase' in method_source or 'K =' in method_source:
            patterns.add("value_adjustment")

        # Pattern 4: Iterative addition/subtraction
        if self._is_iterative_arithmetic(operations) or 'Sum += ' in method_source or 'Current += ' in method_source:
            patterns.add("iterative_arithmetic")

        # Pattern 5: State-based counting transitions
        if self._is_state_based_counting(state_name, method_source):
            patterns.add("incremental_counting")

        # Pattern 6: Decomposition and reconstruction
        if self._is_decomposition_reconstruction_pattern(method_source):
            patterns.add("decomposition_reconstruction")

        return patterns

    def _get_method_source(self, method_node: ast.FunctionDef) -> str:
        """Extract source code from method node."""
        # This is a simplified approach - in practice you'd need line numbers
        # For now, we'll reconstruct from operations and state name
        return " ".join([str(op) for op in self._extract_operations_from_method(method_node)])

    def _is_counting_loop_pattern(self, method_source: str, operations: List[str]) -> bool:
        """Detect state-based counting loops."""
        # Look for patterns that indicate iterative counting
        has_counter = any('Count' in op for op in operations)
        has_increment = any('+=' in op for op in operations)
        has_comparison = '<' in method_source or '>' in method_source
        has_conditional = 'if' in method_source or 'while' in method_source

        return has_counter and has_increment and (has_comparison or has_conditional)

    def _is_state_based_counting(self, state_name: str, method_source: str) -> bool:
        """Detect state-based counting patterns."""
        counting_states = ['inc_tens', 'inc_hundreds', 'add_bases', 'add_ones', 'loop_K', 'count']
        return any(state in state_name.lower() for state in counting_states)

    def _is_decomposition_reconstruction_pattern(self, method_source: str) -> bool:
        """Detect patterns that decompose and reconstruct values."""
        return ('//' in method_source and '%' in method_source) or \
               ('BaseCounter' in method_source and 'OneCounter' in method_source)

    def _is_counting_pattern(self, operations: List[str]) -> bool:
        """Detect if operations represent a counting pattern."""
        count_ops = [op for op in operations if '+=' in op or '-=' in op]
        return len(count_ops) >= 2  # Multiple increment/decrement operations

    def _is_decomposition_pattern(self, operations: List[str]) -> bool:
        """Detect base decomposition patterns."""
        return any('//' in op or '%' in op for op in operations)

    def _is_adjustment_pattern(self, operations: List[str]) -> bool:
        """Detect value adjustment patterns."""
        return any('TargetBase' in op or 'K =' in op for op in operations)

    def _is_iterative_arithmetic(self, operations: List[str]) -> bool:
        """Detect iterative arithmetic patterns."""
        return any('Sum += ' in op or 'Current += ' in op for op in operations)

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
        print("\n🔗 Detecting Algorithmic Elaborations...")

        strategy_list = list(self.strategy_patterns.keys())

        for i, strategy_a in enumerate(strategy_list):
            for strategy_b in strategy_list[i+1:]:
                shared_patterns = self.strategy_patterns[strategy_a] & self.strategy_patterns[strategy_b]

                if shared_patterns:
                    # Determine operation types
                    op_a = self._get_operation_type(strategy_a)
                    op_b = self._get_operation_type(strategy_b)

                    elaboration_type = "intra_categorial" if op_a == op_b else "inter_categorial"
                    confidence = len(shared_patterns) / max(len(self.strategy_patterns[strategy_a]),
                                                          len(self.strategy_patterns[strategy_b]))

                    # Determine elaboration direction based on pattern complexity
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
        # Simple heuristic: strategy with fewer unique patterns is the base
        unique_a = len(self.strategy_patterns[strategy_a] - shared_patterns)
        unique_b = len(self.strategy_patterns[strategy_b] - shared_patterns)

        if unique_a <= unique_b:
            return strategy_a, strategy_b
        else:
            return strategy_b, strategy_a

    def _generate_analysis_report(self) -> Dict[str, Any]:
        """Generate comprehensive analysis report."""
        print(f"\n📊 Analysis Complete:")
        print(f"   • {len(self.patterns)} computational patterns detected")
        print(f"   • {len(self.elaborations)} algorithmic elaborations identified")

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
            }
        }

def main():
    """Run the automated analysis."""
    automata_dir = "/Users/tio/Documents/GitHub/LK_RB_Synthesis_Project/LK_RB_Synthesis/src/automata"

    analyzer = AutomatonAnalyzer(automata_dir)
    results = analyzer.analyze_all_automata()

    # Print summary
    print("\n🎯 AUTOMATED ANALYSIS RESULTS")
    print("=" * 50)

    print(f"\n🔍 Computational Patterns Found:")
    for pattern_name, pattern_data in results['patterns'].items():
        print(f"   • {pattern_name} ({pattern_data['type']}): used by {pattern_data['usage_count']} strategies")

    print(f"\n🔗 Algorithmic Elaborations Detected:")
    for elab in results['elaborations']:
        if elab['confidence'] > 0.3:  # Only show high-confidence relationships
            print(f"   • {elab['base_strategy']} → {elab['elaborated_strategy']}")
            print(f"     Shared: {', '.join(elab['shared_patterns'])}")
            print(".2f")

    print(f"\n📈 Strategy Pattern Usage:")
    for strategy, patterns in results['strategy_patterns'].items():
        print(f"   • {strategy}: {', '.join(patterns)}")

if __name__ == "__main__":
    main()