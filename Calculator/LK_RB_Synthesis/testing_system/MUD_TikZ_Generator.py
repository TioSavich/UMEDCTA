#!/usr/bin/env python3
"""
MUD_TikZ_Generator.py: Generate Meaning-Use Diagrams in TikZ format following Brandom's conventions.

This script creates modular, focused diagrams showing developmental trajectories
from counting to division, capturing both intra-categorial and inter-categorial elaborations.
"""

import os
import json
import sys
from typing import List, Dict, Set
from dataclasses import dataclass

@dataclass
class StrategyNode:
    """Represents a strategy in the MUD."""
    id: str
    name: str
    operation: str  # 'addition', 'subtraction', 'multiplication', 'division'
    description: str
    prerequisites: List[str]
    elaborations: List[str]
    vocabulary: str

@dataclass
class AlgorithmicElaboration:
    """Represents an algorithmic elaboration relationship."""
    base_strategy: str
    elaborated_strategy: str
    description: str
    operation_type: str

class MUDTikZGenerator:
    """Generates TikZ diagrams for Meaning-Use Diagrams."""

    def __init__(self, metadata_path: str):
        self.metadata_path = metadata_path
        self.strategies = {}
        self.elaborations = []
        self.load_metadata()

    def load_metadata(self):
        """Load strategy metadata and extract relationships."""
        with open(self.metadata_path, 'r') as f:
            data = json.load(f)

        # Process strategies
        for item in data:
            strategy_id = item['strategy_id']
            strategy_name = item['strategy_name']

            # Determine operation type from strategy name/id
            operation = self._determine_operation(strategy_name, strategy_id)

            # Extract prerequisites
            prerequisites = []
            for prereq in item.get('pp_necessities', []):
                if 'id' in prereq:
                    prerequisites.append(prereq['id'])

            # Extract elaborations
            elaborations = []
            for elab in item.get('pp_sufficiencies_alg_elaboration', []):
                if 'id' in elab:
                    elaborations.append(elab['id'])

            # Extract LX relations (cross-strategy elaborations)
            for lx_rel in item.get('lx_relations', []):
                if 'elaborates_strategy_id' in lx_rel:
                    self.elaborations.append(AlgorithmicElaboration(
                        base_strategy=lx_rel['elaborates_strategy_id'],
                        elaborated_strategy=strategy_id,
                        description=lx_rel.get('explanation', ''),
                        operation_type='lx_relation'
                    ))

            self.strategies[strategy_id] = StrategyNode(
                id=strategy_id,
                name=strategy_name,
                operation=operation,
                description=item.get('description', ''),
                prerequisites=prerequisites,
                elaborations=elaborations,
                vocabulary=item.get('deployed_vocabulary', '')
            )

        # Add intra-categorial elaborations based on operation analysis
        self._add_intra_categorial_elaborations()

    def _determine_operation(self, name: str, strategy_id: str) -> str:
        """Determine the arithmetic operation from strategy name/id."""
        name_lower = name.lower()
        id_lower = strategy_id.lower()

        if 'add' in name_lower or 'add' in id_lower or 'counting' in name_lower:
            return 'addition'
        elif 'sub' in name_lower or 'sub' in id_lower or 'decomp' in name_lower:
            return 'subtraction'
        elif 'mult' in name_lower or 'mult' in id_lower:
            return 'multiplication'
        elif 'div' in name_lower or 'div' in id_lower:
            return 'division'
        else:
            return 'general'

    def _add_intra_categorial_elaborations(self):
        """Add algorithmic elaborations within the same operation type."""
        operations = {}
        for strategy in self.strategies.values():
            if strategy.operation not in operations:
                operations[strategy.operation] = []
            operations[strategy.operation].append(strategy)

        # Define elaboration hierarchies for each operation
        elaboration_hierarchies = {
            'addition': [
                ('CO', 'RTMB'),  # Counting On → Rearranging to Make Bases
                ('CO', 'CCOBBTO'),  # Counting On → COBO
                ('RTMB', 'RAA'),  # RTMB → Rounding and Adjusting
                ('RAA', 'C'),  # Rounding → Chunking
            ],
            'subtraction': [
                ('S', 'SC'),  # Sliding → Subtraction Chunking
                ('SC', 'SCCBBBAO'),  # Chunking → CBBO
                ('SC', 'SD'),  # Chunking → Decomposition
            ],
            'multiplication': [
                ('C2C', 'CTBAOCM'),  # Counting 2x2 → CBO
            ],
            'division': [
                ('DBO', 'IDRD'),  # Dealing by Ones → Inverse Distributive
                ('IDRD', 'UCRD'),  # IDR → Using Commutative Reasoning
                ('UCRD', 'CTGOTBCD'),  # UCR → CGOB
            ]
        }

        for operation, hierarchy in elaboration_hierarchies.items():
            for base_id, elab_id in hierarchy:
                if base_id in self.strategies and elab_id in self.strategies:
                    self.elaborations.append(AlgorithmicElaboration(
                        base_strategy=base_id,
                        elaborated_strategy=elab_id,
                        description=f"{operation.capitalize()} elaboration: {self.strategies[base_id].name} → {self.strategies[elab_id].name}",
                        operation_type='intra_categorial'
                    ))

    def generate_developmental_diagram(self, operation_sequence: List[str], output_path: str):
        """Generate a focused diagram showing developmental trajectory."""
        tikz_content = self._generate_tikz_header()

        # Add nodes for strategies in the sequence
        strategy_nodes = []
        for op in operation_sequence:
            if op in self.strategies:
                strategy_nodes.append(self.strategies[op])

        # Position nodes in a diagonal layout
        positions = {}
        for i, strategy in enumerate(strategy_nodes):
            x = i * 4  # Spread horizontally
            y = 8 - i * 1.5  # Slight diagonal downward
            positions[strategy.id] = (x, y)

            # Add node
            tikz_content += self._generate_strategy_node(strategy, positions[strategy.id])

        # Add vocabulary nodes
        vocabularies = set()
        for strategy in strategy_nodes:
            if strategy.vocabulary:
                vocabularies.add(strategy.vocabulary)

        vocab_positions = {}
        for i, vocab in enumerate(vocabularies):
            x = i * 3
            y = -2
            vocab_positions[vocab] = (x, y)
            tikz_content += self._generate_vocabulary_node(vocab, (x, y))

        # Add elaboration arrows and boxes for consecutive strategies
        elaboration_counter = 1
        for i in range(len(strategy_nodes) - 1):
            base_strategy = strategy_nodes[i]
            elab_strategy = strategy_nodes[i + 1]

            base_pos = positions[base_strategy.id]
            elab_pos = positions[elab_strategy.id]

            # Calculate midpoint for AlgEl box
            mid_x = (base_pos[0] + elab_pos[0]) / 2
            mid_y = (base_pos[1] + elab_pos[1]) / 2 + 0.5

            # Create elaboration object
            elab_obj = AlgorithmicElaboration(
                base_strategy=base_strategy.id,
                elaborated_strategy=elab_strategy.id,
                description=f"{base_strategy.operation.capitalize()} elaboration",
                operation_type='intra_categorial'
            )

            # Add Algorithmic Elaboration box
            tikz_content += self._generate_alg_el_box(elab_obj, (mid_x, mid_y), elaboration_counter)

            # Add elaboration arrow
            tikz_content += self._generate_elaboration_arrow(
                base_strategy.id, elab_strategy.id,
                elaboration_counter, 'intra_categorial'
            )
            elaboration_counter += 1

        # Add PV-sufficiency arrows
        pv_counter = 1
        for strategy in strategy_nodes:
            if strategy.vocabulary and strategy.vocabulary in vocab_positions:
                tikz_content += self._generate_pv_arrow(
                    strategy.id, strategy.vocabulary, pv_counter
                )
                pv_counter += 1

        tikz_content += self._generate_tikz_footer()
        tikz_content += self._generate_legend()

        # Write to file
        with open(output_path, 'w') as f:
            f.write(tikz_content)

        print(f"Generated developmental diagram: {output_path}")

    def _generate_tikz_header(self) -> str:
        """Generate TikZ document header."""
        return """\\documentclass[tikz,border=5pt]{standalone}
\\usepackage{tikz}
\\usetikzlibrary{positioning, shapes.geometric, arrows.meta, fit, backgrounds, calc, chains}

\\begin{document}
\\begin{tikzpicture}[
  % Node Styles - Following Brandom's conventions exactly
  vnode/.style={ellipse, draw, fill=gray!20, text=black, minimum height=0.8cm, minimum width=2.0cm, align=center, font=\\scriptsize},
  pnode/.style={rectangle, rounded corners=3pt, draw, fill=gray!60, text=black, minimum height=0.7cm, minimum width=2.0cm, align=center, font=\\scriptsize, inner sep=1pt},
  % Arrow Styles
  solidarrow/.style={-Stealth, thick, black},
  dashedarrow/.style={dashed, -Stealth, thick, gray},
  % Special Elements
  algelelement/.style={rectangle, fill=gray!30, draw, inner sep=2pt, minimum width=1.5cm, minimum height=0.5cm, align=center, font=\\tiny},
  textarrow/.style={align=center, inner sep=1pt, font=\\tiny}
]

"""

    def _generate_strategy_node(self, strategy: StrategyNode, position: tuple) -> str:
        """Generate TikZ code for a strategy node."""
        x, y = position
        clean_name = strategy.name.replace('&', 'and').replace('%', 'percent').replace('(', '').replace(')', '')
        return "\\node[pnode] (" + strategy.id + ") at (" + str(x) + "," + str(y) + ") {P\\textsubscript{" + strategy.id + "}\\\\\\textsubscript{" + clean_name + "}};\n"

    def _generate_vocabulary_node(self, vocab: str, position: tuple) -> str:
        """Generate TikZ code for a vocabulary node."""
        x, y = position
        vocab_clean = vocab.replace(' ', '_')
        return "\\node[vnode] (V_" + vocab_clean + ") at (" + str(x) + "," + str(y) + ") {V\\textsubscript{" + vocab + "}};\n"

    def _generate_alg_el_box(self, elab: AlgorithmicElaboration, position: tuple, counter: int) -> str:
        """Generate TikZ code for Algorithmic Elaboration box."""
        x, y = position
        desc_short = elab.description[:30] + "..." if len(elab.description) > 30 else elab.description
        return "\\node[algelelement] (AlgEl_" + str(counter) + ") at (" + str(x) + "," + str(y) + ") {P\\textsubscript{AlgEl} " + str(counter) + ": PP-suff};\n"

    def _generate_elaboration_arrow(self, base_id: str, elab_id: str, counter: int, elab_type: str) -> str:
        """Generate TikZ code for elaboration arrow."""
        style = "solidarrow" if elab_type == "intra_categorial" else "dashedarrow"
        color = "black" if elab_type == "intra_categorial" else "gray"
        return "\\draw[" + style + "] (" + base_id + ") -- (" + elab_id + ");\n"

    def _generate_pv_arrow(self, strategy_id: str, vocab: str, counter: int) -> str:
        """Generate TikZ code for PV-sufficiency arrow."""
        vocab_id = "V_" + vocab.replace(' ', '_')
        return "\\draw[solidarrow] (" + strategy_id + ") -- node[midway, right, textarrow] {" + str(counter) + ": PV-suff} (" + vocab_id + ");\n"

    def _generate_tikz_footer(self) -> str:
        """Generate TikZ document footer."""
        return "\n"

    def _generate_legend(self) -> str:
        """Generate legend for the diagram."""
        return """% Legend
\\node at (10,10) {\\footnotesize\\bfseries Brandom's Visual Conventions};
\\node[vnode] at (10,9.5) {V};
\\node at (10.8,9.5) {\\tiny Vocabulary (Light Gray Ellipse)};
\\node[pnode] at (10,9) {P};
\\node at (10.8,9) {\\tiny Practice (Dark Gray Rounded Rectangle)};
\\node[algelelement] at (10,8.5) {AlgEl};
\\node at (11,8.5) {\\tiny Algorithmic Elaboration};

% Arrow legend
\\draw[solidarrow] (10,8) -- (11,8);
\\node at (11.5,8) {\\tiny Basic MUR};
\\draw[dashedarrow] (10,7.7) -- (11,7.7);
\\node at (11.5,7.7) {\\tiny Resultant MUR};

\\end{tikzpicture}
\\end{document}
"""

def main():
    """Generate developmental trajectory diagrams."""
    print("🚀 Generating Developmental Trajectory MUDs")
    print("=" * 50)

    # Initialize generator
    metadata_path = "/Users/tio/Documents/GitHub/LK_RB_Synthesis_Project/LK_RB_Synthesis/data/strategy_metadata.json"
    generator = MUDTikZGenerator(metadata_path)

    # Define developmental sequences
    sequences = {
        "counting_to_addition": ["CO", "RTMB", "RAA", "C"],
        "subtraction_development": ["S", "SC", "SCCBBBAO", "SD"],
        "multiplication_progression": ["C2C", "CTBAOCM"],
        "division_trajectory": ["DBO", "IDRD", "UCRD", "CTGOTBCD"],
        "full_arithmetic_trajectory": ["CO", "RTMB", "S", "SC", "C2C", "DBO", "IDRD"]
    }

    # Generate diagrams
    output_dir = "/Users/tio/Documents/GitHub/LK_RB_Synthesis_Project/LK_RB_Synthesis"
    os.makedirs(output_dir, exist_ok=True)

    for seq_name, strategy_ids in sequences.items():
        output_path = os.path.join(output_dir, f"developmental_{seq_name}.tex")
        print(f"\n📊 Generating {seq_name} diagram...")

        try:
            generator.generate_developmental_diagram(strategy_ids, output_path)
            print(f"✅ Generated {seq_name} diagram")
        except Exception as e:
            print(f"❌ Error generating {seq_name}: {e}")

    print("\n" + "=" * 50)
    print("🎉 Developmental trajectory diagrams generated!")
    print("Compile with: pdflatex developmental_*.tex")

if __name__ == "__main__":
    main()