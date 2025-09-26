# src/analysis/MUD_Generator.py
import graphviz
# from src.analysis.MUA_Metadata import StrategyMetadata
from typing import List

# Dummy classes to allow the code to run standalone for now
# In the final implementation, these would be imported from MUA_Metadata.py
class Practice:
    def __init__(self, id, description):
        self.id = id
        self.description = description

class StrategyMetadata:
    def __init__(self, strategy_id, strategy_name, deployed_vocabulary, pp_necessities, pp_sufficiencies_alg_elaboration, lx_relations, **kwargs):
        self.strategy_id = strategy_id
        self.strategy_name = strategy_name
        self.deployed_vocabulary = deployed_vocabulary
        self.pp_necessities = [Practice(**p) for p in pp_necessities]
        self.pp_sufficiencies_alg_elaboration = [Practice(**p) for p in pp_sufficiencies_alg_elaboration]
        self.lx_relations = lx_relations


def generate_structural_MUD(meta: StrategyMetadata):
    """Generates a structural Brandomian MUD."""
    dot = graphviz.Digraph(comment=f'MUD for {meta.strategy_name}')
    # Define Styles (Similar to TikZ example)
    V_STYLE = {'shape': 'ellipse', 'style': 'filled', 'fillcolor': '#E0E0E0'}
    P_STYLE = {'shape': 'rectangle', 'style': 'rounded,filled', 'fillcolor': '#A0A0A0'}

    # --- V-Space ---
    V_Core = f"V_{meta.strategy_id}"
    dot.node(V_Core, label=f"V: {meta.deployed_vocabulary}", **V_STYLE)

    # --- P-Space: Core Practice ---
    P_Core = f"P_{meta.strategy_id}"
    dot.node(P_Core, label=f"P: {meta.strategy_name}", **P_STYLE)

    # Relation: PV-Sufficiency (Practice is sufficient to deploy the Vocabulary)
    dot.edge(P_Core, V_Core, label="PV-Suff", color='black')

    # --- P-Space: Algorithmic Elaboration (PP-Sufficiencies) ---
    # Create a cluster to visualize the composition of the elaboration
    with dot.subgraph(name=f'cluster_AlgEl_{meta.strategy_id}') as c:
        c.attr(label='Algorithmic Elaboration (P-AlgEl)')
        c.attr(style='dashed')
        alg_nodes = []
        for p in meta.pp_sufficiencies_alg_elaboration:
            node_id = f"P_AlgEl_{meta.strategy_id}_{p.id}"
            c.node(node_id, label=f"{p.id}: {p.description}", shape='box')
            alg_nodes.append(node_id)

    # Relation: PP-Sufficiency (Elaboration is sufficient for the Core Practice)
    if alg_nodes:
        # Use ltail/lhead to connect edges to the cluster boundary
        # We connect from the first node inside the cluster for layout purposes, but use ltail to point from the cluster itself.
        dot.edge(alg_nodes[0], P_Core, label="PP-Suff (Composition)", color='darkgreen', ltail=f'cluster_AlgEl_{meta.strategy_id}')

    # --- P-Space: Prerequisites (PP-Necessities) ---
    P_Prereq = f"P_Prereq_{meta.strategy_id}"
    prereq_label = "Prerequisites (P-Base):\n" + "\n".join([f"- {p.description}" for p in meta.pp_necessities])
    dot.node(P_Prereq, label=prereq_label, **P_STYLE)

    # Relation: PP-Necessity (Prerequisites are necessary for the Elaboration)
    if alg_nodes:
         dot.edge(P_Prereq, alg_nodes[0], label="PP-Nec", color='gray', style='dashed', lhead=f'cluster_AlgEl_{meta.strategy_id}')

    return dot

def _wrap_label(label, max_width=20):
    """Wraps a label into multiple lines if it's too long."""
    words = label.split(' ')
    lines = []
    current_line = ""
    for word in words:
        if len(current_line) + len(word) + 1 > max_width:
            lines.append(current_line)
            current_line = word
        else:
            if current_line:
                current_line += " "
            current_line += word
    if current_line:
        lines.append(current_line)
    return '\\n'.join(lines)

def generate_LX_Hierarchy(metadata_list: List[StrategyMetadata]):
    """
    Visualizes the full dependency graph of strategies, highlighting LX relationships.
    This version adheres to the specified Brandomian MUD visual conventions.
    """
    dot = graphviz.Digraph(comment='Full Strategy Dependency Hierarchy')
    dot.attr('graph', rankdir='TB', splines='ortho', label='Full Strategy Dependency Hierarchy', labelloc='t', fontsize='16')
    dot.attr('node', fontname='Serif', fontsize='12')
    dot.attr('edge', fontname='Serif', fontsize='10', penwidth='2.0', arrowhead='stealth')

    id_map = {meta.strategy_id: meta for meta in metadata_list}

    # Add all practices (strategies) as nodes first
    for meta in metadata_list:
        wrapped_label = _wrap_label(meta.strategy_name)
        dot.node(meta.strategy_id, label=f"P_{{{wrapped_label}}}", shape='box', style='filled,rounded',
                 fillcolor='gray70', fontcolor='white')

    edge_counter = 1
    # Add edges for both PP-Necessity and LX relations
    for meta in metadata_list:
        # 1. Add PP-Necessity edges (basic relations)
        for prereq in meta.pp_necessities:
            if prereq.id in id_map:
                # Edge goes from the prerequisite to the strategy that requires it
                dot.edge(prereq.id, meta.strategy_id,
                         label=f"{edge_counter}: PP-nec",
                         style='solid', color='black')
                edge_counter += 1

        # 2. Add LX-relation edges (resultant relations)
        # These are emergent from the PP-Nec/PP-Suff/PV-Suff structure
        for lx_rel in meta.lx_relations:
            target_id = lx_rel['elaborates_strategy_id']
            if target_id in id_map:
                # The resultant arrow shows that meta.strategy_id makes explicit what is in target_id
                # This is a VV-Resultant relation, shown as a dashed gray arrow
                dot.edge(target_id, meta.strategy_id,
                         label=f"Res_{edge_counter}: LX for",
                         style='dashed', color='gray',
                         tooltip=_wrap_label(f"Makes explicit the principle: {lx_rel['explicit_principle']}", 40))
                edge_counter += 1

    return dot
