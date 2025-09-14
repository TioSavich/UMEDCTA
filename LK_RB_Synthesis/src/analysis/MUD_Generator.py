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
    def __init__(self, strategy_id, strategy_name, deployed_vocabulary, pp_necessities, pp_sufficiencies_alg_elaboration, lx_relations):
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

def generate_LX_Hierarchy(metadata_list: List[StrategyMetadata]):
    """Visualizes the LX relationships (VV-Resultance) between strategies."""
    dot = graphviz.Digraph(comment='LX Hierarchy')
    dot.attr(rankdir='BT') # Bottom to Top layout

    # Add nodes
    id_map = {meta.strategy_id: meta for meta in metadata_list}
    for meta in metadata_list:
        dot.node(meta.strategy_id, label=meta.strategy_name, shape='box')

    # Add edges
    for meta in metadata_list:
        for lx_rel in meta.lx_relations:
            target_id = lx_rel['elaborates_strategy_id'] # Adjusted for dict access
            if target_id in id_map:
                # Edge goes from the elaborated (target) to the elaborating (meta) strategy
                # Use the explanation as a tooltip for detailed visualization
                dot.edge(target_id, meta.strategy_id, label="LX (VV-Res)", color='purple', style='bold', tooltip=lx_rel['explicit_principle'])

    return dot
