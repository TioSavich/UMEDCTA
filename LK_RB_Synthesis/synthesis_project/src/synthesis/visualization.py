from graphviz import Digraph
import networkx as nx

def visualize_mud(mud, output_path):
    dot = Digraph(comment=mud.name)
    dot.attr('node', fontname='Helvetica', fontsize='10')
    dot.attr('edge', fontname='Helvetica', fontsize='8')

    node_types = nx.get_node_attributes(mud.graph, 'type')
    
    for node in mud.graph.nodes():
        ntype = node_types.get(node, "Unknown")
        # Clean up node names for display
        label = node.replace('P_Strategy_', '').replace('P_Embodied_', '').replace('V_', '').replace('_', ' ')

        if ntype == "Practice":
            dot.node(node, label=f"P: {label}", shape='ellipse')
        elif ntype == "Vocabulary":
            dot.node(node, label=f"V: {label}", shape='box', style='rounded')
        elif ntype == "Metaphor":
            dot.node(node, label=f"M: {label}", shape='box', style='rounded,dashed')
        else:
            dot.node(node, label=label)

    edge_labels = nx.get_edge_attributes(mud.graph, 'type')
    for source, target in mud.graph.edges():
        label = edge_labels.get((source, target), "")
        style = 'dashed' if label in ["Mediated by", "Structures"] else 'solid'
        dot.edge(source, target, label=label, style=style)

    dot.render(output_path, view=False)
