import pandas as pd
import json
import networkx as nx
from graphviz import Digraph

class MUAComponent:
    """Base class for components in a Meaning-Use Diagram."""
    def __init__(self, name, description=""):
        self.name = name
        self.description = description

class Practice(MUAComponent):
    """Represents a Practice (a 'Doing')."""
    def __init__(self, name, description="", vocabulary=None, inferences=None, incompatibilities=None):
        super().__init__(name, description)
        self.vocabulary = vocabulary
        self.inferences = inferences if inferences is not None else set()
        self.incompatibilities = incompatibilities if incompatibilities is not None else set()

class Vocabulary(MUAComponent):
    """Represents a Vocabulary (a 'Saying')."""
    def __init__(self, name, description="", predicates=None):
        super().__init__(name, description)
        self.predicates = predicates if predicates is not None else set()
        self.expressions = []
        self.inferences = []

    def add_expression(self, expression):
        self.expressions.append(expression)

    def add_inference(self, premise, conclusion, relation_type):
        self.inferences.append((premise, conclusion, relation_type))


class MeaningUseDiagram:
    """Represents a Meaning-Use Diagram (MUD)."""
    def __init__(self, name):
        self.name = name
        self.graph = nx.DiGraph()

    def add_component(self, component):
        node_type = component.__class__.__name__
        self.graph.add_node(component.name, type=node_type, description=component.description)

    def add_relation(self, source, target, relation_type):
        self.graph.add_edge(source, target, type=relation_type)


class Synthesizer:
    def __init__(self, cmt_data_path):
        with open(cmt_data_path, 'r') as f:
            self.cmt_data = json.load(f)
        self._prepare_cmt_maps()

    def _prepare_cmt_maps(self):
        self.image_schemas = {s['name']: s for s in self.cmt_data.get('image_schemas', [])}
        self.conceptual_metaphors = {m['name']: m for m in self.cmt_data.get('conceptual_metaphors', [])}

    def load_strategies(self, strategies_path):
        self.strategies_df = pd.read_csv(strategies_path)

    def _populate_vocabulary_inferences(self, vocabulary, metaphor_data):
        """
        Populates the vocabulary's inferential structure based on the metaphor.
        This is a simplified placeholder for the inferential analysis.
        """
        if metaphor_data['name'] == "Arithmetic is Object Collection":
            vocabulary.add_expression("Numbers")
            vocabulary.add_expression("Addition")
            vocabulary.add_expression("Result")
            vocabulary.add_inference("Addition", "Result", "yields")
            vocabulary.add_expression("Greater")
            vocabulary.add_inference("Result", "Greater", "is")

    def _sanitize_name(self, name):
        return name.replace(',', '').replace('(', '').replace(')', '').replace(' - ', '_').replace(' ', '_')

    def generate_mud(self, strategy_row):
        strategy_name = self._sanitize_name(strategy_row['name'])
        annotation = strategy_row['metaphor_annotation']

        if pd.isna(annotation) or annotation not in self.conceptual_metaphors:
            return None

        metaphor_data = self.conceptual_metaphors[annotation]
        mud = MeaningUseDiagram(f"MUD for {strategy_name}")

        if metaphor_data.get('image_schema') in self.image_schemas:
            schema_name = metaphor_data['image_schema']
            basic_practice = Practice(f"P_Embodied_{schema_name}", self.image_schemas[schema_name]['description'])
            mud.add_component(basic_practice)
        else:
            basic_practice = Practice(f"P_{metaphor_data['source_domain']}", f"Practice of {metaphor_data['source_domain']}")
            mud.add_component(basic_practice)

        complex_practice = Practice(f"P_Strategy_{strategy_name}", strategy_row['description'])
        mud.add_component(complex_practice)
        
        math_vocabulary = Vocabulary(f"V_{metaphor_data['target_domain']}")
        self._populate_vocabulary_inferences(math_vocabulary, metaphor_data)
        mud.add_component(math_vocabulary)

        mud.add_relation(basic_practice.name, complex_practice.name, "PP-Sufficiency (Elaboration)")
        mud.add_relation(complex_practice.name, math_vocabulary.name, "PV-Sufficiency (Deployment)")

        mud.graph.add_node(annotation, type="Metaphor")
        mud.add_relation(basic_practice.name, annotation, "Mediated by")
        mud.add_relation(annotation, math_vocabulary.name, "Structures")

        return mud

def visualize_mud(mud, output_path):
    dot = Digraph(comment=mud.name)
    dot.attr('node', fontname='Helvetica', fontsize='10')
    dot.attr('edge', fontname='Helvetica', fontsize='8')

    node_types = nx.get_node_attributes(mud.graph, 'type')
    
    for node in mud.graph.nodes():
        ntype = node_types.get(node, "Unknown")
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

    # Save the .gv source file without rendering the PDF
    with open(output_path, 'w') as f:
        f.write(dot.source)

class MeaningUseRelation:
    """Base class for all Meaning-Use Relations (MURs)."""
    def __init__(self, P_base, P_elaborated):
        self.P_base = P_base
        self.P_elaborated = P_elaborated

class PragmaticProjection(MeaningUseRelation):
    """
    Represents a metaphorical projection from a source practice to a target practice.
    This is a specialized form of PP-Sufficiency.
    """
    def __init__(self, P_base, P_elaborated, mappings, name=""):
        super().__init__(P_base, P_elaborated)
        self.mappings = mappings
        self.name = name

class PP_Sufficiency(MeaningUseRelation):
    """A practice-practice sufficiency relation, where one practice elaborates another."""
    pass

class PV_Sufficiency(MeaningUseRelation):
    """A practice-vocabulary sufficiency relation, where a practice deploys a vocabulary."""
    pass

class AlgorithmicElaboration(PP_Sufficiency):
    """A specific type of PP-Sufficiency that represents an algorithmic elaboration."""
    pass
