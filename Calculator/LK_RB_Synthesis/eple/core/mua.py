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

def visualize_mud(mud, output_path):
    """
    Generates a Graphviz .gv file from a MeaningUseDiagram object,
    following the specific Brandomian MUD visual conventions.
    """
    dot = Digraph(comment=mud.name)
    dot.attr('graph', rankdir='TB', splines='ortho')
    dot.attr('node', fontname='Serif', fontsize='12')
    dot.attr('edge', fontname='Serif', fontsize='10', penwidth='2.0', arrowhead='stealth')

    node_types = nx.get_node_attributes(mud.graph, 'type')
    
    for node_name in mud.graph.nodes():
        ntype = node_types.get(node_name, "Unknown")

        # Sanitize and format the label text
        clean_label = node_name.replace('P_Strategy_', '').replace('P_Embodied_', '').replace('V_', '').replace('_', ' ')
        wrapped_label = _wrap_label(clean_label)

        if ntype == "Practice":
            # Darker gray, filled rectangles with rounded corners
            dot.node(node_name, label=f"P_{{{wrapped_label}}}", shape='box', style='filled,rounded',
                     fillcolor='gray70', fontcolor='white')
        elif ntype == "Vocabulary":
            # Light gray, filled ellipses
            dot.node(node_name, label=f"V_{{{wrapped_label}}}", shape='ellipse', style='filled',
                     fillcolor='gray90')
        elif ntype == "Metaphor":
            # This is not in the spec, but we'll make it distinct
            # Using a dashed box for the metaphor concept itself
            dot.node(node_name, label=wrapped_label, shape='box', style='dashed')
        else:
            # Default for any other node type
            dot.node(node_name, label=wrapped_label)

    edge_labels = nx.get_edge_attributes(mud.graph, 'type')
    edge_counter = 1
    palgel_counter = 1

    for source, target in mud.graph.edges():
        relation_type = edge_labels.get((source, target), "")
        label = f"{edge_counter}: {relation_type}"

        # Handle Algorithmic Elaboration as a special case
        if "PP-Sufficiency (Elaboration)" in relation_type:
            # 1. Create the PAlgEl node
            palgel_name = f"PAlgEl_{palgel_counter}"
            palgel_label = _wrap_label(f"PAlgEl {edge_counter}: PP-suff")
            dot.node(palgel_name, label=palgel_label, shape='box', style='filled',
                     fillcolor='lightgray', fontcolor='black')
            palgel_counter += 1

            # 2. Draw edges to and from the PAlgEl node to simulate it sitting on the arrow
            dot.edge(source, palgel_name, arrowhead='none', style='solid', color='black')
            dot.edge(palgel_name, target, style='solid', color='black')
            edge_counter += 1
            continue

        # Handle other relation types
        is_resultant = "Mediated by" in relation_type or "Structures" in relation_type

        style = 'dashed' if is_resultant else 'solid'
        color = 'gray' if is_resultant else 'black'

        dot.edge(source, target, label=label, style=style, color=color)
        edge_counter += 1

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


def find_pragmatic_metavocabulary(V_base, P_base, all_practices, all_murs):
    """
    Finds all vocabularies that serve as pragmatic metavocabularies for V_base
    relative to P_base. A pragmatic metavocabulary allows specification of
    practices that elaborate P_base.
    
    Returns a set of Vocabulary objects that qualify as metavocabularies.
    """
    metavocabularies = set()
    
    for V_candidate in [p.vocabulary for p in all_practices if p.vocabulary]:
        if V_candidate == V_base:
            continue
            
        # Check if V_candidate can specify elaborations of P_base
        can_specify_elaborations = False
        for mur in all_murs:
            if isinstance(mur, PP_Sufficiency) and mur.P_base == P_base:
                # Check if V_candidate contains predicates that can describe
                # the elaboration mur.P_elaborated
                if _vocabulary_can_describe_practice(V_candidate, mur.P_elaborated):
                    can_specify_elaborations = True
                    break
        
        if can_specify_elaborations:
            metavocabularies.add(V_candidate)
    
    return metavocabularies


def is_LX(p_base, p_elaborated, v_base, v_elaborated, all_practices, all_murs):
    """
    Checks if (P_elaborated, V_elaborated) stands in an LX relation to (P_base, V_base).
    This means:
    1. P_elaborated is an elaboration of P_base (via PP_Sufficiency)
    2. V_elaborated is a pragmatic metavocabulary for V_base relative to P_base
    3. V_elaborated explicates what P_elaborated makes explicit from P_base
    
    Returns True if the LX relation holds.
    """
    # Check 1: Is there a PP_Sufficiency relation from P_base to P_elaborated?
    has_pp_sufficiency = any(
        isinstance(mur, PP_Sufficiency) and mur.P_base == p_base and mur.P_elaborated == p_elaborated
        for mur in all_murs
    )
    
    if not has_pp_sufficiency:
        return False
    
    # Check 2: Is V_elaborated a pragmatic metavocabulary for V_base?
    pv_meta = find_pragmatic_metavocabulary(v_base, p_base, all_practices, all_murs)
    if v_elaborated not in pv_meta:
        return False
    
    # Check 3: Does V_elaborated explicate the structure that P_elaborated makes explicit?
    # This is a more complex check - for now, we'll assume that if the above conditions
    # hold and V_elaborated is different from V_base (has different predicates), it's an explication
    if v_elaborated.predicates == v_base.predicates:
        return False
    
    return True


def _vocabulary_can_describe_practice(vocabulary, practice):
    """
    Helper function to check if a vocabulary contains predicates that can
    describe the structure of a practice.
    
    This is a simplified implementation - in practice, this would involve
    checking if the vocabulary's predicates can express the practice's
    inferential structure.
    """
    # For now, assume that vocabularies with more predicates can describe
    # more complex practices
    return len(vocabulary.predicates) > 0
