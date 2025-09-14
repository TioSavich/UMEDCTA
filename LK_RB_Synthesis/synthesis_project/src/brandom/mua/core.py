import networkx as nx

class Vocabulary:
    def __init__(self, name):
        self.name = name
        # Vocabularies have internal inferential structures (e.g., material inference, incompatibility)
        self.inferential_structure = nx.DiGraph()

    def add_expression(self, expression):
        self.inferential_structure.add_node(expression)

    def add_inference(self, source, target, relation_type="material_inference"):
        self.inferential_structure.add_edge(source, target, type=relation_type)

class Practice:
    def __init__(self, name, description=""):
        self.name = name
        self.description = description

class MeaningUseDiagram:
    def __init__(self, name):
        self.name = name
        self.graph = nx.DiGraph()

    def add_component(self, component):
        component_type = type(component).__name__
        self.graph.add_node(component.name, type=component_type, data=component)

    def add_relation(self, source_name, target_name, relation_type):
        # Relation types: PP-Sufficiency, PV-Sufficiency, VP-Necessity, VV-Sufficiency, etc.
        self.graph.add_edge(source_name, target_name, type=relation_type)
