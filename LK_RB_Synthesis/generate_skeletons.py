import json
import os
import re

def to_snake_case(name):
    """Converts a string to snake_case."""
    name = re.sub(r'[^a-zA-Z0-9\s]', '', name)
    return '_'.join(name.lower().split())

def generate_skeletons(json_path, output_dir):
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    with open(json_path, 'r') as f:
        strategies = json.load(f)

    for strategy in strategies:
        name = strategy['Name']
        description = strategy['Description']
        cognitive_steps = strategy['Cognitive_Steps']

        # Generate a filename from the name
        filename = f"{to_snake_case(name)}.py"
        filepath = os.path.join(output_dir, filename)

        # Generate the class name
        class_name = ''.join(word.title() for word in name.replace('(', '').replace(')', '').split())
        class_name = re.sub(r'[^a-zA-Z0-9]', '', class_name)
        if not class_name.endswith("Automaton"):
            class_name += "Automaton"

        # Generate the python code for the skeleton
        code = f"""
from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata

class {class_name}(BaseAutomaton):
    _metadata = {{
        "Name": "{name}",
        "Description": "{description}",
        "Cognitive Steps": {cognitive_steps},
        "Examples": []
    }}

    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(**self._metadata)

    def execute_q_start(self):
        # Initial state logic
        pass
"""

        for step in cognitive_steps:
            # Skip q_start as it's already defined
            if step == 'q_start':
                continue
            # Clean the step name to be a valid method name
            method_name = step.replace('q_', '')
            code += f"""
    def execute_{step}(self):
        # Logic for {method_name}
        pass
"""

        with open(filepath, 'w') as f:
            f.write(code)
        print(f"Generated skeleton for {name} at {filepath}")

if __name__ == "__main__":
    generate_skeletons('strategies.json', 'strategies')
