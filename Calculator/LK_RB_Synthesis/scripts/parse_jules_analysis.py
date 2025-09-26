"""
Parses the analysis from brandomian_analysis.md and populates the metadata for each strategy.
"""

import json
import os
import re
from typing import List, Dict, Optional

# Assuming the metadata structures are in a place that can be imported
# from src.analysis.MUA_Metadata import StrategyMetadata, Practice, LXRelation, MaterialInference

def get_strategy_id(name: str) -> str:
    """Generates a strategy ID from its name."""
    name = name.lower()
    name = name.replace(" (addition)", "")
    name = name.replace(" (division - sharing)", "")
    name = name.replace(" (missing addend)", "")
    name = name.replace(" (counting back)", "")
    name = name.replace(" (borrowing)", "")
    name = name.replace(" (multiplication)", "")
    name = name.replace(" (backwards by part)", "")
    name = name.replace(" (forwards from part)", "")
    name = name.replace(" (backwards to part)", "")
    
    parts = re.findall(r'\b\w', name)
    return "".join(parts).upper()

def parse_practices(text: str) -> List[Dict]:
    """Parses a list of practices (PP-Necessities/Sufficiencies)."""
    practices = []
    # Regex to capture the ID (P1), the name in parens, and the description
    pattern = r'\*\s+\*\*(P\d+)\s*(?:\((.*?)\))?:\*\*\s*(.*)'
    matches = re.findall(pattern, text)
    for match in matches:
        practice_id, name, description = match
        # Create a more descriptive ID if a name is present
        full_id = f"P_{name.replace(' ', '')}" if name else practice_id
        practices.append({
            "id": full_id,
            "description": description.strip()
        })
    return practices

def parse_jules_analysis(file_path: str) -> List[Dict]:
    """
    Parses the markdown file and extracts strategy analysis.
    """
    print(f"Parsing analysis from: {file_path}")
    
    with open(file_path, 'r') as f:
        content = f.read()

    strategy_metadata_list = []
    
    # Split the document into sections for each strategy
    strategy_sections = re.split(r'### Part A: Meaning-Use Analysis of "(.*?)"', content)
    
    if len(strategy_sections) < 2:
        return []

    # The first element is anything before the first strategy, so we skip it.
    # The sections come in pairs: (strategy_name, strategy_content)
    it = iter(strategy_sections[1:])
    for name, section in zip(it, it):
        strategy_name = name.strip()
        strategy_id = get_strategy_id(strategy_name)
        
        # Extract Deployed Vocabulary from Material Inferences
        deployed_vocabulary = ""
        inference_match = re.search(r'#### 1\. Central Material Inferences\s*\*\s*(.*?)\s*\*', section, re.DOTALL)
        if inference_match:
            # A simple heuristic: take the first sentence's core concept.
            first_sentence = inference_match.group(1).split('.')[0]
            # This is a rough heuristic and could be improved
            if "principle of" in first_sentence:
                deployed_vocabulary = first_sentence.split("principle of")[1].strip()
            elif "embodies the meaning of" in first_sentence:
                 deployed_vocabulary = first_sentence.split("embodies the meaning of")[1].strip()
            elif "enactment of the" in first_sentence:
                deployed_vocabulary = first_sentence.split("enactment of the")[1].strip()

        # Extract PP-Necessities
        pp_necessities = []
        necessities_match = re.search(r'\*\*PP-Necessities \(Prerequisite Practices\):\*\*(.*?)\*\*PP-Sufficiencies', section, re.DOTALL)
        if necessities_match:
            pp_necessities = parse_practices(necessities_match.group(1))

        # Extract PP-Sufficiencies
        pp_sufficiencies = []
        sufficiencies_match = re.search(r'\*\*PP-Sufficiencies \(Practices Sufficient to Deploy\):\*\*(.*?)(?:#### 3|```mermaid)', section, re.DOTALL)
        if sufficiencies_match:
            pp_sufficiencies = parse_practices(sufficiencies_match.group(1))

        # For now, LX relations are manually defined as they require interpretation
        lx_relations = []
        if "Rearranging to Make Bases" in strategy_name:
             lx_relations.append({
                    "elaborates_strategy_id": "CO", # Counting On
                    "implicit_practice": "Implicitly bridging 10 when counting.",
                    "explicit_principle": "Associativity of addition can be used for strategic advantage.",
                    "explanation": "RMB makes the associative property explicit to form convenient groups, while Counting On just implicitly relies on number system structure."
                })

        strategy_metadata = {
            "strategy_id": strategy_id,
            "strategy_name": strategy_name,
            "deployed_vocabulary": deployed_vocabulary,
            "pp_necessities": pp_necessities,
            "pp_sufficiencies_alg_elaboration": pp_sufficiencies,
            "lx_relations": lx_relations,
            "inferences": [], # Placeholder
            "metaphors": [], # Placeholder
            "description": "", # Placeholder
            "visualization_hints": [] # Placeholder
        }
        strategy_metadata_list.append(strategy_metadata)

    return strategy_metadata_list

def main():
    """
    Main function to run the parsing and output the JSON data.
    """
    # Get the directory where the script is located
    script_dir = os.path.dirname(os.path.abspath(__file__))
    # Go up one level to the project root
    project_root = os.path.dirname(script_dir)

    analysis_file = os.path.join(project_root, 'brandomian_analysis.md')
    output_dir = os.path.join(project_root, 'data')
    output_file = os.path.join(output_dir, 'strategy_metadata.json')
    
    if not os.path.exists(analysis_file):
        print(f"Error: Analysis file not found at '{analysis_file}'.")
        return

    # Create data directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    # Parse the analysis
    metadata = parse_jules_analysis(analysis_file)
    
    # Write the output to a JSON file
    with open(output_file, 'w') as f:
        json.dump(metadata, f, indent=4)
        
    print(f"Successfully generated strategy metadata at: {output_file}")

if __name__ == '__main__':
    main()
