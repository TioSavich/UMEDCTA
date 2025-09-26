import os
import sys
import json
from typing import List

# Add the project root to the Python path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
sys.path.insert(0, project_root)

# We import the classes and the function we just modified
from src.analysis.MUD_Generator import StrategyMetadata, generate_LX_Hierarchy

def load_metadata(metadata_path: str) -> List[StrategyMetadata]:
    """
    Loads a list of StrategyMetadata objects from a JSON file,
    ensuring that strategy_id is unique for graph generation.
    """
    with open(metadata_path, 'r') as f:
        data = json.load(f)

    id_counts = {}
    processed_data = []
    for item in data:
        original_id = item['strategy_id']
        if original_id in id_counts:
            id_counts[original_id] += 1
            item['strategy_id'] = f"{original_id}_{id_counts[original_id]}"
        else:
            id_counts[original_id] = 0
        processed_data.append(item)

    return [StrategyMetadata(**item) for item in processed_data]

def main():
    """
    This script generates a single MUD showing the full dependency graph
    of all arithmetic strategies, focusing on PP-Necessity and LX-relations.
    """
    print("Starting full LX hierarchy analysis...")

    # Define paths relative to the project root
    METADATA_JSON = os.path.join(project_root, "data/strategy_metadata.json")
    OUTPUT_DIR = os.path.join(project_root, "eple/assets/output/visualizations")
    output_path_gv = os.path.join(OUTPUT_DIR, "Full_LX_Hierarchy.gv")
    output_path_pdf = os.path.join(OUTPUT_DIR, "Full_LX_Hierarchy.pdf")


    # Create output directory if it doesn't exist
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    # Load the metadata
    try:
        metadata_list = load_metadata(METADATA_JSON)
        print(f"Successfully loaded {len(metadata_list)} strategies from metadata.")
    except Exception as e:
        print(f"Error loading or parsing metadata from {METADATA_JSON}: {e}")
        return

    # Generate the graph
    dot = generate_LX_Hierarchy(metadata_list)

    # Save the .gv source and render it to a PDF
    try:
        dot.render(output_path_pdf.replace('.pdf', ''), format='pdf', cleanup=True)
        print(f"Successfully rendered dependency graph to: {output_path_pdf}")
        # Also save the .gv file for inspection
        with open(output_path_gv, 'w') as f:
            f.write(dot.source)
        print(f"Graphviz source saved to: {output_path_gv}")

    except Exception as e:
        print(f"Error rendering Graphviz file. Make sure Graphviz is installed and in your system's PATH. Error: {e}")


if __name__ == "__main__":
    main()
