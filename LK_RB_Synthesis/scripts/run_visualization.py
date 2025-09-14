import os
import sys

# Add the project root to the Python path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
sys.path.insert(0, project_root)

from eple.core.mua import Synthesizer, visualize_mud
from eple.domains.arithmetic.parsing import parse_strategies

def main():
    # Define paths relative to the project root
    ASSETS_DIR = os.path.join(project_root, "eple/assets")
    RAW_HTML_DIR = os.path.join(ASSETS_DIR, "data/raw_html")
    PROCESSED_DATA_DIR = os.path.join(ASSETS_DIR, "data/processed")
    CMT_DATA = os.path.join(PROCESSED_DATA_DIR, "cmt_data.json")
    STRATEGIES_CSV = os.path.join(PROCESSED_DATA_DIR, "strategies.csv")
    OUTPUT_DIR = os.path.join(ASSETS_DIR, "output/visualizations")

    # Create directories if they don't exist
    os.makedirs(PROCESSED_DATA_DIR, exist_ok=True)
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    # Parse strategies from HTML
    parse_strategies(RAW_HTML_DIR, STRATEGIES_CSV)

    synthesizer = Synthesizer(CMT_DATA)
    synthesizer.load_strategies(STRATEGIES_CSV)

    count = 0
    for index, row in synthesizer.strategies_df.iterrows():
        mud = synthesizer.generate_mud(row)
        if mud:
            output_path = os.path.join(OUTPUT_DIR, f"MUD_{row.get('id', index)}.gv")
            visualize_mud(mud, output_path)
            count += 1
    
    print(f"Successfully generated {count} Meaning-Use Diagrams in {OUTPUT_DIR}.")

if __name__ == "__main__":
    main()
