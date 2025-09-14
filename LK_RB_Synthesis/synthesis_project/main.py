from src.synthesis.synthesizer import Synthesizer
from src.synthesis.visualization import visualize_mud
from src.synthesis.strategy_parser import parse_strategies
import os

def main():
    # Get the absolute path of the directory containing main.py
    script_dir = os.path.dirname(os.path.abspath(__file__))

    RAW_HTML_DIR = os.path.join(script_dir, "data/raw_html")
    PROCESSED_DATA_DIR = os.path.join(script_dir, "data/processed")
    CMT_DATA = os.path.join(PROCESSED_DATA_DIR, "cmt_data.json")
    STRATEGIES_CSV = os.path.join(PROCESSED_DATA_DIR, "strategies.csv")
    OUTPUT_DIR = os.path.join(script_dir, "output/visualizations")

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
            # Ensure a unique output path for each MUD
            output_path = os.path.join(OUTPUT_DIR, f"MUD_{row.get('id', index)}.gv")
            visualize_mud(mud, output_path)
            count += 1
    
    print(f"Successfully generated {count} Meaning-Use Diagrams in {OUTPUT_DIR}.")

if __name__ == "__main__":
    main()
