"""
run_full_analysis.py: Master script to run the complete analytic pipeline for the EPLE system.
"""

import os
import sys
import subprocess

def run_command(command, description):
    """Run a command and print status."""
    print(f"\n--- {description} ---")
    try:
        result = subprocess.run(command, shell=True, check=True, capture_output=True, text=True)
        print(f"✅ {description} completed successfully")
        if result.stdout:
            print(result.stdout.strip())
        return True
    except subprocess.CalledProcessError as e:
        print(f"❌ {description} failed:")
        print(e.stderr.strip())
        return False

def main():
    """Run the complete analytic pipeline."""
    print("🚀 Starting EPLE Full Analytic Pipeline")
    print("=" * 50)

    # Get the project root
    script_dir = os.path.dirname(os.path.abspath(__file__))
    project_root = os.path.dirname(script_dir)

    # Change to project root
    os.chdir(project_root)

    success = True

    # Step 1: Parse conceptual metaphors from knowledge base
    if success:
        success = run_command(
            "python3 scripts/parse_metaphor_kb.py",
            "Parsing conceptual metaphors from Metaphor_Knowledge_Base.md"
        )

    # Step 2: Parse strategy metadata from Brandomian analysis
    if success:
        success = run_command(
            "python3 scripts/parse_jules_analysis.py",
            "Parsing strategy metadata from brandomian_analysis.md"
        )

    # Step 3: Parse strategies from HTML files
    if success:
        success = run_command(
            "python3 -c \"from eple.domains.arithmetic.parsing import parse_strategies; parse_strategies('eple/assets/data/raw_html', 'eple/assets/data/processed/strategies.csv')\"",
            "Parsing strategies from HTML files"
        )

    # Step 4: Generate LX hierarchy analysis
    if success:
        success = run_command(
            "python3 scripts/run_lx_analysis.py",
            "Generating LX hierarchy dependency graphs"
        )

    # Step 5: Generate Meaning-Use Diagrams
    if success:
        success = run_command(
            "python3 scripts/run_visualization.py",
            "Generating Meaning-Use Diagrams for all strategies"
        )

    # Step 6: Run MUA analysis tests
    if success:
        success = run_command(
            "python3 eple/domains/arithmetic/elaboration_analysis.py",
            "Running MUA elaboration analysis"
        )

    if success:
        success = run_command(
            "python3 eple/domains/arithmetic/metaphor_test.py",
            "Running conceptual metaphor validation"
        )

    print("\n" + "=" * 50)
    if success:
        print("🎉 EPLE Full Analytic Pipeline completed successfully!")
        print("\nGenerated outputs:")
        print("- eple/assets/data/processed/cmt_data.json (conceptual metaphors)")
        print("- data/strategy_metadata.json (strategy analysis)")
        print("- eple/assets/data/processed/strategies.csv (parsed strategies)")
        print("- eple/assets/output/visualizations/Full_LX_Hierarchy.* (LX hierarchy)")
        print("- eple/assets/output/visualizations/MUD_*.gv (Meaning-Use Diagrams)")
    else:
        print("💥 EPLE Full Analytic Pipeline failed. Check errors above.")
        sys.exit(1)

if __name__ == '__main__':
    main()