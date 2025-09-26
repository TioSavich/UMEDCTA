#!/usr/bin/env python3
"""
main.py: Unified entry point for the EPLE (Embodied Pragmatic Logic Engine) system.

This script provides a clean interface to all EPLE functionality:
- Automated discovery of algorithmic elaborations
- Meaning-Use Diagram generation
- Report generation in multiple formats
- Strategy analysis and exploration

Usage examples:
    python main.py analyze                    # Run full analysis pipeline
    python main.py report --strategy ADD_COBO # Generate report for specific strategy
    python main.py mud --operation addition   # Generate MUD diagrams
    python main.py explore                    # Interactive exploration mode
"""

import os
import sys
import json
import argparse
from typing import Dict, Any, Optional
from mud_generator import AutomatonAnalyzer, MUDGenerator, ReportGenerator

class EPLE:
    """Main EPLE system interface."""

    def __init__(self):
        self.analysis_results: Optional[Dict[str, Any]] = None
        self.mud_diagrams: Optional[Dict[str, Any]] = None
        self.data_dir = "data"
        self.output_dir = "output"

    def run_full_analysis(self) -> Dict[str, Any]:
        """Run the complete analysis pipeline."""
        print("🚀 Starting EPLE Full Analysis Pipeline")
        print("=" * 60)

        # Step 1: Analyze automata
        print("\n🔬 Phase 1: Analyzing Automata for Computational Patterns")
        # Get the directory of the main.py script
        script_dir = os.path.dirname(os.path.realpath(__file__))
        automata_path = os.path.join(script_dir, "src/automata")
        analyzer = AutomatonAnalyzer(automata_path)
        self.analysis_results = analyzer.analyze_all_automata()

        # Step 2: Generate MUD diagrams
        print("\n🎨 Phase 2: Generating Meaning-Use Diagrams")
        mud_generator = MUDGenerator(self.analysis_results)
        self.mud_diagrams = mud_generator.generate_mud_diagrams()

        # Step 3: Save results
        self._save_results()

        print("\n✅ Analysis Complete!")
        print(f"   📊 Patterns discovered: {len(self.analysis_results.get('patterns', {}))}")
        print(f"   🔗 Elaborations found: {len(self.analysis_results.get('elaborations', []))}")
        print(f"   📈 MUD diagrams created: {len(self.mud_diagrams)}")

        return {
            'analysis_results': self.analysis_results,
            'mud_diagrams': self.mud_diagrams
        }

    def generate_strategy_report(self, strategy_name: str, format_type: str = 'markdown') -> str:
        """Generate a detailed report for a specific strategy."""
        if not self.analysis_results:
            self._load_existing_results()

        if not self.analysis_results:
            print("❌ No analysis results found. Run analysis first.")
            return ""

        report_gen = ReportGenerator(self.analysis_results, self.mud_diagrams)

        if format_type == 'markdown':
            return report_gen.generate_markdown_report(strategy_name)
        elif format_type == 'latex':
            return report_gen.generate_latex_report(strategy_name)
        elif format_type == 'html':
            return report_gen.generate_html_report(strategy_name)
        else:
            print(f"❌ Unsupported format: {format_type}")
            return ""

    def generate_overview_report(self, format_type: str = 'markdown') -> str:
        """Generate an overview report of all findings."""
        if not self.analysis_results:
            self._load_existing_results()

        if not self.analysis_results:
            print("❌ No analysis results found. Run analysis first.")
            return ""

        report_gen = ReportGenerator(self.analysis_results, self.mud_diagrams)

        if format_type == 'markdown':
            return report_gen.generate_markdown_report()
        elif format_type == 'latex':
            return report_gen.generate_latex_report()
        elif format_type == 'html':
            return report_gen.generate_html_report()
        else:
            print(f"❌ Unsupported format: {format_type}")
            return ""

    def list_strategies(self) -> list:
        """List all available strategies."""
        if not self.analysis_results:
            self._load_existing_results()

        if not self.analysis_results:
            return []

        strategies = set()
        for elab in self.analysis_results.get('elaborations', []):
            strategies.add(elab['base_strategy'])
            strategies.add(elab['elaborated_strategy'])

        return sorted(list(strategies))

    def get_strategy_info(self, strategy_name: str) -> Dict[str, Any]:
        """Get detailed information about a specific strategy."""
        if not self.analysis_results:
            self._load_existing_results()

        if not self.analysis_results:
            return {}

        # Find strategy in patterns
        patterns = self.analysis_results.get('strategy_patterns', {}).get(strategy_name, [])

        # Find elaborations involving this strategy
        base_elabs = [e for e in self.analysis_results.get('elaborations', [])
                     if e['base_strategy'] == strategy_name]
        elab_elabs = [e for e in self.analysis_results.get('elaborations', [])
                     if e['elaborated_strategy'] == strategy_name]

        return {
            'name': strategy_name,
            'patterns': patterns,
            'elaborates': base_elabs,
            'elaborated_by': elab_elabs
        }

    def interactive_explore(self):
        """Interactive exploration mode."""
        print("🔍 EPLE Interactive Exploration Mode")
        print("=" * 40)
        print("Commands:")
        print("  list                    - List all strategies")
        print("  info <strategy>         - Get info about a strategy")
        print("  report <strategy>       - Generate detailed report")
        print("  overview                - Show overview report")
        print("  patterns                - Show all computational patterns")
        print("  help                    - Show this help")
        print("  quit                    - Exit")
        print()

        while True:
            try:
                cmd = input("eple> ").strip().split()
                if not cmd:
                    continue

                command = cmd[0].lower()

                if command == 'quit' or command == 'q':
                    break
                elif command == 'help' or command == 'h':
                    self.interactive_explore()  # Show help again
                    break
                elif command == 'list':
                    strategies = self.list_strategies()
                    print(f"\n📋 Available Strategies ({len(strategies)}):")
                    for strategy in strategies:
                        print(f"  • {strategy}")
                elif command == 'info' and len(cmd) > 1:
                    info = self.get_strategy_info(cmd[1])
                    if info:
                        print(f"\n📊 Strategy: {info['name']}")
                        print(f"Patterns: {', '.join(info['patterns'])}")
                        print(f"Elaborates {len(info['elaborates'])} strategies")
                        print(f"Elaborated by {len(info['elaborated_by'])} strategies")
                    else:
                        print(f"❌ Strategy '{cmd[1]}' not found")
                elif command == 'report' and len(cmd) > 1:
                    report = self.generate_strategy_report(cmd[1])
                    print(report)
                elif command == 'overview':
                    report = self.generate_overview_report()
                    print(report)
                elif command == 'patterns':
                    if self.analysis_results:
                        patterns = self.analysis_results.get('patterns', {})
                        print(f"\n🔍 Computational Patterns ({len(patterns)}):")
                        for name, data in patterns.items():
                            print(f"  • {name} ({data.get('type', 'unknown')}): used by {data.get('usage_count', 0)} strategies")
                    else:
                        print("❌ No analysis results loaded")
                else:
                    print("❌ Unknown command. Type 'help' for available commands.")

            except KeyboardInterrupt:
                print("\n👋 Goodbye!")
                break
            except Exception as e:
                print(f"❌ Error: {e}")

    def _save_results(self):
        """Save analysis results to disk."""
        os.makedirs(self.output_dir, exist_ok=True)

        # Save analysis results
        analysis_file = os.path.join(self.output_dir, 'analysis_results.json')
        with open(analysis_file, 'w') as f:
            json.dump(self.analysis_results, f, indent=2)

        # Save MUD diagrams
        mud_file = os.path.join(self.output_dir, 'mud_diagrams.json')
        with open(mud_file, 'w') as f:
            json.dump(self.mud_diagrams, f, indent=2)

        # Save combined results
        combined_file = os.path.join(self.output_dir, 'eple_results.json')
        combined_data = {
            'analysis_results': self.analysis_results,
            'mud_diagrams': self.mud_diagrams
        }
        with open(combined_file, 'w') as f:
            json.dump(combined_data, f, indent=2)

        print(f"💾 Results saved to {self.output_dir}/")

    def _load_existing_results(self):
        """Load existing analysis results if available."""
        results_file = os.path.join(self.output_dir, 'eple_results.json')
        mud_file = os.path.join(self.output_dir, 'mud_diagrams.json')
        
        if os.path.exists(results_file):
            with open(results_file, 'r') as f:
                data = json.load(f)
            self.analysis_results = data.get('analysis_results')
            
            # Load mud_diagrams from the dedicated file if it exists
            if os.path.exists(mud_file):
                with open(mud_file, 'r') as f:
                    self.mud_diagrams = json.load(f)
            else:
                self.mud_diagrams = data.get('mud_diagrams', {})
            
            import sys
            print(f"📂 Loaded existing results from {results_file}", file=sys.stderr)
        else:
            import sys
            print("ℹ️  No existing results found. Run analysis first.", file=sys.stderr)

def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="EPLE (Embodied Pragmatic Logic Engine) - Automated Algorithmic Elaboration Discovery",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python main.py analyze                    # Run full analysis pipeline
  python main.py report --strategy ADD_COBO # Report on specific strategy
  python main.py explore                    # Interactive exploration mode
  python main.py list                       # List all strategies
        """
    )

    parser.add_argument('command', choices=['analyze', 'report', 'explore', 'list'],
                       help='Command to execute')
    parser.add_argument('--strategy', help='Strategy name for report command')
    parser.add_argument('--format', choices=['markdown', 'latex', 'html'],
                       default='markdown', help='Output format for reports')
    parser.add_argument('--output', help='Output file path (default: stdout)')

    args = parser.parse_args()

    eple = EPLE()

    if args.command == 'analyze':
        eple.run_full_analysis()

    elif args.command == 'report':
        if args.strategy:
            report = eple.generate_strategy_report(args.strategy, args.format)
        else:
            report = eple.generate_overview_report(args.format)
        
        if args.output:
            with open(args.output, 'w') as f:
                f.write(report)
            print(f"✅ Report saved to {args.output}")
        else:
            print(report)

    elif args.command == 'explore':
        eple.interactive_explore()

    elif args.command == 'list':
        strategies = eple.list_strategies()
        print(f"📋 Available Strategies ({len(strategies)}):")
        for strategy in strategies:
            print(f"  • {strategy}")

if __name__ == "__main__":
    main()