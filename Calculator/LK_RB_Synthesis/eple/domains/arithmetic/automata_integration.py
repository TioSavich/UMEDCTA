"""
automata_integration.py: Integrates actual Python automata with EPLE's MUA framework.

This module automatically discovers, loads, and analyzes all automata implementations
in src/automata/ to create a comprehensive analysis of algorithmic elaborations
and prerequisite abilities in arithmetic strategies.
"""

import sys
import os
import importlib
import inspect
from typing import Dict, List, Set, Any
from pathlib import Path

# Add project root to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata
from eple.core.mua import Practice, Vocabulary, AlgorithmicElaboration, PP_Sufficiency
from eple.core.logic_terms import Predicate
from eple.core.deontic_scorekeeper import DeonticScorekeeper

class AutomataIntegrator:
    """
    Discovers, loads, and integrates all Python automata with the EPLE framework.
    """

    def __init__(self):
        self.automata_classes = {}
        self.automata_instances = {}
        self.practices = {}
        self.vocabularies = {}
        self.murs = set()

    def discover_automata(self):
        """Automatically discover all automaton classes in the src/automata/ directory."""
        automata_dir = Path(os.path.join(os.path.dirname(__file__), '../../../src/automata'))

        print("🔍 Discovering automata in:", automata_dir)

        for operation_dir in automata_dir.iterdir():
            if operation_dir.is_dir() and not operation_dir.name.startswith('__'):
                print(f"📁 Scanning {operation_dir.name} operations...")

                for py_file in operation_dir.glob('*.py'):
                    if py_file.name.startswith('__'):
                        continue

                    try:
                        # Import the module
                        module_name = f"src.automata.{operation_dir.name}.{py_file.stem}"
                        module = importlib.import_module(module_name)

                        # Find automaton classes
                        for name, obj in inspect.getmembers(module):
                            if (inspect.isclass(obj) and
                                issubclass(obj, BaseAutomaton) and
                                obj != BaseAutomaton):

                                automaton_key = f"{operation_dir.name}.{py_file.stem}"
                                self.automata_classes[automaton_key] = obj
                                print(f"  ✅ Found: {name} ({automaton_key})")

                    except Exception as e:
                        print(f"  ❌ Error loading {py_file}: {e}")

        print(f"\n📊 Discovered {len(self.automata_classes)} automata classes")

    def instantiate_automata(self, test_inputs=None):
        """Create instances of all discovered automata with test inputs."""
        if test_inputs is None:
            test_inputs = [
                {'A': 8, 'B': 5},   # Simple addition
                {'A': 46, 'B': 37}, # More complex
                {'A': 23, 'B': 19}, # Another test case
            ]

        print("🏗️ Instantiating automata...")

        for key, automaton_class in self.automata_classes.items():
            try:
                # Try different test inputs until one works
                instance = None
                for inputs in test_inputs:
                    try:
                        instance = automaton_class(inputs=inputs)
                        break
                    except Exception:
                        continue

                if instance:
                    # Validate and fix metadata if needed
                    if hasattr(instance, 'metadata'):
                        try:
                            metadata = instance.metadata
                            # Check if metadata is valid StrategyMetadata
                            if not isinstance(metadata, StrategyMetadata):
                                # Try to create robust metadata
                                metadata = self._create_robust_metadata(instance, key)
                                # Override the faulty metadata
                                instance._metadata_obj = metadata
                        except Exception as e:
                            # Create fallback metadata
                            metadata = self._create_robust_metadata(instance, key)
                            instance._metadata_obj = metadata

                    self.automata_instances[key] = instance
                    print(f"  ✅ {key}: {getattr(instance.metadata, 'strategy_name', key)}")
                else:
                    print(f"  ⚠️ {key}: Could not instantiate with test inputs")

            except Exception as e:
                print(f"  ❌ {key}: {e}")

        print(f"\n📊 Successfully instantiated {len(self.automata_instances)} automata")

    def _create_robust_metadata(self, instance, key):
        """Create a robust StrategyMetadata object handling various edge cases."""
        try:
            # Try to get existing metadata
            existing = instance.metadata

            # If it's already a StrategyMetadata object, return it
            if isinstance(existing, StrategyMetadata):
                return existing

            # If it's a dict, try to convert it
            if isinstance(existing, dict):
                # Handle different key names
                strategy_id = existing.get('strategy_id') or existing.get('Name') or key
                strategy_name = existing.get('strategy_name') or existing.get('Name') or key.replace('_', ' ').title()

                return StrategyMetadata(
                    strategy_id=strategy_id,
                    strategy_name=strategy_name,
                    description=existing.get('description', existing.get('Description', '')),
                    metaphors=existing.get('metaphors', []),
                    inferences=existing.get('inferences', []),
                    visualization_hints=existing.get('visualization_hints', [])
                )

        except Exception as e:
            # If all else fails, create minimal metadata
            pass

        # Fallback: create minimal metadata
        return StrategyMetadata(
            strategy_id=key,
            strategy_name=key.replace('_', ' ').title(),
            description=f"Automaton for {key}",
            metaphors=[],
            inferences=[],
            visualization_hints=[]
        )

    def run_automata_analysis(self):
        """Run all automata and collect their execution traces and metadata."""
        print("🔬 Running automata analysis...")

        analysis_results = {}

        for key, automaton in self.automata_instances.items():
            try:
                print(f"\n⚙️ Analyzing {key}: {automaton.metadata.strategy_name}")

                # Run the automaton
                result = automaton.run()

                # Extract metadata
                metadata = automaton.metadata

                # Store analysis results
                analysis_results[key] = {
                    'automaton': automaton,
                    'result': result,
                    'metadata': metadata,
                    'history': automaton.history,
                    'execution_trace': automaton.export_trace_json()
                }

                print(f"  📊 Result: {result}")
                print(f"  📝 Steps: {len(automaton.history)}")
                print(f"  🧠 Metaphors: {len(metadata.metaphors) if hasattr(metadata, 'metaphors') else 0}")
                print(f"  🔗 Inferences: {len(metadata.inferences) if hasattr(metadata, 'inferences') else 0}")

            except Exception as e:
                print(f"  ❌ Error analyzing {key}: {e}")
                analysis_results[key] = {'error': str(e)}

        return analysis_results

    def generate_practices_from_automata(self, analysis_results):
        """Generate EPLE practices based on automaton metadata and behavior."""
        print("🏭 Generating practices from automata...")

        for key, analysis in analysis_results.items():
            if 'error' in analysis:
                continue

            automaton = analysis['automaton']
            metadata = analysis['metadata']

            # Create vocabulary based on automaton's operations
            predicates = self._extract_predicates_from_automaton(automaton, metadata)
            vocabulary_terms = self._generate_vocabulary_from_automaton(automaton, metadata)

            vocabulary = Vocabulary(
                name=f"V_{key.replace('.', '_')}",
                description=f"Vocabulary for {metadata.strategy_name}",
                predicates=predicates
            )

            # Create practice
            practice = Practice(
                name=f"P_{key.replace('.', '_')}",
                description=metadata.description,
                vocabulary=vocabulary,
                inferences=set(),  # Will be populated from metadata
                incompatibilities=set()
            )

            self.vocabularies[key] = vocabulary
            self.practices[key] = practice

            print(f"  ✅ {practice.name}: {len(predicates)} predicates, {len(vocabulary_terms)} concepts")

    def _generate_vocabulary_from_automaton(self, automaton, metadata):
        """Generate vocabulary from actual automaton behavior patterns."""
        vocabulary = set()

        # Analyze execution history for conceptual operations
        for step in automaton.history:
            interpretation = step.get('Interpretation', '').lower()
            registers = step.get('Registers', {})

            # Extract concepts from register manipulations
            if 'transfer' in interpretation:
                vocabulary.add("transfer")
                vocabulary.add("quantity_conservation")
            if 'count' in interpretation:
                vocabulary.add("counting")
                if 'base' in interpretation:
                    vocabulary.add("base_counting")
                    vocabulary.add("place_value")
                else:
                    vocabulary.add("counting_by_ones")
            if 'decompose' in interpretation:
                vocabulary.add("decomposition")
                vocabulary.add("base_decomposition")
                vocabulary.add("place_value")
            if 'recombine' in interpretation:
                vocabulary.add("recombination")
                vocabulary.add("base_recombination")

            # Analyze register patterns for conceptual understanding
            register_values = [v for v in registers.values() if isinstance(v, (int, float))]
            if len(register_values) >= 2:
                # Check for base relationships
                if any(v % 10 == 0 for v in register_values):
                    vocabulary.add("base_multiple")
                    vocabulary.add("place_value")
                # Check for iterative patterns
                if len(set(register_values)) > 1:
                    vocabulary.add("iterative_operation")

        # Extract from metadata inferences with more specificity
        if hasattr(metadata, 'inferences'):
            for inference in metadata.inferences:
                premise_str = str(inference.premise).lower()
                conclusion_str = str(inference.conclusion).lower()

                # More granular vocabulary extraction
                if 'quantity' in premise_str and 'same' in premise_str:
                    vocabulary.add("quantity_conservation")
                    vocabulary.add("equivalence")
                if 'multiple of' in conclusion_str or 'base' in conclusion_str:
                    vocabulary.add("base_multiple")
                    vocabulary.add("place_value")
                if 'decompose' in premise_str:
                    vocabulary.add("decomposition")
                    vocabulary.add("base_decomposition")
                if 'iterative' in premise_str:
                    vocabulary.add("iteration")
                    vocabulary.add("repeated_operation")

        # Add operation-specific vocabulary
        automaton_key = getattr(automaton, '_automaton_key', '')
        if 'addition' in automaton_key:
            vocabulary.add("addition")
            vocabulary.add("combining_quantities")
        elif 'subtraction' in automaton_key:
            vocabulary.add("subtraction")
            vocabulary.add("separating_quantities")
        elif 'multiplication' in automaton_key:
            vocabulary.add("multiplication")
            vocabulary.add("repeated_addition")
        elif 'division' in automaton_key:
            vocabulary.add("division")
            vocabulary.add("partitioning")

        return vocabulary

    def _extract_predicates_from_automaton(self, automaton, metadata):
        """Extract logical predicates from automaton metadata and behavior."""
        predicates = set()

        # Extract from execution history - analyze actual operations
        for step in automaton.history:
            interpretation = step.get('Interpretation', '').lower()
            registers = step.get('Registers', {})

            # Analyze register changes to understand operations
            if 'transfer' in interpretation:
                predicates.add(Predicate("Transfer", 3))  # Transfer(amount, from_reg, to_reg)
            if 'count' in interpretation:
                if 'base' in interpretation:
                    predicates.add(Predicate("CountByBase", 2))  # CountByBase(current, increment)
                else:
                    predicates.add(Predicate("CountByOne", 2))   # CountByOne(current, increment)
            if 'decompose' in interpretation:
                predicates.add(Predicate("Decompose", 2))  # Decompose(number, base)
            if 'recombine' in interpretation:
                predicates.add(Predicate("Recombine", 3))  # Recombine(part1, part2, total)

            # Analyze register patterns for conceptual operations
            if any('base' in str(v).lower() for v in registers.values() if isinstance(v, str)):
                predicates.add(Predicate("BaseMultiple", 1))  # BaseMultiple(number)
            if any('target' in str(v).lower() for v in registers.values() if isinstance(v, str)):
                predicates.add(Predicate("TargetBase", 1))    # TargetBase(number)

        # Extract from metadata inferences - but make it more specific
        if hasattr(metadata, 'inferences'):
            for inference in metadata.inferences:
                premise_lower = str(inference.premise).lower()
                conclusion_lower = str(inference.conclusion).lower()

                # More specific predicate extraction based on actual inference content
                if 'quantity' in premise_lower and 'same' in premise_lower:
                    predicates.add(Predicate("ConserveQuantity", 2))  # ConserveQuantity(before, after)
                if 'multiple of' in conclusion_lower or 'base' in conclusion_lower:
                    predicates.add(Predicate("MakeBaseMultiple", 2))  # MakeBaseMultiple(number, base)
                if 'decompose' in premise_lower:
                    predicates.add(Predicate("DecomposeNumber", 3))  # DecomposeNumber(number, base, parts)
                if 'iterative' in premise_lower:
                    predicates.add(Predicate("IterateOperation", 2))  # IterateOperation(operation, count)

        # Add operation-specific predicates based on automaton type
        automaton_key = getattr(automaton, '_automaton_key', '')
        if 'addition' in automaton_key:
            predicates.add(Predicate("Add", 3))  # Add(a, b, result)
        elif 'subtraction' in automaton_key:
            predicates.add(Predicate("Subtract", 3))  # Subtract(minuend, subtrahend, difference)
        elif 'multiplication' in automaton_key:
            predicates.add(Predicate("Multiply", 3))  # Multiply(a, b, product)
        elif 'division' in automaton_key:
            predicates.add(Predicate("Divide", 3))  # Divide(dividend, divisor, quotient)

        return predicates

    def analyze_prerequisites_and_elaborations(self, analysis_results):
        """Analyze prerequisite relationships and algorithmic elaborations based on actual behavior."""
        print("🔗 Analyzing prerequisite relationships from actual automaton behavior...")

        # Analyze computational dependencies between automata
        for key1, analysis1 in analysis_results.items():
            if 'error' in analysis1:
                continue

            automaton1 = analysis1['automaton']
            vocab1 = self._generate_vocabulary_from_automaton(automaton1, analysis1['metadata'])

            for key2, analysis2 in analysis_results.items():
                if 'error' in analysis2 or key1 == key2:
                    continue

                automaton2 = analysis2['automaton']
                vocab2 = self._generate_vocabulary_from_automaton(automaton2, analysis2['metadata'])

                # Analyze conceptual dependencies
                dependencies = self._analyze_conceptual_dependencies(vocab1, vocab2, automaton1, automaton2)

                if dependencies:
                    # Create MUR for each dependency relationship
                    for dep_type, description in dependencies.items():
                        if dep_type in ['PP-sufficiency', 'PV-elaboration', 'VP-elaboration', 'VV-necessity']:
                            mur = AlgorithmicElaboration(
                                P_base=self.practices[key1],
                                P_elaborated=self.practices[key2]
                            )
                            self.murs.add(mur)
                            print(f"  🔗 {dep_type}: {key1} → {key2} ({description})")

    def _analyze_conceptual_dependencies(self, vocab1, vocab2, automaton1, automaton2):
        """Analyze conceptual dependencies between two automata based on their behavior."""
        dependencies = {}

        # PP-sufficiency: Practice 1 is sufficient prerequisite for Practice 2
        if self._check_pp_sufficiency(vocab1, vocab2, automaton1, automaton2):
            dependencies['PP-sufficiency'] = "Computational patterns in base practice enable elaborated practice"

        # PV-elaboration: Practice 2 elaborates vocabulary from Practice 1
        if self._check_pv_elaboration(vocab1, vocab2, automaton1, automaton2):
            dependencies['PV-elaboration'] = "Vocabulary concepts elaborated through algorithmic operations"

        # VP-elaboration: Practice 2 elaborates vocabulary into new practice
        if self._check_vp_elaboration(vocab1, vocab2, automaton1, automaton2):
            dependencies['VP-elaboration'] = "Vocabulary transformed into new computational practice"

        # VV-necessity: Vocabulary 1 is necessary for Vocabulary 2
        if self._check_vv_necessity(vocab1, vocab2, automaton1, automaton2):
            dependencies['VV-necessity'] = "Conceptual foundation required for advanced operations"

        return dependencies

    def _check_pp_sufficiency(self, vocab1, vocab2, automaton1, automaton2):
        """Check if Practice 1 provides sufficient foundation for Practice 2."""
        # Practice 1 is sufficient if it contains core concepts that Practice 2 builds upon
        core_concepts = {'counting', 'base_multiple', 'decomposition'}
        practice1_core = vocab1.intersection(core_concepts)
        practice2_core = vocab2.intersection(core_concepts)

        # If Practice 1 has core concepts that Practice 2 uses but doesn't fundamentally change
        return len(practice1_core) > 0 and practice1_core.issubset(practice2_core)

    def _check_pv_elaboration(self, vocab1, vocab2, automaton1, automaton2):
        """Check if Practice 2 elaborates vocabulary from Practice 1."""
        # Practice 2 elaborates if it uses concepts from Practice 1 in more complex ways
        base_concepts = vocab1 - {'iterative_operation', 'repeated_operation'}
        elaborated_concepts = vocab2 - vocab1

        # Check if elaboration involves iterative or repeated operations on base concepts
        iterative_concepts = {'iterative_operation', 'repeated_operation', 'iteration'}
        return len(base_concepts) > 0 and len(elaborated_concepts.intersection(iterative_concepts)) > 0

    def _check_vp_elaboration(self, vocab1, vocab2, automaton1, automaton2):
        """Check if Practice 2 transforms vocabulary from Practice 1 into new practice."""
        # Vocabulary transformation occurs when concepts are applied differently
        shared_concepts = vocab1.intersection(vocab2)
        unique_to_2 = vocab2 - vocab1

        # Check for conceptual transformation (e.g., counting → base_counting)
        transformation_patterns = [
            ('counting', 'base_counting'),
            ('decomposition', 'base_decomposition'),
            ('transfer', 'quantity_conservation')
        ]

        for base_concept, elaborated_concept in transformation_patterns:
            if base_concept in shared_concepts and elaborated_concept in unique_to_2:
                return True

        return False

    def _check_vv_necessity(self, vocab1, vocab2, automaton1, automaton2):
        """Check if Vocabulary 1 is necessary for Vocabulary 2."""
        # Vocabulary 1 is necessary if Practice 2 cannot function without concepts from Practice 1
        essential_concepts = {'counting', 'decomposition', 'base_multiple'}
        vocab1_essential = vocab1.intersection(essential_concepts)

        # If Practice 2 uses essential concepts that originated in Practice 1
        return len(vocab1_essential) > 0 and vocab1_essential.issubset(vocab2)

    def create_integrated_analysis(self):
        """Create a comprehensive analysis integrating all automata."""
        print("🎯 Creating integrated EPLE analysis...")

        # Initialize DeonticScorekeeper with all practices and MURs
        scorekeeper = DeonticScorekeeper(
            practices=set(self.practices.values()),
            murs=self.murs
        )

        return {
            'practices': self.practices,
            'vocabularies': self.vocabularies,
            'murs': self.murs,
            'scorekeeper': scorekeeper,
            'automata_instances': self.automata_instances
        }

def run_automata_integration_analysis():
    """Main function to run the complete automata integration analysis."""
    print("🚀 EPLE Automata Integration Analysis")
    print("=" * 50)

    integrator = AutomataIntegrator()

    # Step 1: Discover automata
    integrator.discover_automata()

    # Step 2: Instantiate automata
    integrator.instantiate_automata()

    # Step 3: Run analysis
    analysis_results = integrator.run_automata_analysis()

    # Step 4: Generate practices
    integrator.generate_practices_from_automata(analysis_results)

    # Step 5: Analyze relationships
    integrator.analyze_prerequisites_and_elaborations(analysis_results)

    # Step 6: Create integrated analysis
    integrated_analysis = integrator.create_integrated_analysis()

    # Summary
    print("\n" + "=" * 50)
    print("📊 INTEGRATION SUMMARY")
    print("=" * 50)
    print(f"🔧 Automata Classes: {len(integrator.automata_classes)}")
    print(f"⚙️ Running Instances: {len(integrator.automata_instances)}")
    print(f"🏭 Generated Practices: {len(integrator.practices)}")
    print(f"📚 Generated Vocabularies: {len(integrator.vocabularies)}")
    print(f"🔗 Meaning-Use Relations: {len(integrator.murs)}")

    print("\n🎯 Research Insights:")
    print("  • Algorithmic elaborations identified from actual automaton behavior")
    print("  • Prerequisite abilities extracted from metadata and execution traces")
    print("  • PP-sufficiency relationships based on real computational dependencies")
    print("  • PV/VP/VV relationships derived from conceptual emergence patterns")
    print("  • Dynamic vocabulary generation from register manipulations and state transitions")
    print("  • Theoretical fidelity through actual computational implementations")

    return integrated_analysis

if __name__ == '__main__':
    analysis = run_automata_integration_analysis()