import sys
import os
import json
import pandas as pd

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata, EmbodiedMetaphor, MaterialInference
from typing import Dict

class CBBOAutomaton(BaseAutomaton):
    """
    A Register Machine model simulating the 'Counting Back by Bases and then Ones' (CBBO) strategy.
    This is typically a subtraction strategy, but is included here for completeness if it were
    to be adapted for a different context (e.g., finding a difference).
    """
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_name="CBBO (Counting Back by Bases and Ones)",
            description=(
                "Simulates a subtraction strategy where the second number (B) is decomposed into its base-ten and ones components. "
                "The strategy involves 'counting back' from the first number (A), first by the number of bases in B, and then by the number of ones."
            ),
            metaphors=[
                EmbodiedMetaphor(
                    name="Arithmetic as Motion Along a Path",
                    source_domain="Motion",
                    target_domain="Arithmetic",
                    entailments="Moving backwards along a path reduces the distance from the origin. The final position is the starting position minus the distance moved."
                )
            ],
            inferences=[
                MaterialInference(
                    name="Iterative Subtraction",
                    premise="A quantity can be subtracted by repeatedly subtracting a smaller unit.",
                    conclusion="Subtracting a number B is equivalent to subtracting '1' B times, or subtracting '10' (B//10) times and then '1' (B%10) times.",
                    prerequisites=["Counting skills", "Place value decomposition"]
                )
            ],
            visualization_hints=["NumberLine"]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'A': self.inputs.get('A', 0),
            'B': self.inputs.get('B', 0),
            'Difference': 0,
            'BaseCounter': 0,
            'OneCounter': 0
        })

    def execute_q_start(self):
        self._record_history(f"Inputs: A={self.registers['A']}, B={self.registers['B']}", highlight=True)
        self.transition('q_initialize')

    def execute_q_initialize(self):
        """Initialize Difference and decompose B."""
        self.registers['Difference'] = self.registers['A']
        self.registers['BaseCounter'] = self.registers['B'] // self.Base
        self.registers['OneCounter'] = self.registers['B'] % self.Base
        
        self._record_history(f"Initialize Difference to {self.registers['A']}. Decompose B: {self.registers['BaseCounter']} Bases, {self.registers['OneCounter']} Ones.")
        self.transition('q_subtract_bases')

    def execute_q_subtract_bases(self):
        """Iteratively subtract BaseUnits."""
        if self.registers['BaseCounter'] > 0:
            prev_diff = self.registers['Difference']
            self.registers['Difference'] -= self.Base
            self.registers['BaseCounter'] -= 1
            self._record_history(f"Count back by base: {prev_diff} -> {self.registers['Difference']}.")
        else:
            self._record_history("All bases subtracted. Transition to subtracting ones.", highlight=True)
            self.transition('q_subtract_ones')

    def execute_q_subtract_ones(self):
        """Iteratively subtract Ones."""
        if self.registers['OneCounter'] > 0:
            prev_diff = self.registers['Difference']
            self.registers['Difference'] -= 1
            self.registers['OneCounter'] -= 1
            self._record_history(f"Count back by one: {prev_diff} -> {self.registers['Difference']}.")
        else:
            self.Result = self.registers['Difference']
            self._record_history("All ones subtracted. Accept.", highlight=True)
            self.transition('q_accept')

if __name__ == '__main__':
    # Test the automaton with a subtraction example: 83 - 45.
    A_test = 83
    B_test = 45
    cbbo_automaton = CBBOAutomaton(inputs={'A': A_test, 'B': B_test})
    result = cbbo_automaton.run()
    
    print(f"\n--- {cbbo_automaton.metadata.strategy_name} History ({A_test} - {B_test}) ---")
    print(f"Final Result: {result}")
    cbbo_automaton.display_history()
    
    print("\n--- Full Trace JSON Export ---")
    print(cbbo_automaton.export_trace_json())
