
import sys
import os
import json
import pandas as pd

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata, EmbodiedMetaphor, MaterialInference
from typing import Dict

class CoboCountingOnByBasesThenOnesAutomaton(BaseAutomaton):
    """
    A Register Machine model simulating the 'Counting On By Bases and then Ones' (COBO) strategy.
    """
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="ADD_COBO",
            strategy_name="COBO (Counting On by Bases and Ones)",
            description=(
                "Simulates an addition strategy where the second number (B) is decomposed into its base-ten and ones components. "
                "The strategy involves 'counting on' from the first number (A), first by the number of bases in B, and then by the number of ones."
            ),
            metaphors=[
                EmbodiedMetaphor(
                    name="Arithmetic as Motion Along a Path",
                    source_domain="Motion",
                    target_domain="Arithmetic",
                    entailments="Moving along a path can be done in segments. The final position is the sum of the starting position and the lengths of all segments."
                )
            ],
            inferences=[
                MaterialInference(
                    name="Iterative Addition",
                    premise="A quantity can be added by repeatedly adding a smaller unit.",
                    conclusion="Adding a number B is equivalent to adding '1' B times, or adding '10' (B//10) times and then '1' (B%10) times.",
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
            'Sum': 0,
            'BaseCounter': 0,
            'OneCounter': 0
        })

    def execute_q_start(self):
        self._record_history(f"Inputs: A={self.registers['A']}, B={self.registers['B']}", highlight=True)
        self.transition('q_initialize')

    def execute_q_initialize(self):
        """Initialize Sum and decompose B."""
        self.registers['Sum'] = self.registers['A']
        self.registers['BaseCounter'] = self.registers['B'] // self.Base
        self.registers['OneCounter'] = self.registers['B'] % self.Base
        
        self._record_history(f"Initialize Sum to {self.registers['A']}. Decompose B: {self.registers['BaseCounter']} Bases, {self.registers['OneCounter']} Ones.")
        self.transition('q_add_bases')

    def execute_q_add_bases(self):
        """Iteratively add BaseUnits."""
        if self.registers['BaseCounter'] > 0:
            prev_sum = self.registers['Sum']
            self.registers['Sum'] += self.Base
            self.registers['BaseCounter'] -= 1
            self._record_history(f"Count on by base: {prev_sum} -> {self.registers['Sum']}.")
        else:
            self._record_history("All bases added. Transition to adding ones.", highlight=True)
            self.transition('q_add_ones')

    def execute_q_add_ones(self):
        """Iteratively add Ones."""
        if self.registers['OneCounter'] > 0:
            prev_sum = self.registers['Sum']
            self.registers['Sum'] += 1
            self.registers['OneCounter'] -= 1
            self._record_history(f"Count on by one: {prev_sum} -> {self.registers['Sum']}.")
        else:
            self.Result = self.registers['Sum']
            self._record_history("All ones added. Accept.", highlight=True)
            self.transition('q_accept')

if __name__ == '__main__':
    # Test the automaton with an example: 46 + 37.
    A_test = 46
    B_test = 37
    cobo_automaton = CoboCountingOnByBasesThenOnesAutomaton(inputs={'A': A_test, 'B': B_test})
    result = cobo_automaton.run()
    
    print(f"\n--- {cobo_automaton.metadata.strategy_name} History ({A_test} + {B_test}) ---")
    print(f"Final Result: {result}")
    
    # To display the styled DataFrame, you need to be in an environment like Jupyter
    # or have a library that can render HTML.
    # For console output, we'll just print the DataFrame.
    if not cobo_automaton.history:
        print("No history recorded.")
    else:
        df = pd.DataFrame(cobo_automaton.history)
        df['Registers'] = df['Registers'].apply(lambda x: json.dumps(x, indent=2))
        print(df)

    # print("\n--- Full Trace JSON Export ---")
    # print(cobo_automaton.export_trace_json())
