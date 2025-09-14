import sys
import os
import json
import pandas as pd

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata, EmbodiedMetaphor, MaterialInference
from typing import Dict

class SlidingAutomaton(BaseAutomaton):
    """
    A Register Machine model simulating the 'Sliding' (Constant Difference) strategy.
    Models the cognitive process including the iterative steps to calculate the adjustment K.
    """

    @property
    def metadata(self) -> StrategyMetadata:
        """Provides MUA metadata for the Sliding strategy."""
        return StrategyMetadata(
            strategy_name="Sliding to Make Bases (Constant Difference)",
            description=(
                "Simulates the 'Sliding' (Constant Difference) strategy for subtraction. "
                "The core idea is to add or subtract the same amount (K) to both the minuend and subtrahend "
                "to make the subtrahend a multiple of the base, simplifying the subtraction. "
                "This relies on the principle that the difference between two numbers remains constant "
                "if both are shifted by the same amount."
            ),
            metaphors=[
                EmbodiedMetaphor(
                    name="Arithmetic as Motion Along a Path",
                    source_domain="Motion",
                    target_domain="Arithmetic",
                    entailments="The distance between two points on a path remains the same if both points are shifted by the same amount in the same direction."
                )
            ],
            inferences=[
                MaterialInference(
                    name="Invariance of Distance under Translation",
                    premise="The distance between M and S is D.",
                    conclusion="The distance between (M+K) and (S+K) is also D.",
                    prerequisites=["Understanding of path measurement", "Experience with rigid motion"]
                )
            ],
            visualization_hints=["NumberLine"]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        # Initialize registers
        self.registers = {
            'M': self.inputs.get('M', 0),
            'S': self.inputs.get('S', 0),
            'K': 0,
            'M_adj': 0,
            'S_adj': 0,
            'TargetBase': 0,
            'TempCounter': 0
        }

        if self.registers['S'] > self.registers['M']:
            self.state = 'q_error'
            self._record_history(f"Error: Subtrahend ({self.registers['S']}) > Minuend ({self.registers['M']}).")

    # --- State Execution Methods ---

    def execute_q_start(self):
        self._record_history(f"Inputs: M={self.registers['M']}, S={self.registers['S']}. Target S for adjustment.", highlight=True)
        self.transition('q_init_K')

    # Subroutine: Calculate K (Count Up To Base)
    def execute_q_init_K(self):
        """Initialize the 'Count Up To Base' subroutine on S."""
        self.registers['K'] = 0
        self.registers['TempCounter'] = self.registers['S']
        
        # Determine the target base (e.g., 47 -> 50)
        if self.registers['S'] > 0 and self.registers['S'] % self.Base != 0:
             # Calculate the next highest multiple of the base
             self.registers['TargetBase'] = ((self.registers['S'] // self.Base) + 1) * self.Base
        else:
             self.registers['TargetBase'] = self.registers['S'] # Already at a base or zero
        
        self._record_history(f"Initializing K calculation: Counting from {self.registers['S']} to {self.registers['TargetBase']}.")
        self.transition('q_loop_K')

    def execute_q_loop_K(self):
        """Iteratively count up to the base."""
        if self.registers['TempCounter'] < self.registers['TargetBase']:
            # Primitive counting operation
            self.registers['TempCounter'] += 1
            self.registers['K'] += 1
            self._record_history(f"Counting Up: {self.registers['TempCounter']}, K={self.registers['K']}")
        else:
            self._record_history(f"K needed to reach base is {self.registers['K']}.", highlight=True)
            self.transition('q_adjust')

    def execute_q_adjust(self):
        """Apply K to both M and S (The Slide)."""
        self.registers['S_adj'] = self.registers['S'] + self.registers['K'] # Should equal TargetBase
        self.registers['M_adj'] = self.registers['M'] + self.registers['K']
        self._record_history(f"Sliding both by +{self.registers['K']}. New problem: {self.registers['M_adj']} - {self.registers['S_adj']}.", highlight=True)
        self.transition('q_subtract')

    def execute_q_subtract(self):
        """Perform the simplified subtraction."""
        # This step is cognitively simple because S_adj is a base multiple.
        self.Result = self.registers['M_adj'] - self.registers['S_adj']
        self._record_history(f"Perform Subtraction: {self.registers['M_adj']} - {self.registers['S_adj']} = {self.Result}.", highlight=True)
        self.transition('q_accept')

# Test Case: Example from PDF (73 - 47)
if __name__ == '__main__':
    M_test = 73
    S_test = 47
    sliding_auto = SlidingAutomaton(inputs={'M': M_test, 'S': S_test})
    result = sliding_auto.run()
    
    print(f"\n--- {sliding_auto.metadata.strategy_name} History ({M_test} - {S_test}) ---")
    print(f"Final Result: {result}")

    # Display summarized history
    df = pd.DataFrame(sliding_auto.history)
    summary_df = df[df['Highlight'] == True]
    if not summary_df.empty:
        print("\nSummary Trace:")
        print(summary_df[['State', 'Interpretation', 'Registers']].to_markdown(index=False))

    # Export and print the full trace as JSON
    print("\n--- Full Trace JSON Export ---")
    print(sliding_auto.export_trace_json())
