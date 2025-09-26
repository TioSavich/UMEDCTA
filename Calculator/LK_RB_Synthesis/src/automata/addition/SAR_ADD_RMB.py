import sys
import os
import json
import pandas as pd

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata, EmbodiedMetaphor, MaterialInference
from typing import Dict

class RMBAutomaton(BaseAutomaton):
    """
    A Register Machine model simulating the 'Rearranging to Make Bases' (RMB) strategy.
    This strategy involves taking a quantity from one number to make the other number a multiple of the base.
    """
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="SAR_ADD_RMB",
            strategy_name="RMB (Rearranging to Make Bases)",
            description=(
                "Simulates an addition strategy where one number is adjusted to a multiple of the base by 'borrowing' from the other. "
                "For A + B, it calculates K needed to make A a base multiple, then computes (A+K) + (B-K)."
            ),
            metaphors=[
                EmbodiedMetaphor(
                    name="Numbers as Physical Objects",
                    source_domain="Object Collection",
                    target_domain="Arithmetic",
                    entailments="A collection can be split and its parts moved without changing the total quantity. Rearranging parts makes counting easier."
                )
            ],
            inferences=[
                MaterialInference(
                    name="Conservation of Quantity",
                    premise="If you move a quantity from one pile to another, the total amount remains the same.",
                    conclusion="A + B = (A + K) + (B - K).",
                    prerequisites=["Counting skills", "Decomposition/Recomposition"]
                ),
                MaterialInference(
                    name="Making Tens",
                    premise="Adding to a multiple of ten is easier than adding to other numbers.",
                    conclusion="Transforming the problem to be A' + B' where A' is a multiple of 10 simplifies the final addition.",
                    prerequisites=["Knowledge of base-10 structure"]
                )
            ],
            visualization_hints=["Object Piles", "NumberLine with Jumps"]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        # Heuristically, it's easier to make the larger number a base multiple
        A = inputs.get('A', 0)
        B = inputs.get('B', 0)
        self.registers.update({
            'A': max(A, B),
            'B': min(A, B),
            'A_initial': max(A, B),
            'B_initial': min(A, B),
            'K': 0,
            'TargetBase': 0,
            'Result': 0
        })

    def execute_q_start(self):
        self._record_history(f"Inputs: A={self.registers['A_initial']}, B={self.registers['B_initial']}", highlight=True)
        self.transition('q_calc_K')

    def execute_q_calc_K(self):
        """Calculate K needed to reach the next base multiple by counting up from A."""
        a_val = self.registers['A']
        target_base = ((a_val // self.Base) + 1) * self.Base if a_val % self.Base != 0 else a_val
        self.registers['TargetBase'] = target_base
        
        k_needed = target_base - a_val
        self.registers['K'] = k_needed
        
        if k_needed == 0:
            self._record_history(f"A ({a_val}) is already a base multiple. No rearrangement needed.", highlight=True)
            self.transition('q_recombine')
        else:
            self._record_history(f"A is {a_val}. Target base is {target_base}. Need to transfer K={k_needed} from B.", highlight=True)
            self.transition('q_decompose_B')

    def execute_q_decompose_B(self):
        """Decompose B by transferring K."""
        k = self.registers['K']
        b = self.registers['B']

        if b >= k:
            self.registers['A'] += k
            self.registers['B'] -= k
            self._record_history(f"Transferred {k} from B to A. New state: A={self.registers['A']}, B={self.registers['B']}.")
            self.transition('q_recombine')
        else:
            self.Result = "Error: Strategy Failed"
            self._record_history(f"Strategy Failed: B ({b}) is too small to provide K ({k}).", highlight=True)
            self.transition('q_error')

    def execute_q_recombine(self):
        """Combine the new A (base multiple) and the remainder B."""
        self.registers['Result'] = self.registers['A'] + self.registers['B']
        self.Result = self.registers['Result']
        self._record_history(f"Combine rearranged numbers: {self.registers['A']} + {self.registers['B']} = {self.Result}.", highlight=True)
        self.transition('q_accept')

if __name__ == '__main__':
    # Test with Sarah's example: 8 + 5
    A_test, B_test = 8, 5
    rmb_automaton = RMBAutomaton(inputs={'A': A_test, 'B': B_test})
    result = rmb_automaton.run()
    
    print(f"\n--- {rmb_automaton.metadata.strategy_name} History ({A_test} + {B_test}) ---")
    print(f"Final Result: {result}")
    rmb_automaton.display_history()
    
    print("\n--- Full Trace JSON Export ---")
    print(rmb_automaton.export_trace_json())

    # Test with another example: 47 + 25
    A_test, B_test = 47, 25
    rmb_automaton_2 = RMBAutomaton(inputs={'A': A_test, 'B': B_test})
    result_2 = rmb_automaton_2.run()
    
    print(f"\n--- {rmb_automaton_2.metadata.strategy_name} History ({A_test} + {B_test}) ---")
    print(f"Final Result: {result_2}")
    rmb_automaton_2.display_history()
    print("\n--- Full Trace JSON Export ---")
    print(rmb_automaton_2.export_trace_json())