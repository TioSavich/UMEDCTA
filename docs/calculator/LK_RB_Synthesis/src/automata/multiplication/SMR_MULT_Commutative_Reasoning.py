import sys
import os
import json
import pandas as pd

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata, EmbodiedMetaphor, MaterialInference
from typing import Dict

class CommutativeReasoningMultiplicationAutomaton(BaseAutomaton):
    """
    A Register Machine model simulating multiplication using commutative reasoning.
    The strategy evaluates whether A x B or B x A is easier to compute (by picking the smaller multiplier)
    and then performs multiplication by repeated addition.
    """
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="MULT_COMM",
            strategy_name="Commutative Reasoning (Multiplication)",
            description="Selects the easier orientation for multiplication (e.g., 3 x 9 instead of 9 x 3) and then calculates via repeated addition.",
            metaphors=[
                EmbodiedMetaphor(
                    name="Arithmetic as Object Collection",
                    source_domain="Object Collection",
                    target_domain="Arithmetic",
                    entailments="3 groups of 9 is the same total as 9 groups of 3. It's easier to conceptualize and count the former."
                )
            ],
            inferences=[
                MaterialInference(
                    name="Commutativity of Multiplication",
                    premise="The order of factors does not change the product.",
                    conclusion="A x B is equivalent to B x A.",
                    prerequisites=["Understanding that multiplication can represent groups of items"]
                ),
                MaterialInference(
                    name="Multiplication as Repeated Addition",
                    premise="Multiplying by N is equivalent to adding a number to itself N times.",
                    conclusion="The product can be found by iterating additions.",
                    prerequisites=["Addition skills"]
                )
            ],
            visualization_hints=["ObjectArrays"]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'A': self.inputs.get('A', 0),
            'B': self.inputs.get('B', 0),
            'Multiplier': 0,
            'Multiplicand': 0,
            'Product': 0,
            'Counter': 0
        })

    def execute_q_start(self):
        self._record_history(f"Inputs: A={self.registers['A']}, B={self.registers['B']}", highlight=True)
        self.transition('q_evaluate')

    def execute_q_evaluate(self):
        """Evaluate which number is smaller to use as the multiplier."""
        a = self.registers['A']
        b = self.registers['B']
        self._record_history(f"Evaluate which is smaller: {a} or {b}.")
        self.transition('q_repackage')

    def execute_q_repackage(self):
        """Set the smaller number as the Multiplier."""
        a = self.registers['A']
        b = self.registers['B']
        if a < b:
            self.registers['Multiplier'] = a
            self.registers['Multiplicand'] = b
            self._record_history(f"Chose {a} as Multiplier (A < B). Problem is {a} x {b}.")
        else:
            self.registers['Multiplier'] = b
            self.registers['Multiplicand'] = a
            self._record_history(f"Chose {b} as Multiplier (B <= A). Problem is {b} x {a}.")
        
        self.registers['Counter'] = self.registers['Multiplier']
        self.transition('q_calc_loop')

    def execute_q_calc_loop(self):
        """Perform multiplication via repeated addition."""
        if self.registers['Counter'] > 0:
            multiplicand = self.registers['Multiplicand']
            prev_product = self.registers['Product']
            self.registers['Product'] += multiplicand
            self.registers['Counter'] -= 1
            self._record_history(f"Repeated addition: {prev_product} + {multiplicand} = {self.registers['Product']}. ({self.registers['Counter']} additions remaining).")
        else:
            self.Result = self.registers['Product']
            self._record_history(f"Calculation complete. Product = {self.Result}.", highlight=True)
            self.transition('q_accept')

if __name__ == '__main__':
    # Test Case: 3 x 9 (should be easier than 9 x 3)
    A_test = 9
    B_test = 3
    comm_auto = CommutativeReasoningMultiplicationAutomaton(inputs={'A': A_test, 'B': B_test})
    result = comm_auto.run()
    
    print(f"\n--- {comm_auto.metadata.strategy_name} History ({A_test} x {B_test}) ---")
    print(f"Final Result: {result}")
    
    if not comm_auto.history:
        print("No history recorded.")
    else:
        df = pd.DataFrame(comm_auto.history)
        df['Registers'] = df['Registers'].apply(lambda x: json.dumps(x, indent=2))
        print(df)
