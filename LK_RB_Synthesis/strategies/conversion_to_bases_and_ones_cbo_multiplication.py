
import sys
import os
import json
import pandas as pd

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata, EmbodiedMetaphor, MaterialInference
from typing import Dict

class ConversionToBasesAndOnesCboMultiplicationAutomaton(BaseAutomaton):
    """
    A Register Machine model simulating multiplication using the distributive property
    by breaking the multiplier into bases and ones. (e.g., A x B = A * (B_bases + B_ones)).
    """
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="MULT_CBO",
            strategy_name="Conversion to Bases and Ones (CBO Multiplication)",
            description="Calculates A x B by decomposing B into its base and ones components (B_base + B_ones), calculating the partial products (A * B_base and A * B_ones), and summing them.",
            metaphors=[
                EmbodiedMetaphor(
                    name="Arithmetic as Object Collection",
                    source_domain="Object Collection",
                    target_domain="Arithmetic",
                    entailments="A collection of groups can be counted by first counting the items in the large groups (bases) and then counting the items in the small groups (ones)."
                )
            ],
            inferences=[
                MaterialInference(
                    name="Distributive Property of Multiplication",
                    premise="Multiplication distributes over addition.",
                    conclusion="A x (B + C) is equivalent to (A x B) + (A x C).",
                    prerequisites=["Understanding of multiplication and addition", "Number decomposition"]
                )
            ],
            visualization_hints=["ObjectArrays"]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'A': self.inputs.get('A', 0),
            'B': self.inputs.get('B', 0),
            'B_Bases': 0,
            'B_Ones': 0,
            'Product_Bases': 0,
            'Product_Ones': 0,
            'TotalProduct': 0
        })

    def execute_q_start(self):
        self._record_history(f"Inputs: A={self.registers['A']}, B={self.registers['B']}", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        """Decompose B into its base and ones components."""
        b = self.registers['B']
        self.registers['B_Bases'] = (b // self.Base) * self.Base
        self.registers['B_Ones'] = b % self.Base
        self._record_history(f"Decompose B ({b}) into {self.registers['B_Bases']} (bases) and {self.registers['B_Ones']} (ones).")
        self.transition('q_mult_base')

    def execute_q_mult_base(self):
        """Calculate the partial product for the base part."""
        a = self.registers['A']
        b_bases = self.registers['B_Bases']
        # This step assumes the multiplication of A by a multiple of the base is a single operation.
        self.registers['Product_Bases'] = a * b_bases
        self._record_history(f"Multiply A by B_Bases: {a} x {b_bases} = {self.registers['Product_Bases']}.")
        self.transition('q_mult_ones')

    def execute_q_mult_ones(self):
        """Calculate the partial product for the ones part."""
        a = self.registers['A']
        b_ones = self.registers['B_Ones']
        # This step assumes the multiplication of A by a single digit is a single operation.
        self.registers['Product_Ones'] = a * b_ones
        self._record_history(f"Multiply A by B_Ones: {a} x {b_ones} = {self.registers['Product_Ones']}.")
        self.transition('q_add_products')

    def execute_q_add_products(self):
        """Sum the partial products."""
        prod_bases = self.registers['Product_Bases']
        prod_ones = self.registers['Product_Ones']
        self.registers['TotalProduct'] = prod_bases + prod_ones
        self.Result = self.registers['TotalProduct']
        self._record_history(f"Sum partial products: {prod_bases} + {prod_ones} = {self.Result}.", highlight=True)
        self.transition('q_accept')

if __name__ == '__main__':
    # Test Case: 7 x 14 = 7 * (10 + 4) = 70 + 28 = 98
    A_test = 7
    B_test = 14
    cbo_mult_auto = ConversionToBasesAndOnesCboMultiplicationAutomaton(inputs={'A': A_test, 'B': B_test})
    result = cbo_mult_auto.run()
    
    print(f"\n--- {cbo_mult_auto.metadata.strategy_name} History ({A_test} x {B_test}) ---")
    print(f"Final Result: {result}")
    
    if not cbo_mult_auto.history:
        print("No history recorded.")
    else:
        df = pd.DataFrame(cbo_mult_auto.history)
        df['Registers'] = df['Registers'].apply(lambda x: json.dumps(x, indent=2))
        print(df)
