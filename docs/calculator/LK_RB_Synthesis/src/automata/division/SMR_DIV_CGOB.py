import sys
import os
from typing import Dict

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata

class ConversionToGroupsOtherThanBasesCgobDivisionAutomaton(BaseAutomaton):
    _metadata = {
        "Name": "Conversion to Groups Other than Bases (CGOB Division)",
        "Description": "Leverage base decomposition of dividend T and analysis of base/divisor relation.",
        "Cognitive Steps": ['q_init', 'q_analyze', 'q_processBases', 'q_combineR', 'q_processR'],
        "Examples": []
    }

    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="SMR_DIV_CGOB",
            strategy_name="Conversion to Groups Other than Bases (CGOB Division)",
            description="Leverage base decomposition of dividend T and analysis of base/divisor relation.",
            metaphors=[],
            inferences=[],
            visualization_hints=[]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'T': self.inputs.get('T', 42),  # Dividend
            'D': self.inputs.get('D', 6),   # Divisor
            'Quotient': 0,
            'Remainder': 0,
            'CurrentValue': 0
        })

    def execute_q_start(self):
        self._record_history(f"Starting division: {self.registers['T']} ÷ {self.registers['D']}", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        """Initialize division process."""
        self.registers['CurrentValue'] = self.registers['T']
        self.registers['Quotient'] = 0
        self.registers['Remainder'] = 0
        self._record_history(f"Initialized: T={self.registers['T']}, D={self.registers['D']}")
        self.transition('q_analyze')

    def execute_q_analyze(self):
        """Analyze the division problem."""
        t = self.registers['T']
        d = self.registers['D']

        if d == 0:
            self.Result = "Error: Division by zero"
            self.transition('q_error')
            return

        # Simple division for now
        quotient = t // d
        remainder = t % d

        self.registers['Quotient'] = quotient
        self.registers['Remainder'] = remainder

        self._record_history(f"Analysis complete: {t} ÷ {d} = {quotient} remainder {remainder}")
        self.transition('q_processBases')

    def execute_q_processBases(self):
        """Process base components."""
        self._record_history("Processing base components")
        self.transition('q_combineR')

    def execute_q_combineR(self):
        """Combine results."""
        self._record_history("Combining results")
        self.transition('q_processR')

    def execute_q_processR(self):
        """Process final result."""
        quotient = self.registers['Quotient']
        remainder = self.registers['Remainder']

        if remainder == 0:
            self.Result = quotient
            self._record_history(f"Division complete: {self.registers['T']} ÷ {self.registers['D']} = {self.Result}", highlight=True)
        else:
            self.Result = f"{quotient} remainder {remainder}"
            self._record_history(f"Division complete with remainder: {self.Result}", highlight=True)

        self.transition('q_accept')
