import sys
import os
from typing import Dict

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata

class SubtractionRoundingAndAdjustingAutomaton(BaseAutomaton):
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="SAR_SUB_Rounding",
            strategy_name="Subtraction Rounding and Adjusting",
            description="Dual rounding yields simplified M' - S', then contrasting compensations.",
            metaphors=[],
            inferences=[],
            visualization_hints=[]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'M': self.inputs.get('M', 83),  # Minuend
            'S': self.inputs.get('S', 45),  # Subtrahend
            'M_rounded': 0,
            'S_rounded': 0,
            'K_M': 0,  # Adjustment for M
            'K_S': 0,  # Adjustment for S
            'Difference': 0,
            'Result': 0
        })

    def execute_q_start(self):
        self._record_history(f"Starting subtraction rounding: {self.registers['M']} - {self.registers['S']}", highlight=True)
        self.transition('q_roundM')

    def execute_q_roundM(self):
        """Round M up to next base."""
        m = self.registers['M']
        base = self.Base

        if m % base == 0:
            k_m = 0
            m_rounded = m
        else:
            k_m = base - (m % base)
            m_rounded = m + k_m

        self.registers['K_M'] = k_m
        self.registers['M_rounded'] = m_rounded

        self._record_history(f"Rounded M: {m} + {k_m} = {m_rounded}")
        self.transition('q_roundS')

    def execute_q_roundS(self):
        """Round S up to next base."""
        s = self.registers['S']
        base = self.Base

        if s % base == 0:
            k_s = 0
            s_rounded = s
        else:
            k_s = base - (s % base)
            s_rounded = s + k_s

        self.registers['K_S'] = k_s
        self.registers['S_rounded'] = s_rounded

        self._record_history(f"Rounded S: {s} + {k_s} = {s_rounded}")
        self.transition('q_subtract')

    def execute_q_subtract(self):
        """Subtract the rounded values."""
        m_rounded = self.registers['M_rounded']
        s_rounded = self.registers['S_rounded']
        difference = m_rounded - s_rounded

        self.registers['Difference'] = difference
        self._record_history(f"Subtracted: {m_rounded} - {s_rounded} = {difference}")
        self.transition('q_adjustM')

    def execute_q_adjustM(self):
        """Adjust back for M rounding."""
        difference = self.registers['Difference']
        k_m = self.registers['K_M']
        adjusted = difference - k_m

        self.registers['Difference'] = adjusted
        self._record_history(f"Adjusted for M: {difference} - {k_m} = {adjusted}")
        self.transition('q_adjustS')

    def execute_q_adjustS(self):
        """Adjust back for S rounding."""
        difference = self.registers['Difference']
        k_s = self.registers['K_S']
        final_result = difference + k_s

        self.registers['Result'] = final_result
        self.Result = final_result

        self._record_history(f"Adjusted for S: {difference} + {k_s} = {final_result}", highlight=True)
        self.transition('q_accept')
