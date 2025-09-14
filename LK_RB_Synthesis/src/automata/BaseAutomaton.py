# src/automata/BaseAutomaton.py
import pandas as pd
from abc import ABC, abstractmethod
from src.analysis.MUA_Metadata import StrategyMetadata
import json
from typing import Dict

class BaseAutomaton(ABC):
    def __init__(self, inputs: Dict, Base=10):
        self.inputs = inputs # Dictionary of initial inputs (e.g., {'M': 73, 'S': 47})
        self.Base = Base
        self.history = []
        self.state = 'q_start'
        self.Result = 0
        self.registers = {} # Flexible dictionary for internal registers

    @property
    @abstractmethod
    def metadata(self) -> StrategyMetadata:
        """Must be implemented by subclasses to provide MUA metadata."""
        pass

    def _record_history(self, interpretation, highlight=False):
        # This method now automatically captures the state of all registers
        self.history.append({
            'State': self.state,
            'Interpretation': interpretation,
            'Registers': self.registers.copy(), # Crucial: use copy()
            'Highlight': highlight
        })

    def transition(self, next_state):
        self.state = next_state

    def run(self):
        while self.state not in ['q_accept', 'q_error']:
            executor = getattr(self, f"execute_{self.state}", self.execute_error)
            executor()
        return self.Result

    @abstractmethod
    def execute_q_start(self):
        pass

    def execute_error(self):
        if self.state != 'q_error':
            self._record_history(f"Error: Entered unknown state {self.state}")
            self.transition('q_error')

    def execute_q_accept(self):
        pass # Final state

    def display_history(self):
        """Displays the execution history as a pandas DataFrame."""
        if not self.history:
            print("No history recorded.")
            return
        df = pd.DataFrame(self.history)
        
        # Format the 'Registers' column to be more readable
        df['Registers'] = df['Registers'].apply(lambda x: json.dumps(x, indent=2))
        
        # Highlighting logic
        def highlight_rows(row):
            if row['Highlight']:
                return ['background-color: yellow'] * len(row)
            return [''] * len(row)

        styled_df = df.style.apply(highlight_rows, axis=1)
        return styled_df

    def export_trace_json(self):
        """Exports the execution history and metadata for visualization and analysis."""
        # Note: Serialization might require handling dataclass conversion if not using Python 3.10+
        return json.dumps({
            "metadata": self.metadata.__dict__,
            "history": self.history
        }, indent=4)
