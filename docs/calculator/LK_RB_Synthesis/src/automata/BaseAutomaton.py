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
    def metadata(self) -> StrategyMetadata:
        """Must be implemented by subclasses to provide MUA metadata."""
        # Check if we have an overridden metadata object
        if hasattr(self, '_metadata_obj'):
            return self._metadata_obj

        # Try to get metadata from subclass - handle both patterns
        try:
            # Check if subclass has defined its own metadata property
            metadata_attr = getattr(type(self), 'metadata', None)
            if metadata_attr and hasattr(metadata_attr, 'fget') and metadata_attr.fget != BaseAutomaton.metadata.fget:
                # Subclass has its own metadata property implementation
                return metadata_attr.fget(self)
            else:
                # Try _get_metadata method
                return self._get_metadata()
        except (AttributeError, NotImplementedError):
            # Fallback for existing subclasses
            raise NotImplementedError("Subclasses must implement metadata property or _get_metadata method")

    def _get_metadata(self) -> StrategyMetadata:
        """Default implementation - subclasses should override this or the metadata property."""
        raise NotImplementedError("Subclasses must implement _get_metadata method")

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
        # Custom JSON encoder to handle dataclass objects
        def custom_encoder(obj):
            if hasattr(obj, '__dict__'):
                return obj.__dict__
            raise TypeError(f"Object of type {type(obj)} is not JSON serializable")

        # Note: Serialization might require handling dataclass conversion if not using Python 3.10+
        return json.dumps({
            "metadata": self.metadata.__dict__ if hasattr(self.metadata, '__dict__') else str(self.metadata),
            "history": self.history
        }, indent=4, default=custom_encoder)
