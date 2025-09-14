
from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata

class SubtractionCbboCountingBackAutomaton(BaseAutomaton):
    _metadata = {
        "Name": "Subtraction CBBO (Counting Back)",
        "Description": "Start at M, subtract base units from decomposed S, then ones.",
        "Cognitive Steps": ['q_init', 'q_sub_bases', 'q_sub_ones'],
        "Examples": []
    }

    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(**self._metadata)

    def execute_q_start(self):
        # Initial state logic
        pass

    def execute_q_init(self):
        # Logic for init
        pass

    def execute_q_sub_bases(self):
        # Logic for sub_bases
        pass

    def execute_q_sub_ones(self):
        # Logic for sub_ones
        pass
