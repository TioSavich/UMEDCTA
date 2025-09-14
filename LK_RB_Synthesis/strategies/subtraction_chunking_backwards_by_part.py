
from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata

class SubtractionChunkingBackwardsByPartAutomaton(BaseAutomaton):
    _metadata = {
        "Name": "Subtraction Chunking (Backwards by Part)",
        "Description": "Sequentially subtract decomposed parts of S from M.",
        "Cognitive Steps": ['q_init', 'q_chunk'],
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

    def execute_q_chunk(self):
        # Logic for chunk
        pass
