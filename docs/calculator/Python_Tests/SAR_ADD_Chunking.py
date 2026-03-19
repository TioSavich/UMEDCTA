import pandas as pd

class ChunkingAutomaton:
    """
    A Register Machine model simulating the 'Chunking by Bases and Ones' strategy.
    Models the cognitive process including the iterative steps of the RMB subroutine.
    """
    def __init__(self, A, B, Base=10):
        self.A = A
        self.B = B
        self.Base = Base
        
        # Registers
        self.Sum = 0
        self.BasesRemaining = 0
        self.OnesRemaining = 0
        self.K = 0 # Strategic gap for ones
        
        # Internal registers for iteration
        self.internal_sum_temp = 0 # Used during iterative K calculation
        self.TargetBase = 0

        self.state = 'q_start'
        self.history = []

    def _record_history(self, interpretation, highlight=False):
        self.history.append({
            'State': self.state, 'Interpretation': interpretation,
            'Sum': self.Sum, 'BasesRem': self.BasesRemaining, 'OnesRem': self.OnesRemaining, 'K': self.K,
            'Highlight': highlight
        })

    def transition(self, next_state):
        self.state = next_state
        # Reset K and internal counters when moving between major phases (e.g., exiting the RMB loop)
        if next_state in ['q_init_ones_chunk', 'q_accept']:
             self.K = 0
             self.internal_sum_temp = 0

    def run(self):
        while self.state not in ['q_accept', 'q_error']:
            # Dynamically call the method corresponding to the state
            executor = getattr(self, f"execute_{self.state}", self.execute_error)
            executor()
        return self.Sum

    def execute_error(self):
        self._record_history(f"Error: Unknown state {self.state}")
        self.transition('q_error')

    # --- State Execution Methods ---

    def execute_q_start(self):
        self._record_history(f"Inputs: A={self.A}, B={self.B}", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        """Initialize Sum and decompose B."""
        self.Sum = self.A
        self.BasesRemaining = (self.B // self.Base) * self.Base
        self.OnesRemaining = self.B % self.Base
        self._record_history(f"Initialize Sum to {self.A}. Decompose B: {self.BasesRemaining} + {self.OnesRemaining}.")
        self.transition('q_add_base_chunk')

    def execute_q_add_base_chunk(self):
        """Add the entire base chunk (Compressed COBO)."""
        if self.BasesRemaining > 0:
            Chunk = self.BasesRemaining
            self.Sum += Chunk
            self.BasesRemaining = 0
            self._record_history(f"Add Base Chunk (+{Chunk}). Sum = {self.Sum}.", highlight=True)
        else:
            self._record_history("No bases to add.")
        self.transition('q_init_ones_chunk')

    def execute_q_init_ones_chunk(self):
        """Check if ones remain and transition accordingly (RMB Subroutine Start)."""
        if self.OnesRemaining > 0:
            self._record_history(f"Begin strategic chunking of remaining ones ({self.OnesRemaining}).")
            self.transition('q_init_K')
        else:
            self._record_history("All ones added. Accepting.", highlight=True)
            self.transition('q_accept')

    # Subroutine: Calculate K (Count Up To Base)
    def execute_q_init_K(self):
        """Initialize the 'Count Up To Base' subroutine."""
        self.K = 0
        self.internal_sum_temp = self.Sum
        
        # Determine the target base
        if self.Sum > 0 and self.Sum % self.Base != 0:
             self.TargetBase = ((self.Sum // self.Base) + 1) * self.Base
        else:
             self.TargetBase = self.Sum # Already at a base or zero
        
        self._record_history(f"Calculating K: Counting from {self.Sum} to {self.TargetBase}.")
        self.transition('q_loop_K')

    def execute_q_loop_K(self):
        """Iteratively count up to the base."""
        if self.internal_sum_temp < self.TargetBase:
            self.internal_sum_temp += 1
            self.K += 1
            self._record_history(f"Counting Up: {self.internal_sum_temp}, K={self.K}")
        else:
            self._record_history(f"K needed to reach base is {self.K}.")
            self.transition('q_add_ones_chunk')

    def execute_q_add_ones_chunk(self):
        """Apply the strategic chunk K or the remainder."""
        # Condition 1: Sufficient ones to make the base using K
        if self.OnesRemaining >= self.K and self.K > 0:
            Chunk = self.K
            self.Sum += Chunk
            self.OnesRemaining -= Chunk
            self._record_history(f"Add Strategic Chunk (+{Chunk}) to make base. Sum = {self.Sum}.", highlight=True)
            
        # Condition 2: Insufficient ones for K, or K is 0 (already at base)
        elif self.OnesRemaining > 0:
            Chunk = self.OnesRemaining
            self.Sum += Chunk
            self.OnesRemaining = 0
            self._record_history(f"Add Remaining Chunk (+{Chunk}). Sum = {self.Sum}.", highlight=True)
        
        # Loop back to check status or exit
        self.transition('q_init_ones_chunk')


    def display_history(self, summarized=True):
        print(f"\n--- Chunking Execution History ({self.A} + {self.B}) ---")
        df = pd.DataFrame(self.history)
        display_cols = ['State', 'Interpretation', 'Sum', 'BasesRem', 'OnesRem', 'K']

        if summarized:
             print("Summary Trace:")
             summary_df = df[df['Highlight'] == True]
             print(summary_df[display_cols].to_markdown(index=False))
        else:
            print("Full Iterative Trace:")
            print(df[display_cols].to_markdown(index=False))

# Test Case: Dionne's example (46 + 37)
chunking_46_37 = ChunkingAutomaton(A=46, B=37)
chunking_46_37.run()
chunking_46_37.display_history(summarized=False)