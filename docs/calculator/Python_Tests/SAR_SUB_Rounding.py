import pandas as pd
import math

class SubtractionRoundingKevin:
    """
    A Register Machine model simulating Kevin's double-rounding strategy (e.g., 84-29).
    Rounds both M and S down, calculates intermediate result, and adjusts sequentially, 
    incorporating strategic chunking for the final adjustment.
    """
    strategy_name = "Subtraction Rounding (Kevin's Double Round Down)"

    def __init__(self, M, S, Base=10):
        self.M = M
        self.S = S
        self.Base = Base
        self.history = []
        self.state = 'q_start'
        self.Result = 0

        # Registers
        self.M_rounded = 0; self.K_M = 0 # Adjustment for M (Amount rounded down)
        self.S_rounded = 0; self.K_S = 0 # Adjustment for S (Amount rounded down)
        self.TempResult = 0
        
        # Internal registers for iterative adjustment (Chunking K_S)
        self.K_S_Remaining = 0
        self.Chunk = 0

        if S > M:
            self.state = 'q_error'
            self._record_history(f"Error: Subtrahend ({S}) > Minuend ({M}).")

    def _record_history(self, interpretation, highlight=False):
        record = {
            'State': self.state, 'Interpretation': interpretation,
            'K_M': self.K_M, 'K_S': self.K_S, 'TempResult': self.TempResult,
            'Highlight': highlight
        }
        # Add K_S_Remaining only if it's relevant (during the adjustment loop)
        if self.state.startswith('q_loop_adjust_S') or self.state.startswith('q_init_adjust_S'):
             record['K_S_Rem'] = self.K_S_Remaining
             
        self.history.append(record)

    def transition(self, next_state):
        self.state = next_state

    def run(self):
        while self.state not in ['q_accept', 'q_error']:
            executor = getattr(self, f"execute_{self.state}", self.execute_error)
            executor()
        return self.Result

    def execute_error(self):
        if self.state != 'q_error':
            self._record_history(f"Error: Entered unknown state {self.state}")
            self.transition('q_error')

    # --- State Execution Methods ---

    def execute_q_start(self):
        self._record_history(f"Inputs: M={self.M}, S={self.S}.", highlight=True)
        self.transition('q_round_M')

    def execute_q_round_M(self):
        """Round M down to the nearest base."""
        # Models the cognitive step of identifying the lower base and the difference.
        self.K_M = self.M % self.Base
        self.M_rounded = self.M - self.K_M
        self._record_history(f"Round M down: {self.M} -> {self.M_rounded}. (K_M = {self.K_M}).")
        self.transition('q_round_S')

    def execute_q_round_S(self):
        """Round S down to the nearest base."""
        self.K_S = self.S % self.Base
        self.S_rounded = self.S - self.K_S
        self._record_history(f"Round S down: {self.S} -> {self.S_rounded}. (K_S = {self.K_S}).")
        self.transition('q_subtract')

    def execute_q_subtract(self):
        """Calculate the intermediate result."""
        self.TempResult = self.M_rounded - self.S_rounded
        self._record_history(f"Intermediate Subtraction: {self.M_rounded} - {self.S_rounded} = {self.TempResult}.", highlight=True)
        self.transition('q_adjust_M')

    def execute_q_adjust_M(self):
        """Adjust for M. M was rounded down (result too small). Add K_M back."""
        prev = self.TempResult
        self.TempResult += self.K_M
        self._record_history(f"Adjust for M (Add K_M): {prev} + {self.K_M} = {self.TempResult}.", highlight=True)
        self.transition('q_init_adjust_S')

    def execute_q_init_adjust_S(self):
        """Initialize adjustment for S. S was rounded down (result too big). Subtract K_S."""
        self.K_S_Remaining = self.K_S
        if self.K_S_Remaining > 0:
            self._record_history(f"Begin Adjust for S (Subtract K_S): Need to subtract {self.K_S_Remaining}.")
            self.transition('q_loop_adjust_S')
        else:
            # If K_S was 0, proceed to the loop to finalize
            self.transition('q_loop_adjust_S') 

    def execute_q_loop_adjust_S(self):
        """Iteratively subtract K_S using strategic chunking (as Kevin did)."""
        if self.K_S_Remaining == 0:
            self.Result = self.TempResult
            self._record_history(f"Adjustment for S complete. Final Result = {self.Result}.", highlight=True)
            self.transition('q_accept')
            return

        # Determine the strategic chunk (subtract down to the previous base - Inverse RMB)
        # Models Kevin's move from 64 -> 60 (Chunk=4) before subtracting the rest (5).
        
        K_to_prev_base = self.TempResult % self.Base
        
        if K_to_prev_base > 0 and self.K_S_Remaining >= K_to_prev_base:
             # Sufficient remaining to reach the previous base
             self.Chunk = K_to_prev_base
        else:
             # Either already at a base, or insufficient remaining. Subtract what's left.
             self.Chunk = self.K_S_Remaining

        # Apply the chunk
        prev = self.TempResult
        self.TempResult -= self.Chunk
        self.K_S_Remaining -= self.Chunk
        
        interpretation = f"Chunking Adjustment: {prev} - {self.Chunk} = {self.TempResult}."
        # Add interpretation note if a boundary was reached
        if self.TempResult % self.Base == 0 and self.Chunk > 0 and prev % self.Base != 0:
             interpretation += " (Reached base boundary)."
             
        self._record_history(interpretation)
        # Loop back to q_loop_adjust_S

    def display_history(self):
        print(f"\n--- {self.strategy_name} History ({self.M} - {self.S}) ---")
        df = pd.DataFrame(self.history)
        # Determine columns to display, handling the optional K_S_Rem
        display_cols = ['State', 'Interpretation', 'K_M', 'K_S', 'TempResult']
        if 'K_S_Rem' in df.columns:
             display_cols.append('K_S_Rem')
             
        if not df.empty:
            # Fill NaNs for cleaner display where K_S_Rem is not applicable
            df = df[display_cols].fillna('')
            
        print(df.to_markdown(index=False))

# Test Case: Kevin's example (84 - 29)
M_test = 84
S_test = 29
kevin_strategy = SubtractionRoundingKevin(M=M_test, S=S_test)
result = kevin_strategy.run()
kevin_strategy.display_history()