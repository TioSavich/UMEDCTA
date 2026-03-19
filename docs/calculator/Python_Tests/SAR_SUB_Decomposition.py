import pandas as pd

class DecompositionAutomaton:
    """
    A Register Machine model simulating the 'Decomposition' (Borrowing) strategy for subtraction.
    Models the Left-to-Right approach: Subtract bases first, then ones, decomposing if necessary.
    """
    def __init__(self, M, S, Base=10):
        self.M = M
        self.S = S
        self.Base = Base
        self.state = 'q_start'
        self.history = []
        self.Result = 0
        
        # Registers for place values (Simplified for 2 digits based on the example)
        # S=Subtrahend (Reference), R=Result (Working Memory); T=Tens, O=Ones
        self.S_T = 0; self.S_O = 0
        self.R_T = 0; self.R_O = 0

        if S > M:
            self.state = 'q_error'
            self._record_history(f"Error: Subtrahend ({S}) > Minuend ({M}).")

    def _record_history(self, interpretation, highlight=False):
        self.history.append({
            'State': self.state, 'Interpretation': interpretation,
            'R_Tens': self.R_T, 'R_Ones': self.R_O,
            'Highlight': highlight
        })

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
        self._record_history(f"Inputs: M={self.M}, S={self.S}", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        """Decompose M and S into Tens and Ones."""
        # Decompose S for reference
        self.S_T = self.S // self.Base; self.S_O = self.S % self.Base
        # Initialize Result registers to M components
        self.R_T = self.M // self.Base; self.R_O = self.M % self.Base
        
        self._record_history(f"Decompose M ({self.R_T}T+{self.R_O}O) and S ({self.S_T}T+{self.S_O}O).")
        self.transition('q_sub_bases')

    def execute_q_sub_bases(self):
        """Subtract the bases (Tens)."""
        Initial_R_T = self.R_T
        # In this L-to-R approach, we subtract the tens first.
        self.R_T -= self.S_T
        self._record_history(f"Subtract Bases: {Initial_R_T}T - {self.S_T}T = {self.R_T}T.", highlight=True)
        self.transition('q_check_ones')

    def execute_q_check_ones(self):
        """Check if there are enough ones to subtract."""
        if self.R_O >= self.S_O:
            self._record_history(f"Sufficient Ones ({self.R_O} >= {self.S_O}). Proceed.")
            self.transition('q_sub_ones')
        else:
            self._record_history(f"Insufficient Ones ({self.R_O} < {self.S_O}). Need decomposition.", highlight=True)
            self.transition('q_decompose')

    def execute_q_decompose(self):
        """Decompose (borrow) one ten into ones."""
        if self.R_T > 0:
            self.R_T -= 1
            self.R_O += self.Base
            self._record_history(f"Decomposed 1 Ten. New state: {self.R_T}T, {self.R_O}O.", highlight=True)
            self.transition('q_sub_ones')
        else:
            # Should be unreachable if M>=S
            self.transition('q_error')

    def execute_q_sub_ones(self):
        """Subtract the ones."""
        prev_O = self.R_O
        self.R_O -= self.S_O
        self._record_history(f"Subtract Ones: {prev_O}O - {self.S_O}O = {self.R_O}O.", highlight=True)
        self.transition('q_accept')

    def execute_q_accept(self):
        """Combine results."""
        self.Result = self.R_T * self.Base + self.R_O
        self._record_history(f"Accept. Final Result: {self.Result}.", highlight=True)

    def display_history(self, summarized=True):
        print(f"\n--- Decomposition (L-to-R) Execution History ({self.M} - {self.S}) ---")
        df = pd.DataFrame(self.history)
        display_cols = ['State', 'Interpretation', 'R_Tens', 'R_Ones']

        if summarized:
             print("Summary Trace:")
             summary_df = df[df['Highlight'] == True]
             if not summary_df.empty:
                print(summary_df[display_cols].to_markdown(index=False))
        else:
            print("Full Trace:")
            print(df[display_cols].to_markdown(index=False))

# Test Case 1: Joel's example (45 - 27) - Requires Decomposition
print("=== Test Case 1: 45 - 27 (Requires Decomposition) ===")
decomp_45_27 = DecompositionAutomaton(M=45, S=27)
decomp_45_27.run()
decomp_45_27.display_history(summarized=False)

# Test Case 2: No decomposition needed (48 - 23)
print("\n=== Test Case 2: 48 - 23 (No Decomposition Needed) ===")
decomp_48_23 = DecompositionAutomaton(M=48, S=23)
decomp_48_23.run()
decomp_48_23.display_history(summarized=True)