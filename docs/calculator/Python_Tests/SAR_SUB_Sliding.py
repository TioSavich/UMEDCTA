import pandas as pd
import math

class SlidingAutomaton:
    """
    A Register Machine model simulating the 'Sliding' (Constant Difference) strategy.
    Models the cognitive process including the iterative steps to calculate the adjustment K.
    """
    strategy_name = "Sliding to Make Bases (Constant Difference)"

    def __init__(self, M, S, Base=10):
        self.M = M
        self.S = S
        self.Base = Base
        self.history = []
        self.state = 'q_start'
        self.Result = 0
        
        # Main Registers
        self.K = 0
        self.M_adj = 0
        self.S_adj = 0
        
        # Internal registers for iteration
        self.TargetBase = 0
        self.TempCounter = 0

        if S > M:
            self.state = 'q_error'
            self._record_history(f"Error: Subtrahend ({S}) > Minuend ({M}).")

    def _record_history(self, interpretation, highlight=False):
        self.history.append({
            'State': self.state, 'Interpretation': interpretation,
            'K': self.K, 'M_adj': self.M_adj, 'S_adj': self.S_adj,
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
        self._record_history(f"Inputs: M={self.M}, S={self.S}. Target S for adjustment.", highlight=True)
        self.transition('q_init_K')

    # Subroutine: Calculate K (Count Up To Base)
    def execute_q_init_K(self):
        """Initialize the 'Count Up To Base' subroutine on S."""
        self.K = 0
        self.TempCounter = self.S
        
        # Determine the target base (e.g., 47 -> 50)
        if self.S > 0 and self.S % self.Base != 0:
             # Calculate the next highest multiple of the base
             self.TargetBase = ((self.S // self.Base) + 1) * self.Base
        else:
             self.TargetBase = self.S # Already at a base or zero
        
        self._record_history(f"Initializing K calculation: Counting from {self.S} to {self.TargetBase}.")
        self.transition('q_loop_K')

    def execute_q_loop_K(self):
        """Iteratively count up to the base."""
        if self.TempCounter < self.TargetBase:
            # Primitive counting operation
            self.TempCounter += 1
            self.K += 1
            self._record_history(f"Counting Up: {self.TempCounter}, K={self.K}")
        else:
            self._record_history(f"K needed to reach base is {self.K}.", highlight=True)
            self.transition('q_adjust')

    def execute_q_adjust(self):
        """Apply K to both M and S (The Slide)."""
        self.S_adj = self.S + self.K # Should equal TargetBase
        self.M_adj = self.M + self.K
        self._record_history(f"Sliding both by +{self.K}. New problem: {self.M_adj} - {self.S_adj}.", highlight=True)
        self.transition('q_subtract')

    def execute_q_subtract(self):
        """Perform the simplified subtraction."""
        # This step is cognitively simple because S_adj is a base multiple.
        self.Result = self.M_adj - self.S_adj
        self._record_history(f"Perform Subtraction: {self.M_adj} - {self.S_adj} = {self.Result}.", highlight=True)
        self.transition('q_accept')

    def execute_q_accept(self):
         # Final state logic (if any additional recording is needed)
         pass

    def display_history(self, summarized=True):
        print(f"\n--- {self.strategy_name} History ({self.M} - {self.S}) ---")
        df = pd.DataFrame(self.history)
        display_cols = ['State', 'Interpretation', 'K', 'M_adj', 'S_adj']

        if summarized:
             print("Summary Trace:")
             summary_df = df[df['Highlight'] == True]
             if not summary_df.empty:
                print(summary_df[display_cols].to_markdown(index=False))
        else:
            print("Full Iterative Trace:")
            print(df[display_cols].to_markdown(index=False))

# Test Case: Example from PDF (73 - 47)
M_test = 73
S_test = 47
sliding_auto = SlidingAutomaton(M=M_test, S=S_test)
sliding_auto.run()
sliding_auto.display_history(summarized=False)