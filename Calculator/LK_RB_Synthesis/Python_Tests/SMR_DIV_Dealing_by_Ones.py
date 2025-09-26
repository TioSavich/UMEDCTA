import pandas as pd
import numpy as np

class DealingByOnesAutomaton:
    """
    A Register Machine modeling the 'Dealing by Ones' strategy for Sharing Division.
    Models the cognitive process of round-robin distribution using an array for groups.
    """
    strategy_name = "Dealing by Ones (Sharing Division)"

    def __init__(self, T, N):
        self.T = T # Total Items
        self.N = N # Number of Groups
        
        # Registers
        self.Remaining = 0
        # Array representing the groups (boxes) in working memory
        self.Groups = np.zeros(N, dtype=int) if N > 0 else np.array([], dtype=int)
        self.CurrentIdx = 0

        self.state = 'q_init'
        self.history = []

        if N <= 0 and T > 0:
            self.state = 'q_error'
            self._record_history(f"Error: Cannot divide by N={N}.")

    def _record_history(self, interpretation, highlight=False):
        record = {
            'State': self.state, 'Interpretation': interpretation,
            'Remaining': self.Remaining,
            'Current Idx': self.CurrentIdx if self.N > 0 else 'N/A',
            # Display the current state of all groups (making a copy for the history)
            'Group State': np.array2string(self.Groups.copy(), separator=','),
            'Highlight': highlight
        }
        self.history.append(record)

    def transition(self, next_state):
        self.state = next_state

    def run(self):
        while self.state not in ['q_accept', 'q_error']:
            executor = getattr(self, f"execute_{self.state}", self.execute_error)
            executor()
        
        if self.state == 'q_accept' and self.N > 0:
            # The result is the count in any group (assuming perfect division as per the example)
            return self.Groups[0]
        return 0

    def execute_error(self):
        if self.state != 'q_error':
            self._record_history(f"Error: Entered unknown state {self.state}")
            self.transition('q_error')

    # --- State Execution Methods ---

    def execute_q_init(self):
        """Initialize the registers."""
        self.Remaining = self.T
        self.CurrentIdx = 0
        self._record_history(f"Initialize: {self.T} items to deal into {self.N} groups.", highlight=True)
        self.transition('q_loop_deal')

    def execute_q_loop_deal(self):
        """Iteratively deal one item in a round-robin fashion."""
        
        # Condition: Items remain to be dealt
        if self.Remaining > 0:
            # Deal one item
            self.Groups[self.CurrentIdx] += 1
            self.Remaining -= 1
            
            interpretation = f"Dealt 1 item to Group {self.CurrentIdx+1}."
            
            # Advance the index (Round-Robin)
            self.CurrentIdx = (self.CurrentIdx + 1) % self.N
            
            # Highlight if a round is complete
            is_round_complete = (self.CurrentIdx == 0)
            if is_round_complete:
                interpretation += " (Round complete)."
                
            self._record_history(interpretation, highlight=is_round_complete)
            # Stay in q_loop_deal
            
        # Condition: All items dealt
        else:
            result = self.Groups[0] if self.N > 0 else 0
            self._record_history(f"Dealing complete. Result: {result} per group.", highlight=True)
            self.transition('q_accept')


    def display_history(self, summarized=True):
        print(f"\n--- {self.strategy_name} History ({self.T} / {self.N}) ---")
        df = pd.DataFrame(self.history)
        display_cols = ['State', 'Interpretation', 'Remaining', 'Group State']

        if summarized:
             print("Summary Trace (Rounds):")
             # Filter for initialization and highlights (rounds completed)
             summary_df = df[df['Highlight'] == True]
             if not summary_df.empty:
                print(summary_df[display_cols].to_markdown(index=False))
        else:
            print("Full Iterative Trace:")
            print(df[display_cols].to_markdown(index=False))

# Test Case: Example from PDF (12 cupcakes into 4 boxes)
T_test = 12
N_test = 4
dealing = DealingByOnesAutomaton(T=T_test, N=N_test)
dealing.run()
dealing.display_history(summarized=False)