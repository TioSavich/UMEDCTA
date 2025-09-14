import pandas as pd

class C2C_MultiplicationAutomaton:
    """
    A Register Machine modeling the 'Coordinating Two Counts' (C2C) strategy for multiplication.
    Models the process of counting all items by ones while tracking group boundaries.
    """
    strategy_name = "Coordinating Two Counts (C2C)"

    def __init__(self, N, S):
        self.N = N # Total number of Groups
        self.S = S # Size of each group (Items per group)
        
        # Registers (Counters)
        self.G = 0 # Group Counter
        self.I = 0 # Item Counter (within current group)
        self.T = 0 # Total Counter

        self.state = 'q_init'
        self.history = []

    def _record_history(self, interpretation, highlight=False):
        record = {
            'State': self.state, 'Interpretation': interpretation,
            'G (Groups Done)': self.G, 'I (Item in Group)': self.I, 'T (Total)': self.T,
            'Highlight': highlight
        }
        self.history.append(record)

    def transition(self, next_state):
        self.state = next_state

    def run(self):
        while self.state not in ['q_accept', 'q_error']:
            executor = getattr(self, f"execute_{self.state}", self.execute_error)
            executor()
        return self.T

    def execute_error(self):
        self._record_history(f"Error: Entered unknown state {self.state}")
        self.transition('q_error')

    # --- State Execution Methods ---

    def execute_q_init(self):
        """Initialize all counters to zero."""
        self.G = 0; self.I = 0; self.T = 0
        self._record_history(f"Inputs: {self.N} groups of {self.S}. Initialize counters.", highlight=True)
        # Proceed to check status immediately (handles N=0 case as well)
        self.transition('q_check_G')

    def execute_q_check_G(self):
         """Outer loop check: Check if all groups are counted."""
         # Condition: More groups remain (G < N)
         if self.G < self.N:
              # We use G+1 for interpretation to align with 1-based counting (Group 1, 2...)
              self._record_history(f"G < N. Starting Group {self.G+1}.")
              self.transition('q_count_items')
         # Condition: All groups finished (G == N)
         else:
              self._record_history(f"G = N. All groups counted. Result = {self.T}.", highlight=True)
              self.transition('q_accept')

    def execute_q_count_items(self):
        """Inner loop: Count items within the current group."""
        # Condition: More items remain in the current group (I < S)
        if self.I < self.S:
            self.I += 1
            self.T += 1
            # Interpretation mirrors student vocalizing the total count and tracking context
            self._record_history(f"Count: {self.T}. (Item {self.I} in Group {self.G+1}).")
        # Condition: Current group is finished (I == S)
        else:
            self._record_history(f"Group {self.G+1} finished.", highlight=True)
            self.transition('q_next_group')

    def execute_q_next_group(self):
        """Outer loop increment: Move to the next group."""
        self.G += 1
        self.I = 0 # Reset item counter
        self._record_history(f"Increment G. Reset I.")
        self.transition('q_check_G')


    def display_history(self, summarized=False):
        print(f"\n--- {self.strategy_name} History ({self.N} x {self.S}) ---")
        df = pd.DataFrame(self.history)
        display_cols = ['State', 'Interpretation', 'G (Groups Done)', 'I (Item in Group)', 'T (Total)']

        if summarized:
             print("Summary Trace (Group Boundaries):")
             summary_df = df[df['Highlight'] == True]
             if not summary_df.empty:
                print(summary_df[display_cols].to_markdown(index=False))
        else:
            print("Full Iterative Trace:")
            print(df[display_cols].to_markdown(index=False))

# Test Case: Example from PDF (3 bags of 6 cookies)
N_test = 3
S_test = 6
c2c = C2C_MultiplicationAutomaton(N=N_test, S=S_test)
c2c.run()
c2c.display_history(summarized=False)