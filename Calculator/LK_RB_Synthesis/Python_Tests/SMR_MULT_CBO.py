import pandas as pd
import numpy as np

class CBO_MultiplicationAutomaton:
    """
    A Register Machine modeling the 'Conversion to Bases and Ones' (CBO) strategy.
    Models the cognitive process of redistributing units from one group to others 
    to form complete base units, using an array to represent working memory.
    """
    strategy_name = "Conversion to Bases and Ones (CBO - Redistribution)"

    def __init__(self, N, S, Base=10):
        self.N = N # Total number of Groups
        self.S = S # Initial size of each group
        self.Base = Base
        
        # Registers
        # Using a numpy array to represent the size of each group in working memory
        self.Groups = np.zeros(N, dtype=int) if N > 0 else np.array([], dtype=int)
        self.SourceIdx = 0
        self.TargetIdx = 0

        self.state = 'q_init'
        self.history = []

    def _record_history(self, interpretation, highlight=False):
        record = {
            'State': self.state, 'Interpretation': interpretation,
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
        return self.calculate_total()

    def execute_error(self):
        self._record_history(f"Error: Entered unknown state {self.state}")
        self.transition('q_error')

    # --- State Execution Methods ---

    def execute_q_init(self):
        """Initialize the groups in working memory."""
        if self.N > 0:
            self.Groups.fill(self.S)
        self._record_history(f"Initialize {self.N} groups of {self.S}.", highlight=True)
        self.transition('q_select_source')

    def execute_q_select_source(self):
        """Select a group to break apart for redistribution."""
        if self.N == 0:
            self.transition('q_finalize'); return
            
        # Heuristic: Select the last group as the source (as implied in George's example)
        self.SourceIdx = self.N - 1
        self._record_history(f"Selected Group {self.SourceIdx+1} as the source for redistribution.")
        self.transition('q_init_transfer')

    def execute_q_init_transfer(self):
        """Initialize the target index for redistribution."""
        self.TargetIdx = 0
        self._record_history("Starting redistribution loop.")
        self.transition('q_loop_transfer')

    def execute_q_loop_transfer(self):
        """Iteratively transfer units from Source to Targets until targets are full or source is empty."""
        
        # Exit Conditions
        if self.Groups[self.SourceIdx] == 0:
            self._record_history("Source group is empty. Redistribution complete.", highlight=True)
            self.transition('q_finalize')
            return
        if self.TargetIdx >= self.N:
            self._record_history("All groups checked. Redistribution complete.", highlight=True)
            self.transition('q_finalize')
            return

        # Transfer Logic
        if self.TargetIdx != self.SourceIdx:
            if self.Groups[self.TargetIdx] < self.Base:
                # Transfer one unit
                self.Groups[self.SourceIdx] -= 1
                self.Groups[self.TargetIdx] += 1
                
                interpretation = f"Transferred 1 unit from Group {self.SourceIdx+1} to Group {self.TargetIdx+1}."
                
                # Check if the target is now full
                if self.Groups[self.TargetIdx] == self.Base:
                    interpretation += " (Target reached Base)."
                    # Move to the next target immediately if full
                    self.TargetIdx += 1
                
                self._record_history(interpretation)

            else:
                # Target is already full, skip it
                self.TargetIdx += 1
        else:
            # Skip the source index
            self.TargetIdx += 1
            
        # Stay in q_loop_transfer

    def calculate_total(self):
        """Calculate the final total by recognizing the bases and ones."""
        if self.N == 0: return 0
        Bases = np.sum(self.Groups // self.Base)
        Ones = np.sum(self.Groups % self.Base)
        Total = Bases * self.Base + Ones
        return Total
        
    def execute_q_finalize(self):
        """Tally the results."""
        Total = self.calculate_total()
        Bases = np.sum(self.Groups // self.Base)
        Ones = np.sum(self.Groups % self.Base)
        self._record_history(f"Final Tally: {Bases} Bases + {Ones} Ones = {Total}.", highlight=True)
        self.transition('q_accept')


    def display_history(self, summarized=False):
        print(f"\n--- {self.strategy_name} History ({self.N} x {self.S}) ---")
        df = pd.DataFrame(self.history)
        display_cols = ['State', 'Interpretation', 'Group State']

        if summarized:
             print("Summary Trace:")
             summary_df = df[df['Highlight'] == True]
             if not summary_df.empty:
                print(summary_df[display_cols].to_markdown(index=False))
        else:
            print("Full Iterative Trace:")
            print(df[display_cols].to_markdown(index=False))

# Test Case: Example from PDF (7 cans of 9 oz)
N_test = 7
S_test = 9
cbo = CBO_MultiplicationAutomaton(N=N_test, S=S_test)
cbo.run()
cbo.display_history(summarized=False)