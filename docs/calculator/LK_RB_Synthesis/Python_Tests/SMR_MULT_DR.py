import pandas as pd

class DistributiveReasoningAutomaton:
    """
    A Register Machine modeling the 'Distributive Reasoning' (DR) strategy.
    Models the heuristic splitting of one factor and the iterative calculation of partial products.
    """
    strategy_name = "Distributive Reasoning (DR)"

    def __init__(self, N, S, Base=10):
        self.N = N # Number of Groups
        self.S = S # Size of groups
        self.Base = Base
        
        # Registers
        self.S1 = 0; self.S2 = 0 # Split parts
        self.P1 = 0; self.P2 = 0 # Partial Products
        self.Total = 0
        self.Counter = 0

        self.state = 'q_init'
        self.history = []

    def _record_history(self, interpretation, highlight=False):
        record = {
            'State': self.state, 'Interpretation': interpretation,
            'S1': self.S1, 'S2': self.S2, 'P1': self.P1, 'P2': self.P2, 'Total': self.Total,
            'Highlight': highlight
        }
        self.history.append(record)

    def transition(self, next_state):
        self.state = next_state

    def run(self):
        while self.state not in ['q_accept', 'q_error']:
            executor = getattr(self, f"execute_{self.state}", self.execute_error)
            executor()
        return self.Total

    def execute_error(self):
        self._record_history(f"Error: Entered unknown state {self.state}")
        self.transition('q_error')

    # --- Heuristic Function ---
    def heuristic_split(self, value):
        """
        Heuristic for splitting a factor (S). Finds the largest "easy" number within S.
        Easy numbers prioritized: Base (10), Half-Base (5), 2, 1.
        """
        # Define prioritized "easy" numbers based on the base system.
        easy_numbers = [1, 2]
        if self.Base % 2 == 0:
            easy_numbers.append(self.Base // 2) # e.g., 5
        easy_numbers.append(self.Base) # e.g., 10
            
        # Sort descending to prioritize larger easy numbers
        easy_numbers.sort(reverse=True)
        
        for easy_num in easy_numbers:
            # Find the largest easy number less than the value
            if value > easy_num:
                S1 = easy_num
                S2 = value - S1
                return S1, S2
        
        # If the value itself is easy or no split is useful
        return value, 0

    # --- State Execution Methods ---

    def execute_q_init(self):
        self._record_history(f"Inputs: {self.N} x {self.S}.", highlight=True)
        self.transition('q_split')

    def execute_q_split(self):
        """Apply heuristic to split S."""
        self.S1, self.S2 = self.heuristic_split(self.S)
        
        if self.S2 > 0:
            self._record_history(f"Split S ({self.S}) into {self.S1} + {self.S2}.", highlight=True)
        else:
            self._record_history(f"S ({self.S}) is easy. No split needed.")
            
        self.transition('q_init_P1')


    # Calculation Subroutine for P1 (N * S1)
    def execute_q_init_P1(self):
        self.P1 = 0
        self.Counter = self.N
        self._record_history(f"Initializing calculation of P1 ({self.N} x {self.S1}).")
        self.transition('q_loop_P1')

    def execute_q_loop_P1(self):
        if self.Counter > 0:
            # Iterative Skip Counting
            self.P1 += self.S1
            self.Counter -= 1
            self._record_history(f"Iterate P1: Added {self.S1}. P1 = {self.P1}.")
        else:
            self._record_history(f"P1 complete. P1 = {self.P1}.", highlight=True)
            # Check if the second part needs calculation
            if self.S2 > 0:
                 self.transition('q_init_P2')
            else:
                 self.transition('q_sum')

    # Calculation Subroutine for P2 (N * S2)
    def execute_q_init_P2(self):
        self.P2 = 0
        self.Counter = self.N
        self._record_history(f"Initializing calculation of P2 ({self.N} x {self.S2}).")
        self.transition('q_loop_P2')

    def execute_q_loop_P2(self):
        if self.Counter > 0:
            # Iterative Skip Counting
            self.P2 += self.S2
            self.Counter -= 1
            self._record_history(f"Iterate P2: Added {self.S2}. P2 = {self.P2}.")
        else:
            self._record_history(f"P2 complete. P2 = {self.P2}.", highlight=True)
            self.transition('q_sum')

    def execute_q_sum(self):
        """Sum the partial products."""
        self.Total = self.P1 + self.P2
        self._record_history(f"Summing partials: {self.P1} + {self.P2} = {self.Total}.", highlight=True)
        self.transition('q_accept')

    def display_history(self, summarized=False):
        print(f"\n--- {self.strategy_name} History ({self.N} x {self.S}) ---")
        df = pd.DataFrame(self.history)
        display_cols = ['State', 'Interpretation', 'S1', 'S2', 'P1', 'P2', 'Total']

        if summarized:
             print("Summary Trace:")
             summary_df = df[df['Highlight'] == True]
             if not summary_df.empty:
                print(summary_df[display_cols].to_markdown(index=False))
        else:
            print("Full Iterative Trace:")
            print(df[display_cols].to_markdown(index=False))

# Test Case: Sarah's example (5 boxes of 7 turtles)
# Heuristic should split 7 into 5 + 2.
N_test = 5
S_test = 7
dr = DistributiveReasoningAutomaton(N=N_test, S=S_test)
dr.run()
dr.display_history(summarized=False)