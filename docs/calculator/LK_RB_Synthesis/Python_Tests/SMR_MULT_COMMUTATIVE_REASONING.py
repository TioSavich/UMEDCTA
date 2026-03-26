import pandas as pd

class CommutativeReasoningMultiplication:
    """
    A Register Machine modeling the strategic use of Commutative Reasoning in multiplication.
    It analyzes the factors, rearranges them for optimization based on a cognitive heuristic,
    and then executes the calculation iteratively.
    """
    strategy_name = "Commutative Reasoning (Multiplication Optimization)"

    def __init__(self, A, B, Base=10):
        self.A_initial = A
        self.B_initial = B
        self.Base = Base
        
        # Working registers for factors
        self.A = A
        self.B = B

        # Calculation registers
        self.Groups = 0
        self.ItemsPerGroup = 0
        self.Total = 0
        self.Counter = 0

        self.state = 'q_start'
        self.history = []

    def _record_history(self, interpretation, highlight=False):
        record = {
            'State': self.state, 'Interpretation': interpretation,
            'Groups': self.Groups, 'Items/Grp': self.ItemsPerGroup, 'Total': self.Total,
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
    def heuristic(self, Groups, Items):
        """
        Estimates cognitive difficulty (H). Lower is better.
        Heuristic prioritizes easy Items (1, 10, 5) first, then minimizes the number of Groups (iterations).
        """
        difficulty = 0
        # Penalty for difficult Items (Multiplicand)
        is_easy_item = (Items == 1) or (Items == self.Base) or (self.Base % 2 == 0 and Items == self.Base / 2)
        
        if not is_easy_item:
            # Apply a large penalty if the item is difficult to count by
            difficulty += 100
        
        # Add penalty for the number of iterations (Groups)
        difficulty += Groups
        return difficulty

    # --- State Execution Methods ---

    def execute_q_start(self):
        self._record_history(f"Inputs: {self.A_initial} x {self.B_initial}.", highlight=True)
        self.transition('q_evaluate')

    def execute_q_evaluate(self):
        """Analyze factors and decide whether to swap based on optimization heuristic."""
        
        # Calculate difficulty for A*B (A groups of B items)
        H_AB = self.heuristic(self.A, self.B)
        # Calculate difficulty for B*A (B groups of A items)
        H_BA = self.heuristic(self.B, self.A)
        
        self._record_history(f"Evaluating: H({self.A}x{self.B})={H_AB} vs H({self.B}x{self.A})={H_BA}.")

        if H_BA < H_AB:
            # B*A is strictly easier
            self._record_history(f"Heuristic suggests commuting (B*A) is easier.", highlight=True)
            self.transition('q_repackage_swap')
        else:
            # A*B is easier or equal
            self._record_history(f"Heuristic suggests original (A*B) is optimal or equal.")
            self.transition('q_repackage_noswap')

    def execute_q_repackage_swap(self):
        """Swap A and B and assign roles."""
        self.A, self.B = self.B, self.A
        self.Groups = self.A
        self.ItemsPerGroup = self.B
        self._record_history(f"Repackaged as {self.Groups} x {self.ItemsPerGroup}.")
        self.transition('q_init_calc')
        
    def execute_q_repackage_noswap(self):
        """Keep A and B as is and assign roles."""
        self.Groups = self.A
        self.ItemsPerGroup = self.B
        self._record_history(f"Proceeding as {self.Groups} x {self.ItemsPerGroup}.")
        self.transition('q_init_calc')

    # Calculation Subroutine (Iterative Addition / Skip Counting)
    def execute_q_init_calc(self):
        self.Total = 0
        self.Counter = self.Groups
        self._record_history("Initializing iterative calculation.")
        self.transition('q_loop_calc')

    def execute_q_loop_calc(self):
        if self.Counter > 0:
            self.Total += self.ItemsPerGroup
            self.Counter -= 1
            self._record_history(f"Iterate: Added {self.ItemsPerGroup}. Total = {self.Total}.")
        else:
            self._record_history(f"Calculation complete. Result = {self.Total}.", highlight=True)
            self.transition('q_accept')

    def display_history(self, summarized=True):
        print(f"\n--- {self.strategy_name} History ({self.A_initial} x {self.B_initial}) ---")
        df = pd.DataFrame(self.history)
        display_cols = ['State', 'Interpretation', 'Groups', 'Items/Grp', 'Total']

        if summarized:
             print("Summary Trace:")
             summary_df = df[df['Highlight'] == True]
             if not summary_df.empty:
                print(summary_df[display_cols].to_markdown(index=False))
        else:
            print("Full Iterative Trace:")
            print(df[display_cols].to_markdown(index=False))

# Test Case 1: Example from PDF (10 x 7) 
# H(10x7) = 100 (for 7) + 10 = 110. H(7x10) = 0 (for 10) + 7 = 7. Should swap.
print("=== Test Case 1: 10 x 7 (Optimization favors 7x10) ===")
comm_10_7 = CommutativeReasoningMultiplication(A=10, B=7)
comm_10_7.run()
comm_10_7.display_history(summarized=False)

# Test Case 2: 8 x 3
# H(8x3) = 100 (for 3) + 8 = 108. H(3x8) = 100 (for 8) + 3 = 103. Should swap.
print("\n=== Test Case 2: 8 x 3 (Optimization favors 3x8) ===")
comm_8_3 = CommutativeReasoningMultiplication(A=8, B=3)
comm_8_3.run()
comm_8_3.display_history(summarized=True)